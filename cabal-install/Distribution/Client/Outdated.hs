{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Outdated
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'outdated' command. Checks for outdated
-- dependencies in the package description file or freeze file.
-----------------------------------------------------------------------------

module Distribution.Client.Outdated ( outdated
                                    , ListOutdatedSettings(..), listOutdated )
where

import Prelude ()
import Distribution.Client.Config
import Distribution.Client.IndexUtils as IndexUtils
import Distribution.Client.Compat.Prelude
import Distribution.Client.ProjectConfig
import Distribution.Client.DistDirLayout
import Distribution.Client.RebuildMonad
import Distribution.Client.Setup hiding (quiet)
import Distribution.Client.Targets
import Distribution.Client.Types
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.PackageIndex
import Distribution.Client.Sandbox.PackageEnvironment

import Distribution.Package                          (PackageName, packageVersion)
import Distribution.PackageDescription               (allBuildDepends)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.Simple.Compiler                  (Compiler, compilerInfo)
import Distribution.Simple.Setup                     (fromFlagOrDefault)
import Distribution.Simple.Utils
       (die', notice, debug, tryFindPackageDesc)
import Distribution.System                           (Platform)
import Distribution.Text                             (display)
import Distribution.Types.ComponentRequestedSpec
       (ComponentRequestedSpec(..))
import Distribution.Types.Dependency
       (Dependency(..), depPkgName, simplifyDependency)
import Distribution.Verbosity                        (silent)
import Distribution.Monad                            (CabalM, liftIO, localVerbosity)
import Distribution.Version
       (Version, LowerBound(..), UpperBound(..)
       ,asVersionIntervals, majorBoundVersion)
import Distribution.PackageDescription.Parsec
       (readGenericPackageDescription)

import qualified Data.Set as S
import System.Directory                              (getCurrentDirectory)
import System.Exit                                   (exitFailure)
import Control.Exception                             (throwIO)

-- | Entry point for the 'outdated' command.
outdated :: OutdatedFlags -> RepoContext
         -> Compiler -> Platform
         -> CabalM ()
outdated outdatedFlags repoContext comp platform = do
  let freezeFile    = fromFlagOrDefault False (outdatedFreezeFile outdatedFlags)
      newFreezeFile = fromFlagOrDefault False
                      (outdatedNewFreezeFile outdatedFlags)
      simpleOutput  = fromFlagOrDefault False
                      (outdatedSimpleOutput outdatedFlags)
      quiet         = fromFlagOrDefault False (outdatedQuiet outdatedFlags)
      exitCode      = fromFlagOrDefault quiet (outdatedExitCode outdatedFlags)
      ignorePred    = let ignoreSet = S.fromList (outdatedIgnore outdatedFlags)
                      in \pkgname -> pkgname `S.member` ignoreSet
      minorPred     = case outdatedMinor outdatedFlags of
                        Nothing -> const False
                        Just IgnoreMajorVersionBumpsNone -> const False
                        Just IgnoreMajorVersionBumpsAll  -> const True
                        Just (IgnoreMajorVersionBumpsSome pkgs) ->
                          let minorSet = S.fromList pkgs
                          in \pkgname -> pkgname `S.member` minorSet
      quietVerbosity v = if quiet then silent else v

  localVerbosity quietVerbosity $ do
    sourcePkgDb <- IndexUtils.getSourcePackages repoContext
    let pkgIndex = packageIndex sourcePkgDb
    deps <- if freezeFile
            then depsFromFreezeFile
            else if newFreezeFile
                then depsFromNewFreezeFile
                else depsFromPkgDesc       comp platform
    debug $ "Dependencies loaded: "
      ++ (intercalate ", " $ map display deps)
    let outdatedDeps = listOutdated deps pkgIndex
                      (ListOutdatedSettings ignorePred minorPred)
    when (not quiet) $
      showResult outdatedDeps simpleOutput
    if (exitCode && (not . null $ outdatedDeps))
      then liftIO $ exitFailure
      else return ()

-- | Print either the list of all outdated dependencies, or a message
-- that there are none.
showResult :: [(Dependency,Version)] -> Bool -> CabalM ()
showResult outdatedDeps simpleOutput =
  if (not . null $ outdatedDeps)
    then
    do when (not simpleOutput) $
         notice "Outdated dependencies:"
       for_ outdatedDeps $ \(d@(Dependency pn _), v) ->
         let outdatedDep = if simpleOutput then display pn
                           else display d ++ " (latest: " ++ display v ++ ")"
         in notice outdatedDep
    else notice "All dependencies are up to date."

-- | Convert a list of 'UserConstraint's to a 'Dependency' list.
userConstraintsToDependencies :: [UserConstraint] -> [Dependency]
userConstraintsToDependencies ucnstrs =
  mapMaybe (packageConstraintToDependency . userToPackageConstraint) ucnstrs

-- | Read the list of dependencies from the freeze file.
depsFromFreezeFile :: CabalM [Dependency]
depsFromFreezeFile = do
  cwd        <- liftIO getCurrentDirectory
  userConfig <- loadUserConfig cwd Nothing
  let ucnstrs = map fst . configExConstraints . savedConfigureExFlags $
                userConfig
      deps    = userConstraintsToDependencies ucnstrs
  debug "Reading the list of dependencies from the freeze file"
  return deps

-- | Read the list of dependencies from the new-style freeze file.
depsFromNewFreezeFile :: CabalM [Dependency]
depsFromNewFreezeFile = do
  projectRoot <- liftIO $ either (throwIO) return =<<
                            findProjectRoot Nothing
                 {- TODO: Support '--project-file': -} Nothing
  let distDirLayout = defaultDistDirLayout projectRoot
                      {- TODO: Support dist dir override -} Nothing
  projectConfig  <- runRebuild (distProjectRootDirectory distDirLayout) $
                    readProjectLocalFreezeConfig distDirLayout
  let ucnstrs = map fst . projectConfigConstraints . projectConfigShared
                $ projectConfig
      deps    = userConstraintsToDependencies ucnstrs
  debug
    "Reading the list of dependencies from the new-style freeze file"
  return deps

-- | Read the list of dependencies from the package description.
depsFromPkgDesc :: Compiler  -> Platform -> CabalM [Dependency]
depsFromPkgDesc comp platform = do
  cwd  <- liftIO getCurrentDirectory
  path <- liftIO $ tryFindPackageDesc cwd
  gpd  <- readGenericPackageDescription path
  let cinfo = compilerInfo comp
      epd = finalizePD mempty (ComponentRequestedSpec True True)
            (const True) platform cinfo [] gpd
  case epd of
    Left _        -> die' "finalizePD failed"
    Right (pd, _) -> do
      let bd = allBuildDepends pd
      debug
        "Reading the list of dependencies from the package description"
      return bd

-- | Various knobs for customising the behaviour of 'listOutdated'.
data ListOutdatedSettings = ListOutdatedSettings {
  -- | Should this package be ignored?
  listOutdatedIgnorePred :: PackageName -> Bool,
  -- | Should major version bumps should be ignored for this package?
  listOutdatedMinorPred  :: PackageName -> Bool
  }

-- | Find all outdated dependencies.
listOutdated :: [Dependency]
             -> PackageIndex UnresolvedSourcePackage
             -> ListOutdatedSettings
             -> [(Dependency, Version)]
listOutdated deps pkgIndex (ListOutdatedSettings ignorePred minorPred) =
  mapMaybe isOutdated $ map simplifyDependency deps
  where
    isOutdated :: Dependency -> Maybe (Dependency, Version)
    isOutdated dep
      | ignorePred (depPkgName dep) = Nothing
      | otherwise                   =
          let this   = map packageVersion $ lookupDependency pkgIndex dep
              latest = lookupLatest dep
          in (\v -> (dep, v)) `fmap` isOutdated' this latest

    isOutdated' :: [Version] -> [Version] -> Maybe Version
    isOutdated' [] _  = Nothing
    isOutdated' _  [] = Nothing
    isOutdated' this latest =
      let this'   = maximum this
          latest' = maximum latest
      in if this' < latest' then Just latest' else Nothing

    lookupLatest :: Dependency -> [Version]
    lookupLatest dep
      | minorPred (depPkgName dep) =
        map packageVersion $ lookupDependency pkgIndex  (relaxMinor dep)
      | otherwise                  =
        map packageVersion $ lookupPackageName pkgIndex (depPkgName dep)

    relaxMinor :: Dependency -> Dependency
    relaxMinor (Dependency pn vr) = (Dependency pn vr')
      where
        vr' = let vis = asVersionIntervals vr
                  (LowerBound v0 _,upper) = last vis
              in case upper of
                   NoUpperBound     -> vr
                   UpperBound _v1 _ -> majorBoundVersion v0
