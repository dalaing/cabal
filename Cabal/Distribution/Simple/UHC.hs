{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.UHC
-- Copyright   :  Andres Loeh 2009
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the UHC-specific code for configuring, building
-- and installing packages.
--
-- Thanks to the authors of the other implementation-specific files, in
-- particular to Isaac Jones, Duncan Coutts and Henning Thielemann, for
-- inspiration on how to design this module.

module Distribution.Simple.UHC (
    configure, getInstalledPackages,
    buildLib, buildExe, installLib, registerPackage, inplacePackageDbPath
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Compat.ReadP
import Distribution.InstalledPackageInfo
import Distribution.Package hiding (installedUnitId)
import Distribution.PackageDescription
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler as C
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Types.MungedPackageId
import Distribution.Verbosity
import Distribution.Monad
import Distribution.Version
import Distribution.System
import Language.Haskell.Extension

import qualified Data.Map as Map ( empty )
import System.Directory
import System.FilePath

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Maybe FilePath -> Maybe FilePath
          -> ProgramDb -> CabalM (Compiler, Maybe Platform, ProgramDb)
configure hcPath _hcPkgPath progdb = do

  (_uhcProg, uhcVersion, progdb') <-
    requireProgramVersion uhcProgram
    (orLaterVersion (mkVersion [1,0,2]))
    (userMaybeSpecifyPath "uhc" hcPath progdb)

  let comp = Compiler {
               compilerId         =  CompilerId UHC uhcVersion,
               compilerAbiTag     =  C.NoAbiTag,
               compilerCompat     =  [],
               compilerLanguages  =  uhcLanguages,
               compilerExtensions =  uhcLanguageExtensions,
               compilerProperties =  Map.empty
             }
      compPlatform = Nothing
  return (comp, compPlatform, progdb')

uhcLanguages :: [(Language, C.Flag)]
uhcLanguages = [(Haskell98, "")]

-- | The flags for the supported extensions.
uhcLanguageExtensions :: [(Extension, Maybe C.Flag)]
uhcLanguageExtensions =
    let doFlag (f, (enable, disable)) = [(EnableExtension  f, enable),
                                         (DisableExtension f, disable)]
        alwaysOn = (Nothing, Nothing{- wrong -})
    in concatMap doFlag
    [(CPP,                          (Just "--cpp", Nothing{- wrong -})),
     (PolymorphicComponents,        alwaysOn),
     (ExistentialQuantification,    alwaysOn),
     (ForeignFunctionInterface,     alwaysOn),
     (UndecidableInstances,         alwaysOn),
     (MultiParamTypeClasses,        alwaysOn),
     (Rank2Types,                   alwaysOn),
     (PatternSignatures,            alwaysOn),
     (EmptyDataDecls,               alwaysOn),
     (ImplicitPrelude,              (Nothing, Just "--no-prelude"{- wrong -})),
     (TypeOperators,                alwaysOn),
     (OverlappingInstances,         alwaysOn),
     (FlexibleInstances,            alwaysOn)]

getInstalledPackages :: Compiler -> PackageDBStack -> ProgramDb
                     -> CabalM InstalledPackageIndex
getInstalledPackages comp packagedbs progdb = do
  let compilerid = compilerId comp
  systemPkgDir <- getGlobalPackageDir progdb
  userPkgDir   <- liftIO getUserPackageDir
  let pkgDirs    = nub (concatMap (packageDbPaths userPkgDir systemPkgDir) packagedbs)
  -- putStrLn $ "pkgdirs: " ++ show pkgDirs
  pkgs <- liftIO . fmap (map addBuiltinVersions . concat) $
          traverse (\ d -> getDirectoryContents d >>= filterM (isPkgDir (display compilerid) d))
          pkgDirs
  -- putStrLn $ "pkgs: " ++ show pkgs
  let iPkgs =
        map mkInstalledPackageInfo $
        concatMap parsePackage $
        pkgs
  -- putStrLn $ "installed pkgs: " ++ show iPkgs
  return (fromList iPkgs)

getGlobalPackageDir :: ProgramDb -> CabalM FilePath
getGlobalPackageDir progdb = do
    output <- getDbProgramOutput
                uhcProgram progdb ["--meta-pkgdir-system"]
    -- call to "lines" necessary, because pkgdir contains an extra newline at the end
    let [pkgdir] = lines output
    return pkgdir

getUserPackageDir :: NoCallStackIO FilePath
getUserPackageDir = do
    homeDir <- getHomeDirectory
    return $ homeDir </> ".cabal" </> "lib"  -- TODO: determine in some other way

packageDbPaths :: FilePath -> FilePath -> PackageDB -> [FilePath]
packageDbPaths user system db =
  case db of
    GlobalPackageDB         ->  [ system ]
    UserPackageDB           ->  [ user ]
    SpecificPackageDB path  ->  [ path ]

-- | Hack to add version numbers to UHC-built-in packages. This should sooner or
-- later be fixed on the UHC side.
addBuiltinVersions :: String -> String
{-
addBuiltinVersions "uhcbase"  = "uhcbase-1.0"
addBuiltinVersions "base"  = "base-3.0"
addBuiltinVersions "array" = "array-0.2"
-}
addBuiltinVersions xs      = xs

-- | Name of the installed package config file.
installedPkgConfig :: String
installedPkgConfig = "installed-pkg-config"

-- | Check if a certain dir contains a valid package. Currently, we are
-- looking only for the presence of an installed package configuration.
-- TODO: Actually make use of the information provided in the file.
isPkgDir :: String -> String -> String -> NoCallStackIO Bool
isPkgDir _ _   ('.' : _)  = return False  -- ignore files starting with a .
isPkgDir c dir xs         = do
                              let candidate = dir </> uhcPackageDir xs c
                              -- putStrLn $ "trying: " ++ candidate
                              doesFileExist (candidate </> installedPkgConfig)

parsePackage :: String -> [PackageId]
parsePackage x = map fst (filter (\ (_,y) -> null y) (readP_to_S parse x))

-- | Create a trivial package info from a directory name.
mkInstalledPackageInfo :: PackageId -> InstalledPackageInfo
mkInstalledPackageInfo p = emptyInstalledPackageInfo
  { installedUnitId = mkLegacyUnitId p,
    sourcePackageId = p }


-- -----------------------------------------------------------------------------
-- Building

buildLib :: PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> CabalM ()
buildLib pkg_descr lbi lib clbi = do
  verbosity <- askVerbosity
  systemPkgDir <- getGlobalPackageDir (withPrograms lbi)
  userPkgDir   <- liftIO getUserPackageDir
  let runUhcProg = runDbProgram uhcProgram (withPrograms lbi)
  let uhcArgs =    -- set package name
                   ["--pkg-build=" ++ display (packageId pkg_descr)]
                   -- common flags lib/exe
                ++ constructUHCCmdLine userPkgDir systemPkgDir
                                       lbi (libBuildInfo lib) clbi
                                       (buildDir lbi) verbosity
                   -- source files
                   -- suboptimal: UHC does not understand module names, so
                   -- we replace periods by path separators
                ++ map (map (\ c -> if c == '.' then pathSeparator else c))
                       (map display (allLibModules lib clbi))

  runUhcProg uhcArgs

  return ()

buildExe :: PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> CabalM ()
buildExe _pkg_descr lbi exe clbi = do
  verbosity <- askVerbosity
  systemPkgDir <- getGlobalPackageDir (withPrograms lbi)
  userPkgDir   <- liftIO getUserPackageDir
  let runUhcProg = runDbProgram uhcProgram (withPrograms lbi)
  let uhcArgs =    -- common flags lib/exe
                   constructUHCCmdLine userPkgDir systemPkgDir
                                       lbi (buildInfo exe) clbi
                                       (buildDir lbi) verbosity
                   -- output file
                ++ ["--output", buildDir lbi </> display (exeName exe)]
                   -- main source module
                ++ [modulePath exe]
  runUhcProg uhcArgs

constructUHCCmdLine :: FilePath -> FilePath
                    -> LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                    -> FilePath -> Verbosity -> [String]
constructUHCCmdLine user system lbi bi clbi odir verbosity =
     -- verbosity
     (if      verbosity >= deafening then ["-v4"]
      else if verbosity >= normal    then []
      else                                ["-v0"])
  ++ hcOptions UHC bi
     -- flags for language extensions
  ++ languageToFlags   (compiler lbi) (defaultLanguage bi)
  ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
     -- packages
  ++ ["--hide-all-packages"]
  ++ uhcPackageDbOptions user system (withPackageDB lbi)
  ++ ["--package=uhcbase"]
  ++ ["--package=" ++ display (mungedName pkgid) | (_, pkgid) <- componentPackageDeps clbi ]
     -- search paths
  ++ ["-i" ++ odir]
  ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
  ++ ["-i" ++ autogenComponentModulesDir lbi clbi]
  ++ ["-i" ++ autogenPackageModulesDir lbi]
     -- cpp options
  ++ ["--optP=" ++ opt | opt <- cppOptions bi]
     -- output path
  ++ ["--odir=" ++ odir]
     -- optimization
  ++ (case withOptimization lbi of
        NoOptimisation       ->  ["-O0"]
        NormalOptimisation   ->  ["-O1"]
        MaximumOptimisation  ->  ["-O2"])

uhcPackageDbOptions :: FilePath -> FilePath -> PackageDBStack -> [String]
uhcPackageDbOptions user system db = map (\ x -> "--pkg-searchpath=" ++ x)
                                         (concatMap (packageDbPaths user system) db)

-- -----------------------------------------------------------------------------
-- Installation

installLib :: LocalBuildInfo
           -> FilePath -> FilePath -> FilePath
           -> PackageDescription -> Library -> ComponentLocalBuildInfo -> CabalM ()
installLib _lbi targetDir _dynlibTargetDir builtDir pkg _library _clbi = do
    -- putStrLn $ "dest:  " ++ targetDir
    -- putStrLn $ "built: " ++ builtDir
    installDirectoryContents (builtDir </> display (packageId pkg)) targetDir

-- currently hard-coded UHC code generator and variant to use
uhcTarget, uhcTargetVariant :: String
uhcTarget        = "bc"
uhcTargetVariant = "plain"

-- root directory for a package in UHC
uhcPackageDir    :: String -> String -> FilePath
uhcPackageSubDir ::           String -> FilePath
uhcPackageDir    pkgid compilerid = pkgid </> uhcPackageSubDir compilerid
uhcPackageSubDir       compilerid = compilerid </> uhcTarget </> uhcTargetVariant

-- -----------------------------------------------------------------------------
-- Registering

registerPackage
  :: Compiler
  -> ProgramDb
  -> PackageDBStack
  -> InstalledPackageInfo
  -> CabalM ()
registerPackage comp progdb packageDbs installedPkgInfo = do
    dbdir <- case last packageDbs of
      GlobalPackageDB       -> getGlobalPackageDir progdb
      UserPackageDB         -> liftIO getUserPackageDir
      SpecificPackageDB dir -> return dir
    let pkgdir = dbdir </> uhcPackageDir (display pkgid) (display compilerid)
    createDirectoryIfMissingVerbose True pkgdir
    liftIO $ writeUTF8File (pkgdir </> installedPkgConfig)
                           (showInstalledPackageInfo installedPkgInfo)
  where
    pkgid      = sourcePackageId installedPkgInfo
    compilerid = compilerId comp

inplacePackageDbPath :: LocalBuildInfo -> FilePath
inplacePackageDbPath lbi = buildDir lbi
