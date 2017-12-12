{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.JHC
-- Copyright   :  Isaac Jones 2003-2006
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the JHC-specific code for configuring, building
-- and installing packages.

module Distribution.Simple.JHC (
        configure, getInstalledPackages,
        buildLib, buildExe,
        installLib, installExe
 ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.InstalledPackageInfo
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Language.Haskell.Extension
import Distribution.Simple.Program
import Distribution.Types.MungedPackageId (mungedName)
import Distribution.Types.PackageId
import Distribution.Types.UnitId
import Distribution.Version
import Distribution.Package
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Monad
import Distribution.Text

import System.FilePath          ( (</>) )
import Distribution.Compat.ReadP
    ( readP_to_S, string, skipSpaces )
import Distribution.System ( Platform )

import qualified Data.Map as Map  ( empty )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8


-- -----------------------------------------------------------------------------
-- Configuring

configure :: Maybe FilePath -> Maybe FilePath
          -> ProgramDb -> CabalM (Compiler, Maybe Platform, ProgramDb)
configure hcPath _hcPkgPath progdb = do

  (jhcProg, _, progdb') <- requireProgramVersion
                           jhcProgram (orLaterVersion (mkVersion [0,7,2]))
                           (userMaybeSpecifyPath "jhc" hcPath progdb)

  let Just version = programVersion jhcProg
      comp = Compiler {
        compilerId             = CompilerId JHC version,
        compilerAbiTag         = NoAbiTag,
        compilerCompat         = [],
        compilerLanguages      = jhcLanguages,
        compilerExtensions     = jhcLanguageExtensions,
        compilerProperties     = Map.empty
      }
      compPlatform = Nothing
  return (comp, compPlatform, progdb')

jhcLanguages :: [(Language, Flag)]
jhcLanguages = [(Haskell98, "")]

-- | The flags for the supported extensions
jhcLanguageExtensions :: [(Extension, Maybe Flag)]
jhcLanguageExtensions =
    [(EnableExtension  TypeSynonymInstances       , Nothing)
    ,(DisableExtension TypeSynonymInstances       , Nothing)
    ,(EnableExtension  ForeignFunctionInterface   , Nothing)
    ,(DisableExtension ForeignFunctionInterface   , Nothing)
    ,(EnableExtension  ImplicitPrelude            , Nothing) -- Wrong
    ,(DisableExtension ImplicitPrelude            , Just "--noprelude")
    ,(EnableExtension  CPP                        , Just "-fcpp")
    ,(DisableExtension CPP                        , Just "-fno-cpp")
    ]

getInstalledPackages :: PackageDBStack -> ProgramDb
                    -> CabalM InstalledPackageIndex
getInstalledPackages _packageDBs progdb = do
   -- jhc --list-libraries lists all available libraries.
   -- How shall I find out, whether they are global or local
   -- without checking all files and locations?
   str <- getDbProgramOutput jhcProgram progdb ["--list-libraries"]
   let pCheck :: [(a, String)] -> [a]
       pCheck rs = [ r | (r,s) <- rs, all isSpace s ]
   let parseLine ln =
          pCheck (readP_to_S
             (skipSpaces >> string "Name:" >> skipSpaces >> parse) ln)
   return $
      PackageIndex.fromList $
      map (\p -> emptyInstalledPackageInfo {
                    InstalledPackageInfo.installedUnitId = mkLegacyUnitId p,
                    InstalledPackageInfo.sourcePackageId = p
                 }) $
      concatMap parseLine $
      lines str

-- -----------------------------------------------------------------------------
-- Building

-- | Building a package for JHC.
-- Currently C source files are not supported.
buildLib :: PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> CabalM ()
buildLib pkg_descr lbi lib clbi = do
  verbosity <- askVerbosity
  let Just jhcProg = lookupProgram jhcProgram (withPrograms lbi)
  let libBi = libBuildInfo lib
  let args  = constructJHCCmdLine lbi libBi clbi (buildDir lbi) verbosity
  let pkgid = display (packageId pkg_descr)
      pfile = buildDir lbi </> "jhc-pkg.conf"
      hlfile= buildDir lbi </> (pkgid ++ ".hl")
  liftIO $ writeFileAtomic pfile . BS.Char8.pack $ jhcPkgConf pkg_descr
  runProgram jhcProg $
     ["--build-hl="++pfile, "-o", hlfile] ++
     args ++ map display (allLibModules lib clbi)

-- | Building an executable for JHC.
-- Currently C source files are not supported.
buildExe :: PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> CabalM ()
buildExe _pkg_descr lbi exe clbi = do
  verbosity <- askVerbosity
  let Just jhcProg = lookupProgram jhcProgram (withPrograms lbi)
  let exeBi = buildInfo exe
  let out   = buildDir lbi </> display (exeName exe)
  let args  = constructJHCCmdLine lbi exeBi clbi (buildDir lbi) verbosity
  runProgram jhcProg (["-o",out] ++ args ++ [modulePath exe])

constructJHCCmdLine :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                    -> FilePath -> Verbosity -> [String]
constructJHCCmdLine lbi bi clbi _odir verbosity =
        (if verbosity >= deafening then ["-v"] else [])
     ++ hcOptions JHC bi
     ++ languageToFlags (compiler lbi) (defaultLanguage bi)
     ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
     ++ ["--noauto","-i-"]
     ++ concat [["-i", l] | l <- nub (hsSourceDirs bi)]
     ++ ["-i", autogenComponentModulesDir lbi clbi]
     ++ ["-i", autogenPackageModulesDir lbi]
     ++ ["-optc" ++ opt | opt <- PD.ccOptions bi]
     -- It would be better if JHC would accept package names with versions,
     -- but JHC-0.7.2 doesn't accept this.
     -- Thus, we have to strip the version with 'pkgName'.
     ++ (concat [ ["-p", display (mungedName pkgid)]
                | (_, pkgid) <- componentPackageDeps clbi ])

jhcPkgConf :: PackageDescription -> String
jhcPkgConf pd =
  let sline name sel = name ++ ": "++sel pd
      lib pd' = case library pd' of
                Just lib' -> lib'
                Nothing -> error "no library available"
      comma = intercalate "," . map display
  in unlines [sline "name" (display . pkgName . packageId)
             ,sline "version" (display . pkgVersion . packageId)
             ,sline "exposed-modules" (comma . PD.exposedModules . lib)
             ,sline "hidden-modules" (comma . otherModules . libBuildInfo . lib)
             ]

installLib :: LocalBuildInfo
           -> FilePath
           -> FilePath
           -> FilePath
           -> PackageDescription
           -> Library
           -> ComponentLocalBuildInfo
           -> CabalM ()
installLib _lbi dest _dyn_dest build_dir pkg_descr _lib _clbi = do
    let p = display (packageId pkg_descr)++".hl"
    createDirectoryIfMissingVerbose True dest
    installOrdinaryFile (build_dir </> p) (dest </> p)

installExe :: FilePath -> FilePath -> (FilePath,FilePath) -> PackageDescription -> Executable -> CabalM ()
installExe dest build_dir (progprefix,progsuffix) _ exe = do
    let exe_name = display $ exeName exe
        src = exe_name </> exeExtension
        out   = (progprefix ++ exe_name ++ progsuffix) </> exeExtension
    createDirectoryIfMissingVerbose True dest
    installExecutableFile (build_dir </> src) (dest </> out)
