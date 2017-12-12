{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008, 2012
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point to actually building the modules in a package. It
-- doesn't actually do much itself, most of the work is delegated to
-- compiler-specific actions. It does do some non-compiler specific bits like
-- running pre-processors.
--

module Distribution.Simple.Build (
    build, repl,
    startInterpreter,

    initialBuildSteps,
    componentInitialBuildSteps,
    writeAutogenFiles,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Dependency
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.ForeignLib
import Distribution.Types.MungedPackageId
import Distribution.Types.MungedPackageName
import Distribution.Types.UnqualComponentName
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ExecutableScope

import Distribution.Package
import Distribution.Backpack
import Distribution.Backpack.DescribeUnitId
import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.JHC   as JHC
import qualified Distribution.Simple.LHC   as LHC
import qualified Distribution.Simple.UHC   as UHC
import qualified Distribution.Simple.HaskellSuite as HaskellSuite
import qualified Distribution.Simple.PackageIndex as Index

import qualified Distribution.Simple.Build.Macros      as Build.Macros
import qualified Distribution.Simple.Build.PathsModule as Build.PathsModule
import qualified Distribution.Simple.Program.HcPkg as HcPkg

import Distribution.Simple.Compiler hiding (Flag)
import Distribution.PackageDescription hiding (Flag)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.ModuleName as ModuleName

import Distribution.Simple.Setup
import Distribution.Simple.BuildTarget
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db
import Distribution.Simple.BuildPaths
import Distribution.Simple.Configure
import Distribution.Simple.Register
import Distribution.Simple.Test.LibV09
import Distribution.Simple.Utils

import Distribution.System
import Distribution.Text
import Distribution.Monad

import Distribution.Compat.Graph (IsNode(..))

import Control.Monad
import qualified Data.Set as Set
import System.FilePath ( (</>), (<.>), takeDirectory )
import System.Directory ( getCurrentDirectory )

-- -----------------------------------------------------------------------------
-- |Build the libraries and executables in this package.

build    :: PackageDescription  -- ^ Mostly information from the .cabal file
         -> LocalBuildInfo      -- ^ Configuration information
         -> BuildFlags          -- ^ Flags that the user passed to build
         -> [ PPSuffixHandler ] -- ^ preprocessors to run before compiling
         -> IO ()
build pkg_descr lbi flags suffixes = flip runCabalM verbosity $ do
  targets <- readTargetInfos pkg_descr lbi (buildArgs flags)
  let componentsToBuild = neededTargetsInBuildOrder' pkg_descr lbi (map nodeKey targets)
  info $ "Component build order: "
                ++ intercalate ", "
                    (map (showComponentName . componentLocalName . targetCLBI)
                        componentsToBuild)

  when (null targets) $
    -- Only bother with this message if we're building the whole package
    setupMessage "Building" (packageId pkg_descr)

  internalPackageDB <- createInternalPackageDB lbi distPref

  (\f -> foldM_ f (installedPkgs lbi) componentsToBuild) $ \index target -> do
    let comp = targetComponent target
        clbi = targetCLBI target
    componentInitialBuildSteps distPref pkg_descr lbi clbi
    let bi     = componentBuildInfo comp
        progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
        lbi'   = lbi {
                   withPrograms  = progs',
                   withPackageDB = withPackageDB lbi ++ [internalPackageDB],
                   installedPkgs = index
                 }
    mb_ipi <- buildComponent (buildNumJobs flags) pkg_descr
                   lbi' suffixes comp clbi distPref
    return (maybe index (Index.insert `flip` index) mb_ipi)
  return ()
 where
  distPref  = fromFlag (buildDistPref flags)
  verbosity = fromFlag (buildVerbosity flags)


repl     :: PackageDescription  -- ^ Mostly information from the .cabal file
         -> LocalBuildInfo      -- ^ Configuration information
         -> ReplFlags           -- ^ Flags that the user passed to build
         -> [ PPSuffixHandler ] -- ^ preprocessors to run before compiling
         -> [String]
         -> IO ()
repl pkg_descr lbi flags suffixes args = flip runCabalM verbosity $ do

  target <- readTargetInfos pkg_descr lbi args >>= \r -> case r of
    -- This seems DEEPLY questionable.
    []       -> return (head (allTargetsInBuildOrder' pkg_descr lbi))
    [target] -> return target
    _        -> die' $ "The 'repl' command does not support multiple targets at once."
  let componentsToBuild = neededTargetsInBuildOrder' pkg_descr lbi [nodeKey target]
  debug $ "Component build order: "
                 ++ intercalate ", "
                      (map (showComponentName . componentLocalName . targetCLBI)
                           componentsToBuild)

  internalPackageDB <- createInternalPackageDB lbi distPref

  let lbiForComponent comp lbi' =
        lbi' {
          withPackageDB = withPackageDB lbi ++ [internalPackageDB],
          withPrograms  = addInternalBuildTools pkg_descr lbi'
                            (componentBuildInfo comp) (withPrograms lbi')
        }

  -- build any dependent components
  sequence_
    [ do let clbi = targetCLBI subtarget
             comp = targetComponent subtarget
             lbi' = lbiForComponent comp lbi
         componentInitialBuildSteps distPref pkg_descr lbi clbi
         buildComponent NoFlag
                        pkg_descr lbi' suffixes comp clbi distPref
    | subtarget <- init componentsToBuild ]

  -- REPL for target components
  let clbi = targetCLBI target
      comp = targetComponent target
      lbi' = lbiForComponent comp lbi
  componentInitialBuildSteps distPref pkg_descr lbi clbi
  replComponent pkg_descr lbi' suffixes comp clbi distPref
  where
    distPref  = fromFlag (replDistPref flags)
    verbosity = fromFlag (replVerbosity flags)


-- | Start an interpreter without loading any package files.
startInterpreter :: ProgramDb -> Compiler -> Platform
                 -> PackageDBStack -> CabalM ()
startInterpreter programDb comp platform packageDBs =
  case compilerFlavor comp of
    GHC   -> GHC.startInterpreter   programDb comp platform packageDBs
    GHCJS -> GHCJS.startInterpreter programDb comp platform packageDBs
    _     -> die' "A REPL is not supported with this compiler."

buildComponent :: Flag (Maybe Int)
               -> PackageDescription
               -> LocalBuildInfo
               -> [PPSuffixHandler]
               -> Component
               -> ComponentLocalBuildInfo
               -> FilePath
               -> CabalM (Maybe InstalledPackageInfo)
buildComponent numJobs pkg_descr lbi suffixes
               comp@(CLib lib) clbi distPref = do
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    setupMessage' "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    let libbi = libBuildInfo lib
        lib' = lib { libBuildInfo = addExtraCxxSources (addExtraCSources libbi extras) extras }
    buildLib numJobs pkg_descr lbi lib' clbi

    let oneComponentRequested (OneComponentRequestedSpec _) = True
        oneComponentRequested _ = False
    -- Don't register inplace if we're only building a single component;
    -- it's not necessary because there won't be any subsequent builds
    -- that need to tag us
    if (not (oneComponentRequested (componentEnabledSpec lbi)))
      then do
        -- Register the library in-place, so exes can depend
        -- on internally defined libraries.
        pwd <- liftIO getCurrentDirectory
        let -- The in place registration uses the "-inplace" suffix, not an ABI hash
            installedPkgInfo = inplaceInstalledPackageInfo pwd distPref pkg_descr
                                    -- NB: Use a fake ABI hash to avoid
                                    -- needing to recompute it every build.
                                    (mkAbiHash "inplace") lib' lbi clbi

        debug $ "Registering inplace:\n" ++ (IPI.showInstalledPackageInfo installedPkgInfo)
        registerPackage (compiler lbi) (withPrograms lbi)
                        (withPackageDB lbi) installedPkgInfo
                        HcPkg.defaultRegisterOptions {
                          HcPkg.registerMultiInstance = True
                        }
        return (Just installedPkgInfo)
      else return Nothing

buildComponent numJobs pkg_descr lbi suffixes
               comp@(CFLib flib) clbi _distPref = do
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    setupMessage' "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    buildFLib numJobs pkg_descr lbi flib clbi
    return Nothing

buildComponent numJobs pkg_descr lbi suffixes
               comp@(CExe exe) clbi _ = do
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    setupMessage' "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe numJobs pkg_descr lbi exe' clbi
    return Nothing


buildComponent numJobs pkg_descr lbi suffixes
               comp@(CTest test@TestSuite { testInterface = TestSuiteExeV10{} })
               clbi _distPref = do
    let exe = testSuiteExeV10AsExe test
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    setupMessage' "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe numJobs pkg_descr lbi exe' clbi
    return Nothing


buildComponent numJobs pkg_descr lbi0 suffixes
               comp@(CTest
                 test@TestSuite { testInterface = TestSuiteLibV09{} })
               clbi -- This ComponentLocalBuildInfo corresponds to a detailed
                    -- test suite and not a real component. It should not
                    -- be used, except to construct the CLBIs for the
                    -- library and stub executable that will actually be
                    -- built.
               distPref = do
    pwd <- liftIO getCurrentDirectory
    let (pkg, lib, libClbi, lbi, ipi, exe, exeClbi) =
          testSuiteLibV09AsLibAndExe pkg_descr test clbi lbi0 distPref pwd
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    setupMessage' "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    buildLib numJobs pkg lbi lib libClbi
    -- NB: need to enable multiple instances here, because on 7.10+
    -- the package name is the same as the library, and we still
    -- want the registration to go through.
    registerPackage (compiler lbi) (withPrograms lbi)
                    (withPackageDB lbi) ipi
                    HcPkg.defaultRegisterOptions {
                      HcPkg.registerMultiInstance = True
                    }
    let ebi = buildInfo exe
        -- NB: The stub executable is linked against the test-library
        --     which already contains all `other-modules`, so we need
        --     to remove those from the stub-exe's build-info
        exe' = exe { buildInfo = (addExtraCSources ebi extras) { otherModules = [] } }
    buildExe numJobs pkg_descr lbi exe' exeClbi
    return Nothing -- Can't depend on test suite


buildComponent _ _ _ _
               (CTest TestSuite { testInterface = TestSuiteUnsupported tt })
               _ _ =
    die' $ "No support for building test suite type " ++ display tt


buildComponent numJobs pkg_descr lbi suffixes
               comp@(CBench bm@Benchmark { benchmarkInterface = BenchmarkExeV10 {} })
               clbi _ = do
    let (exe, exeClbi) = benchmarkExeV10asExe bm clbi
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    setupMessage' "Building" (packageId pkg_descr)
      (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe numJobs pkg_descr lbi exe' exeClbi
    return Nothing


buildComponent _ _ _ _
               (CBench Benchmark { benchmarkInterface = BenchmarkUnsupported tt })
               _ _ =
    die' $ "No support for building benchmark type " ++ display tt


-- | Add extra C sources generated by preprocessing to build
-- information.
addExtraCSources :: BuildInfo -> [FilePath] -> BuildInfo
addExtraCSources bi extras = bi { cSources = new }
  where new = Set.toList $ old `Set.union` exs
        old = Set.fromList $ cSources bi
        exs = Set.fromList extras


-- | Add extra C++ sources generated by preprocessing to build
-- information.
addExtraCxxSources :: BuildInfo -> [FilePath] -> BuildInfo
addExtraCxxSources bi extras = bi { cxxSources = new }
  where new = Set.toList $ old `Set.union` exs
        old = Set.fromList $ cxxSources bi
        exs = Set.fromList extras


replComponent :: PackageDescription
              -> LocalBuildInfo
              -> [PPSuffixHandler]
              -> Component
              -> ComponentLocalBuildInfo
              -> FilePath
              -> CabalM ()
replComponent pkg_descr lbi suffixes
               comp@(CLib lib) clbi _ = do
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    let libbi = libBuildInfo lib
        lib' = lib { libBuildInfo = libbi { cSources = cSources libbi ++ extras } }
    replLib pkg_descr lbi lib' clbi

replComponent pkg_descr lbi suffixes
               comp@(CFLib flib) clbi _ = do
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    replFLib pkg_descr lbi flib clbi

replComponent pkg_descr lbi suffixes
               comp@(CExe exe) clbi _ = do
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    let ebi = buildInfo exe
        exe' = exe { buildInfo = ebi { cSources = cSources ebi ++ extras } }
    replExe pkg_descr lbi exe' clbi


replComponent pkg_descr lbi suffixes
               comp@(CTest test@TestSuite { testInterface = TestSuiteExeV10{} })
               clbi _distPref = do
    let exe = testSuiteExeV10AsExe test
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    let ebi = buildInfo exe
        exe' = exe { buildInfo = ebi { cSources = cSources ebi ++ extras } }
    replExe pkg_descr lbi exe' clbi


replComponent pkg_descr lbi0 suffixes
               comp@(CTest
                 test@TestSuite { testInterface = TestSuiteLibV09{} })
               clbi distPref = do
    pwd <- liftIO getCurrentDirectory
    let (pkg, lib, libClbi, lbi, _, _, _) =
          testSuiteLibV09AsLibAndExe pkg_descr test clbi lbi0 distPref pwd
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    let libbi = libBuildInfo lib
        lib' = lib { libBuildInfo = libbi { cSources = cSources libbi ++ extras } }
    replLib pkg lbi lib' libClbi


replComponent _ _ _
              (CTest TestSuite { testInterface = TestSuiteUnsupported tt })
              _ _ =
    die' $ "No support for building test suite type " ++ display tt


replComponent pkg_descr lbi suffixes
               comp@(CBench bm@Benchmark { benchmarkInterface = BenchmarkExeV10 {} })
               clbi _ = do
    let (exe, exeClbi) = benchmarkExeV10asExe bm clbi
    preprocessComponent pkg_descr comp lbi clbi False suffixes
    extras <- preprocessExtras comp lbi
    let ebi = buildInfo exe
        exe' = exe { buildInfo = ebi { cSources = cSources ebi ++ extras } }
    replExe pkg_descr lbi exe' exeClbi


replComponent _ _ _
              (CBench Benchmark { benchmarkInterface = BenchmarkUnsupported tt })
              _ _ =
    die' $ "No support for building benchmark type " ++ display tt

----------------------------------------------------
-- Shared code for buildComponent and replComponent
--

-- | Translate a exe-style 'TestSuite' component into an exe for building
testSuiteExeV10AsExe :: TestSuite -> Executable
testSuiteExeV10AsExe test@TestSuite { testInterface = TestSuiteExeV10 _ mainFile } =
    Executable {
      exeName    = testName test,
      modulePath = mainFile,
      exeScope   = ExecutablePublic,
      buildInfo  = testBuildInfo test
    }
testSuiteExeV10AsExe TestSuite{} = error "testSuiteExeV10AsExe: wrong kind"

-- | Translate a lib-style 'TestSuite' component into a lib + exe for building
testSuiteLibV09AsLibAndExe :: PackageDescription
                           -> TestSuite
                           -> ComponentLocalBuildInfo
                           -> LocalBuildInfo
                           -> FilePath
                           -> FilePath
                           -> (PackageDescription,
                               Library, ComponentLocalBuildInfo,
                               LocalBuildInfo,
                               IPI.InstalledPackageInfo,
                               Executable, ComponentLocalBuildInfo)
testSuiteLibV09AsLibAndExe pkg_descr
                     test@TestSuite { testInterface = TestSuiteLibV09 _ m }
                     clbi lbi distPref pwd =
    (pkg, lib, libClbi, lbi, ipi, exe, exeClbi)
  where
    bi  = testBuildInfo test
    lib = Library {
            libName = Nothing,
            exposedModules = [ m ],
            reexportedModules = [],
            signatures = [],
            libExposed     = True,
            libBuildInfo   = bi
          }
    -- This is, like, the one place where we use a CTestName for a library.
    -- Should NOT use library name, since that could conflict!
    PackageIdentifier pkg_name pkg_ver = package pkg_descr
    compat_name = computeCompatPackageName pkg_name (Just (testName test))
    compat_key = computeCompatPackageKey (compiler lbi) compat_name pkg_ver (componentUnitId clbi)
    libClbi = LibComponentLocalBuildInfo
                { componentPackageDeps = componentPackageDeps clbi
                , componentInternalDeps = componentInternalDeps clbi
                , componentIsIndefinite_ = False
                , componentExeDeps = componentExeDeps clbi
                , componentLocalName = CSubLibName (testName test)
                , componentIsPublic = False
                , componentIncludes = componentIncludes clbi
                , componentUnitId = componentUnitId clbi
                , componentComponentId = componentComponentId clbi
                , componentInstantiatedWith = []
                , componentCompatPackageName = compat_name
                , componentCompatPackageKey = compat_key
                , componentExposedModules = [IPI.ExposedModule m Nothing]
                }
    pkg = pkg_descr {
            package      = (package pkg_descr) { pkgName = mkPackageName $ unMungedPackageName compat_name }
          , executables  = []
          , testSuites   = []
          , subLibraries = [lib]
          }
    ipi    = inplaceInstalledPackageInfo pwd distPref pkg (mkAbiHash "") lib lbi libClbi
    testDir = buildDir lbi </> stubName test
          </> stubName test ++ "-tmp"
    testLibDep = thisPackageVersion $ package pkg
    exe = Executable {
            exeName    = mkUnqualComponentName $ stubName test,
            modulePath = stubFilePath test,
            exeScope   = ExecutablePublic,
            buildInfo  = (testBuildInfo test) {
                           hsSourceDirs       = [ testDir ],
                           targetBuildDepends = testLibDep
                             : (targetBuildDepends $ testBuildInfo test)
                         }
          }
    -- | The stub executable needs a new 'ComponentLocalBuildInfo'
    -- that exposes the relevant test suite library.
    deps = (IPI.installedUnitId ipi, mungedId ipi)
         : (filter (\(_, x) -> let name = unMungedPackageName $ mungedName x
                               in name == "Cabal" || name == "base")
                   (componentPackageDeps clbi))
    exeClbi = ExeComponentLocalBuildInfo {
                -- TODO: this is a hack, but as long as this is unique
                -- (doesn't clobber something) we won't run into trouble
                componentUnitId = mkUnitId (stubName test),
                componentComponentId = mkComponentId (stubName test),
                componentInternalDeps = [componentUnitId clbi],
                componentExeDeps = [],
                componentLocalName = CExeName $ mkUnqualComponentName $ stubName test,
                componentPackageDeps = deps,
                -- Assert DefUnitId invariant!
                -- Executable can't be indefinite, so dependencies must
                -- be definite packages.
                componentIncludes = zip (map (DefiniteUnitId . unsafeMkDefUnitId . fst) deps)
                                        (repeat defaultRenaming)
              }
testSuiteLibV09AsLibAndExe _ TestSuite{} _ _ _ _ = error "testSuiteLibV09AsLibAndExe: wrong kind"


-- | Translate a exe-style 'Benchmark' component into an exe for building
benchmarkExeV10asExe :: Benchmark -> ComponentLocalBuildInfo
                     -> (Executable, ComponentLocalBuildInfo)
benchmarkExeV10asExe bm@Benchmark { benchmarkInterface = BenchmarkExeV10 _ f }
                     clbi =
    (exe, exeClbi)
  where
    exe = Executable {
            exeName    = benchmarkName bm,
            modulePath = f,
            exeScope   = ExecutablePublic,
            buildInfo  = benchmarkBuildInfo bm
          }
    exeClbi = ExeComponentLocalBuildInfo {
                componentUnitId = componentUnitId clbi,
                componentComponentId = componentComponentId clbi,
                componentLocalName = CExeName (benchmarkName bm),
                componentInternalDeps = componentInternalDeps clbi,
                componentExeDeps = componentExeDeps clbi,
                componentPackageDeps = componentPackageDeps clbi,
                componentIncludes = componentIncludes clbi
              }
benchmarkExeV10asExe Benchmark{} _ = error "benchmarkExeV10asExe: wrong kind"

-- | Initialize a new package db file for libraries defined
-- internally to the package.
createInternalPackageDB :: LocalBuildInfo -> FilePath
                        -> CabalM PackageDB
createInternalPackageDB lbi distPref = do
    liftIO $ do
      existsAlready <- doesPackageDBExist dbPath
      when existsAlready $ deletePackageDB dbPath
    createPackageDB (compiler lbi) (withPrograms lbi) False dbPath
    return (SpecificPackageDB dbPath)
  where
    dbPath = internalPackageDBPath lbi distPref

addInternalBuildTools :: PackageDescription -> LocalBuildInfo -> BuildInfo
                      -> ProgramDb -> ProgramDb
addInternalBuildTools pkg lbi bi progs =
    foldr updateProgram progs internalBuildTools
  where
    internalBuildTools =
      [ simpleConfiguredProgram toolName' (FoundOnSystem toolLocation)
      | toolName <- getAllInternalToolDependencies pkg bi
      , let toolName' = unUnqualComponentName toolName
      , let toolLocation = buildDir lbi </> toolName' </> toolName' <.> exeExtension ]


-- TODO: build separate libs in separate dirs so that we can build
-- multiple libs, e.g. for 'LibTest' library-style test suites
buildLib :: Flag (Maybe Int)
         -> PackageDescription -> LocalBuildInfo
         -> Library            -> ComponentLocalBuildInfo -> CabalM ()
buildLib numJobs pkg_descr lbi lib clbi =
  case compilerFlavor (compiler lbi) of
    GHC   -> GHC.buildLib   numJobs pkg_descr lbi lib clbi
    GHCJS -> GHCJS.buildLib numJobs pkg_descr lbi lib clbi
    JHC   -> JHC.buildLib   pkg_descr lbi lib clbi
    LHC   -> LHC.buildLib   pkg_descr lbi lib clbi
    UHC   -> UHC.buildLib   pkg_descr lbi lib clbi
    HaskellSuite {} -> HaskellSuite.buildLib pkg_descr lbi lib clbi
    _    -> die' "Building is not supported with this compiler."

-- | Build a foreign library
--
-- NOTE: We assume that we already checked that we can actually build the
-- foreign library in configure.
buildFLib :: Flag (Maybe Int)
          -> PackageDescription -> LocalBuildInfo
          -> ForeignLib         -> ComponentLocalBuildInfo -> CabalM ()
buildFLib numJobs pkg_descr lbi flib clbi =
    case compilerFlavor (compiler lbi) of
      GHC -> GHC.buildFLib numJobs pkg_descr lbi flib clbi
      _   -> die' "Building is not supported with this compiler."

buildExe :: Flag (Maybe Int)
         -> PackageDescription -> LocalBuildInfo
         -> Executable         -> ComponentLocalBuildInfo -> CabalM ()
buildExe numJobs pkg_descr lbi exe clbi =
  case compilerFlavor (compiler lbi) of
    GHC   -> GHC.buildExe   numJobs pkg_descr lbi exe clbi
    GHCJS -> GHCJS.buildExe numJobs pkg_descr lbi exe clbi
    JHC   -> JHC.buildExe   pkg_descr lbi exe clbi
    LHC   -> LHC.buildExe   pkg_descr lbi exe clbi
    UHC   -> UHC.buildExe   pkg_descr lbi exe clbi
    _     -> die' "Building is not supported with this compiler."

replLib :: PackageDescription -> LocalBuildInfo
        -> Library            -> ComponentLocalBuildInfo -> CabalM ()
replLib pkg_descr lbi lib clbi =
  case compilerFlavor (compiler lbi) of
    -- 'cabal repl' doesn't need to support 'ghc --make -j', so we just pass
    -- NoFlag as the numJobs parameter.
    GHC   -> GHC.replLib   NoFlag pkg_descr lbi lib clbi
    GHCJS -> GHCJS.replLib NoFlag pkg_descr lbi lib clbi
    _     -> die' "A REPL is not supported for this compiler."

replExe :: PackageDescription -> LocalBuildInfo
        -> Executable         -> ComponentLocalBuildInfo -> CabalM ()
replExe pkg_descr lbi exe clbi =
  case compilerFlavor (compiler lbi) of
    GHC   -> GHC.replExe   NoFlag pkg_descr lbi exe clbi
    GHCJS -> GHCJS.replExe NoFlag pkg_descr lbi exe clbi
    _     -> die' "A REPL is not supported for this compiler."

replFLib :: PackageDescription -> LocalBuildInfo
         -> ForeignLib         -> ComponentLocalBuildInfo -> CabalM ()
replFLib pkg_descr lbi exe clbi =
  case compilerFlavor (compiler lbi) of
    GHC -> GHC.replFLib NoFlag pkg_descr lbi exe clbi
    _   -> die' "A REPL is not supported for this compiler."

-- | Runs 'componentInitialBuildSteps' on every configured component.
initialBuildSteps :: FilePath -- ^"dist" prefix
                  -> PackageDescription  -- ^mostly information from the .cabal file
                  -> LocalBuildInfo -- ^Configuration information
                  -> CabalM ()
initialBuildSteps distPref pkg_descr lbi =
    withAllComponentsInBuildOrder pkg_descr lbi $ \_comp ->
        componentInitialBuildSteps distPref pkg_descr lbi

-- | Creates the autogenerated files for a particular configured component.
componentInitialBuildSteps :: FilePath -- ^"dist" prefix
                  -> PackageDescription  -- ^mostly information from the .cabal file
                  -> LocalBuildInfo -- ^Configuration information
                  -> ComponentLocalBuildInfo
                  -> CabalM ()
componentInitialBuildSteps _distPref pkg_descr lbi clbi = do
  createDirectoryIfMissingVerbose True (componentBuildDir lbi clbi)

  writeAutogenFiles pkg_descr lbi clbi

-- | Generate and write out the Paths_<pkg>.hs and cabal_macros.h files
--
writeAutogenFiles :: PackageDescription
                  -> LocalBuildInfo
                  -> ComponentLocalBuildInfo
                  -> CabalM ()
writeAutogenFiles pkg lbi clbi = do
  createDirectoryIfMissingVerbose True (autogenComponentModulesDir lbi clbi)

  let pathsModulePath = autogenComponentModulesDir lbi clbi
                 </> ModuleName.toFilePath (autogenPathsModuleName pkg) <.> "hs"
      pathsModuleDir = takeDirectory pathsModulePath
  -- Ensure that the directory exists!
  createDirectoryIfMissingVerbose True pathsModuleDir
  rewriteFileEx pathsModulePath (Build.PathsModule.generate pkg lbi clbi)

  --TODO: document what we're doing here, and move it to its own function
  case clbi of
    LibComponentLocalBuildInfo { componentInstantiatedWith = insts } ->
        -- Write out empty hsig files for all requirements, so that GHC
        -- has a source file to look at it when it needs to typecheck
        -- a signature.  It's harmless to write these out even when
        -- there is a real hsig file written by the user, since
        -- include path ordering ensures that the real hsig file
        -- will always be picked up before the autogenerated one.
        for_ (map fst insts) $ \mod_name -> do
            let sigPath = autogenComponentModulesDir lbi clbi
                      </> ModuleName.toFilePath mod_name <.> "hsig"
            createDirectoryIfMissingVerbose True (takeDirectory sigPath)
            rewriteFileEx sigPath $
                "{-# LANGUAGE NoImplicitPrelude #-}\n" ++
                "signature " ++ display mod_name ++ " where"
    _ -> return ()

  let cppHeaderPath = autogenComponentModulesDir lbi clbi </> cppHeaderName
  rewriteFileEx cppHeaderPath (Build.Macros.generate pkg lbi clbi)
