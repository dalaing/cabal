{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.HaskellSuite where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Data.Map as Map (empty)

import Distribution.Simple.Program
import Distribution.Simple.Compiler as Compiler
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths
import Distribution.Monad
import Distribution.Version
import Distribution.Text
import Distribution.Package
import Distribution.InstalledPackageInfo hiding (includeDirs)
import Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.System (Platform)
import Distribution.Compat.Exception
import Language.Haskell.Extension
import Distribution.Simple.Program.Builtin

configure
  :: Maybe FilePath -> Maybe FilePath
  -> ProgramDb -> CabalM (Compiler, Maybe Platform, ProgramDb)
configure mbHcPath hcPkgPath progdb0 = do

  -- We have no idea how a haskell-suite tool is named, so we require at
  -- least some information from the user.
  hcPath <-
    let msg = "You have to provide name or path of a haskell-suite tool (-w PATH)"
    in maybe (die' msg) return mbHcPath

  when (isJust hcPkgPath) $
    warn "--with-hc-pkg option is ignored for haskell-suite"

  (comp, confdCompiler, progdb1) <- configureCompiler hcPath progdb0

  -- Update our pkg tool. It uses the same executable as the compiler, but
  -- all command start with "pkg"
  (confdPkg, _) <- requireProgram haskellSuitePkgProgram progdb1
  let progdb2 =
        updateProgram
          confdPkg
            { programLocation = programLocation confdCompiler
            , programDefaultArgs = ["pkg"]
            }
          progdb1

  return (comp, Nothing, progdb2)

  where
    configureCompiler hcPath progdb0' = do
      let
        haskellSuiteProgram' =
          haskellSuiteProgram
            { programFindLocation = \p -> findProgramOnSearchPath p hcPath }

      -- NB: cannot call requireProgram right away — it'd think that
      -- the program is already configured and won't reconfigure it again.
      -- Instead, call configureProgram directly first.
      progdb1 <- configureProgram haskellSuiteProgram' progdb0'
      (confdCompiler, progdb2) <- requireProgram haskellSuiteProgram' progdb1

      extensions <- getExtensions confdCompiler
      languages  <- getLanguages  confdCompiler
      (compName, compVersion) <-
        getCompilerVersion confdCompiler

      let
        comp = Compiler {
          compilerId             = CompilerId (HaskellSuite compName) compVersion,
          compilerAbiTag         = Compiler.NoAbiTag,
          compilerCompat         = [],
          compilerLanguages      = languages,
          compilerExtensions     = extensions,
          compilerProperties     = Map.empty
        }

      return (comp, confdCompiler, progdb2)

hstoolVersion :: FilePath -> CabalM (Maybe Version)
hstoolVersion = findProgramVersion "--hspkg-version" id

numericVersion :: FilePath -> CabalM (Maybe Version)
numericVersion = findProgramVersion "--compiler-version" (last . words)

getCompilerVersion :: ConfiguredProgram -> CabalM (String, Version)
getCompilerVersion prog = do
  output <- rawSystemStdout (programPath prog) ["--compiler-version"]
  let
    parts = words output
    name = concat $ init parts -- there shouldn't be any spaces in the name anyway
    versionStr = last parts
  version <-
    maybe (die' "haskell-suite: couldn't determine compiler version") return $
      simpleParse versionStr
  return (name, version)

getExtensions :: ConfiguredProgram -> CabalM [(Extension, Maybe Compiler.Flag)]
getExtensions prog = do
  extStrs <-
    lines `fmap`
    rawSystemStdout (programPath prog) ["--supported-extensions"]
  return
    [ (ext, Just $ "-X" ++ display ext) | Just ext <- map simpleParse extStrs ]

getLanguages :: ConfiguredProgram -> CabalM [(Language, Compiler.Flag)]
getLanguages prog = do
  langStrs <-
    lines `fmap`
    rawSystemStdout (programPath prog) ["--supported-languages"]
  return
    [ (ext, "-G" ++ display ext) | Just ext <- map simpleParse langStrs ]

-- Other compilers do some kind of a packagedb stack check here. Not sure
-- if we need something like that as well.
getInstalledPackages :: PackageDBStack -> ProgramDb
                     -> CabalM InstalledPackageIndex
getInstalledPackages packagedbs progdb =
  fmap (PackageIndex.fromList . concat) $ for packagedbs $ \packagedb ->
    do str <- runCabalMInIO $ \liftC -> do
        liftC (getDbProgramOutput haskellSuitePkgProgram progdb
                ["dump", packageDbOpt packagedb])
         `catchExit` \_ -> liftC $ die' $ "pkg dump failed"
       case parsePackages str of
         Right ok -> return ok
         _       -> die' "failed to parse output of 'pkg dump'"

  where
    parsePackages str =
      let parsed = map parseInstalledPackageInfo (splitPkgs str)
       in case [ msg | ParseFailed msg <- parsed ] of
            []   -> Right [ pkg | ParseOk _ pkg <- parsed ]
            msgs -> Left msgs

    splitPkgs :: String -> [String]
    splitPkgs = map unlines . splitWith ("---" ==) . lines
      where
        splitWith :: (a -> Bool) -> [a] -> [[a]]
        splitWith p xs = ys : case zs of
                           []   -> []
                           _:ws -> splitWith p ws
          where (ys,zs) = break p xs

buildLib
  :: PackageDescription -> LocalBuildInfo
  -> Library -> ComponentLocalBuildInfo -> CabalM ()
buildLib pkg_descr lbi lib clbi = do
  -- In future, there should be a mechanism for the compiler to request any
  -- number of the above parameters (or their parts) — in particular,
  -- pieces of PackageDescription.
  --
  -- For now, we only pass those that we know are used.

  let odir = buildDir lbi
      bi = libBuildInfo lib
      srcDirs = hsSourceDirs bi ++ [odir]
      dbStack = withPackageDB lbi
      language = fromMaybe Haskell98 (defaultLanguage bi)
      progdb = withPrograms lbi
      pkgid = packageId pkg_descr

  runDbProgram haskellSuiteProgram progdb $
    [ "compile", "--build-dir", odir ] ++
    concat [ ["-i", d] | d <- srcDirs ] ++
    concat [ ["-I", d] | d <- [autogenComponentModulesDir lbi clbi
                              ,autogenPackageModulesDir lbi
                              ,odir] ++ includeDirs bi ] ++
    [ packageDbOpt pkgDb | pkgDb <- dbStack ] ++
    [ "--package-name", display pkgid ] ++
    concat [ ["--package-id", display ipkgid ]
           | (ipkgid, _) <- componentPackageDeps clbi ] ++
    ["-G", display language] ++
    concat [ ["-X", display ex] | ex <- usedExtensions bi ] ++
    cppOptions (libBuildInfo lib) ++
    [ display modu | modu <- allLibModules lib clbi ]



installLib
  :: LocalBuildInfo
  -> FilePath  -- ^install location
  -> FilePath  -- ^install location for dynamic libraries
  -> FilePath  -- ^Build location
  -> PackageDescription
  -> Library
  -> ComponentLocalBuildInfo
  -> CabalM ()
installLib lbi targetDir dynlibTargetDir builtDir pkg lib clbi = do
  let progdb = withPrograms lbi
  runDbProgram haskellSuitePkgProgram progdb $
    [ "install-library"
    , "--build-dir", builtDir
    , "--target-dir", targetDir
    , "--dynlib-target-dir", dynlibTargetDir
    , "--package-id", display $ packageId pkg
    ] ++ map display (allLibModules lib clbi)

registerPackage
  :: ProgramDb
  -> PackageDBStack
  -> InstalledPackageInfo
  -> CabalM ()
registerPackage progdb packageDbs installedPkgInfo = do
  (hspkg, _) <- requireProgram haskellSuitePkgProgram progdb

  runProgramInvocation $
    (programInvocation hspkg
      ["update", packageDbOpt $ last packageDbs])
      { progInvokeInput = Just $ showInstalledPackageInfo installedPkgInfo }

initPackageDB :: ProgramDb -> FilePath -> CabalM ()
initPackageDB progdb dbPath =
  runDbProgram haskellSuitePkgProgram progdb
    ["init", dbPath]

packageDbOpt :: PackageDB -> String
packageDbOpt GlobalPackageDB        = "--global"
packageDbOpt UserPackageDB          = "--user"
packageDbOpt (SpecificPackageDB db) = "--package-db=" ++ db
