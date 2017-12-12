{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Bench
-- Copyright   :  Johan Tibell 2011
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into running the benchmarks in a built
-- package. It performs the \"@.\/setup bench@\" action. It runs
-- benchmarks designated in the package description.

module Distribution.Simple.Bench
    ( bench
    ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.UnqualComponentName
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils
import Distribution.Monad
import Distribution.Text

import System.Exit ( ExitCode(..), exitFailure, exitSuccess )
import System.Directory ( doesFileExist )
import System.FilePath ( (</>), (<.>) )

-- | Perform the \"@.\/setup bench@\" action.
bench :: Args                    -- ^positional command-line arguments
      -> PD.PackageDescription   -- ^information from the .cabal file
      -> LBI.LocalBuildInfo      -- ^information from the configure step
      -> BenchmarkFlags          -- ^flags sent to benchmark
      -> IO ()
bench args pkg_descr lbi flags = do
    let verbosity         = fromFlag $ benchmarkVerbosity flags
        benchmarkNames    = args
        pkgBenchmarks     = PD.benchmarks pkg_descr
        enabledBenchmarks = map fst (LBI.enabledBenchLBIs pkg_descr lbi)

        -- Run the benchmark
        doBench :: PD.Benchmark -> CabalM ExitCode
        doBench bm = do
            case PD.benchmarkInterface bm of
              PD.BenchmarkExeV10 _ _ -> do
                  let cmd = LBI.buildDir lbi </> name </> name <.> exeExtension
                      options = map (benchOption pkg_descr lbi bm) $
                                benchmarkOptions flags
                  -- Check that the benchmark executable exists.
                  exists <- liftIO $ doesFileExist cmd
                  unless exists $ die' $
                      "Error: Could not find benchmark program \""
                      ++ cmd ++ "\". Did you build the package first?"

                  notice $ startMessage name
                  -- This will redirect the child process
                  -- stdout/stderr to the parent process.
                  exitcode <- rawSystemExitCode cmd options
                  notice $ finishMessage name exitcode
                  return exitcode

              _ -> do
                  notice $ "No support for running "
                      ++ "benchmark " ++ name ++ " of type: "
                      ++ display (PD.benchmarkType bm)
                  liftIO exitFailure
          where name = unUnqualComponentName $ PD.benchmarkName bm

    flip runCabalM verbosity $ do
      unless (PD.hasBenchmarks pkg_descr) $ do
          notice "Package has no benchmarks."
          liftIO exitSuccess

      when (PD.hasBenchmarks pkg_descr && null enabledBenchmarks) $
          die' $ "No benchmarks enabled. Did you remember to configure with "
                ++ "\'--enable-benchmarks\'?"

      bmsToRun <- case benchmarkNames of
              [] -> return enabledBenchmarks
              names -> for names $ \bmName ->
                  let benchmarkMap = zip enabledNames enabledBenchmarks
                      enabledNames = map PD.benchmarkName enabledBenchmarks
                      allNames = map PD.benchmarkName pkgBenchmarks
                  in case lookup (mkUnqualComponentName bmName) benchmarkMap of
                      Just t -> return t
                      _ | mkUnqualComponentName bmName `elem` allNames ->
                            die' $ "Package configured with benchmark "
                                  ++ bmName ++ " disabled."
                        | otherwise -> die' $ "no such benchmark: " ++ bmName

      let totalBenchmarks = length bmsToRun
      notice $ "Running " ++ show totalBenchmarks ++ " benchmarks..."
      exitcodes <- traverse doBench bmsToRun
      let allOk = totalBenchmarks == length (filter (== ExitSuccess) exitcodes)
      unless allOk $ liftIO exitFailure
  where
    startMessage name = "Benchmark " ++ name ++ ": RUNNING...\n"
    finishMessage name exitcode = "Benchmark " ++ name ++ ": "
                               ++ (case exitcode of
                                        ExitSuccess -> "FINISH"
                                        ExitFailure _ -> "ERROR")


-- TODO: This is abusing the notion of a 'PathTemplate'.  The result isn't
-- necessarily a path.
benchOption :: PD.PackageDescription
            -> LBI.LocalBuildInfo
            -> PD.Benchmark
            -> PathTemplate
            -> String
benchOption pkg_descr lbi bm template =
    fromPathTemplate $ substPathTemplate env template
  where
    env = initialPathTemplateEnv
          (PD.package pkg_descr) (LBI.localUnitId lbi)
          (compilerInfo $ LBI.compiler lbi) (LBI.hostPlatform lbi) ++
          [(BenchmarkNameVar, toPathTemplate $ unUnqualComponentName $ PD.benchmarkName bm)]
