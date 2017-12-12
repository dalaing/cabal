{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Strip
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @strip@ program.

module Distribution.Simple.Program.Strip (stripLib, stripExe)
       where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Monad
import Distribution.Version

import System.FilePath             (takeBaseName)

runStrip :: ProgramDb -> FilePath -> [String] -> CabalM ()
runStrip progDb path args =
  case lookupProgram stripProgram progDb of
    Just strip -> runProgram strip (args ++ [path])
    Nothing    -> unless (buildOS == Windows) $
                  -- Don't bother warning on windows, we don't expect them to
                  -- have the strip program anyway.
                  warn $ "Unable to strip executable or library '"
                                   ++ (takeBaseName path)
                                   ++ "' (missing the 'strip' program)"

stripExe :: Platform -> ProgramDb -> FilePath -> CabalM ()
stripExe (Platform _arch os) progdb path =
  runStrip progdb path args
  where
    args = case os of
       OSX -> ["-x"] -- By default, stripping the ghc binary on at least
                     -- some OS X installations causes:
                     --     HSbase-3.0.o: unknown symbol `_environ'"
                     -- The -x flag fixes that.
       _   -> []

stripLib :: Platform -> ProgramDb -> FilePath -> CabalM ()
stripLib (Platform arch os) progdb path = do
  case os of
    OSX -> -- '--strip-unneeded' is not supported on OS X, iOS, AIX, or
           -- Solaris. See #1630.
           return ()
    IOS -> return ()
    AIX -> return ()
    Solaris -> return ()
    Windows -> -- Stripping triggers a bug in 'strip.exe' for
               -- libraries with lots identically named modules. See
               -- #1784.
               return()
    Linux | arch == I386 ->
      -- Versions of 'strip' on 32-bit Linux older than 2.18 are
      -- broken. See #2339.
      let okVersion = orLaterVersion (mkVersion [2,18])
      in case programVersion =<< lookupProgram stripProgram progdb of
          Just v | withinRange v okVersion ->
            runStrip progdb path args
          _ -> warn $ "Unable to strip library '"
                                ++ (takeBaseName path)
                                ++ "' (version of 'strip' too old; "
                                ++ "requires >= 2.18 on 32-bit Linux)"
    _   -> runStrip progdb path args
  where
    args = ["--strip-unneeded"]
