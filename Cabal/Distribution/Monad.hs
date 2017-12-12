{-# LANGUAGE RankNTypes #-}
module Distribution.Monad (
    CabalM(..)
  , askVerbosity
  , localVerbosity
  , runCabalMInIO
  , MonadIO(..)
  ) where

import Control.Monad.Trans (MonadIO(..))

import Distribution.Verbosity (Verbosity)

newtype CabalM a = CabalM { runCabalM :: Verbosity -> IO a }

askVerbosity :: CabalM Verbosity
askVerbosity = CabalM pure

localVerbosity :: (Verbosity -> Verbosity) -> CabalM a -> CabalM a
localVerbosity f a = CabalM $ \v -> runCabalM a (f v)

instance Functor CabalM where
  fmap f (CabalM g) = CabalM (fmap (fmap f) g)

instance Applicative CabalM where
  pure = CabalM . pure . pure
  (<*>) (CabalM f) (CabalM x) = CabalM (\v -> f v <*> x v)

instance Monad CabalM where
  return = pure
  (>>=) (CabalM x) f = CabalM (\v -> x v >>= flip runCabalM v . f)

instance MonadIO CabalM where
  liftIO = CabalM . const

-- TODO MonadFail instance

runCabalMInIO :: ((forall x. CabalM x -> IO x) -> IO a) -> CabalM a
runCabalMInIO f = CabalM $ \verbosity -> f (flip runCabalM verbosity)
