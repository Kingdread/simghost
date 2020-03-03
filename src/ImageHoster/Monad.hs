{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ImageHoster.Monad
    ( -- * Data constructors
      App(..)
    , FallibleApp
      -- * Conversion functions
    , runFallibleApp
    , liftApp
    , run
      -- * Data accessors
    , settings
    , settings'
    ) where

import ImageHoster.Data

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Network.CGI
import Network.CGI.Monad

-- | General type for application computations.
--
-- This provides:
--
--   * Access to a global `Settings` instance
--   * IO computations
--   * CGI operations
newtype App a = App { runApp :: ReaderT Settings (CGIT IO) a }
    deriving (Functor, Applicative, Monad)


instance MonadCGI App where
    cgiAddHeader k v = App . lift $ cgiAddHeader k v
    cgiGet s = App . lift $ cgiGet s


instance MonadIO App where
    liftIO = App . liftIO


-- | Type for application computations that can fail.
--
-- Computation stops after the first failing computation, e.g. 'Maybe'
-- semantics.
type FallibleApp = MaybeT App


-- | Run a 'FallibleApp' and return the result wrapped in a 'Maybe'.
runFallibleApp :: FallibleApp a -> App (Maybe a)
runFallibleApp = runMaybeT


-- | Lift an 'App' to a 'FallibleApp' that never fails.
liftApp :: App a -> FallibleApp a
liftApp a = MaybeT $ Just <$> a


-- | Run a complete application using the given 'Settings'.
run :: App CGIResult -> Settings -> IO ()
run a s = runCGI $ runReaderT (runApp a) s


-- | A lifted version of 'Control.Monad.Reader.ask'.
--
-- Provides access to the 'Settings' instance in the 'App' monad stack.
settings :: App Settings
settings = App ask


-- | A lifted version of 'Control.Monad.Reader.asks'.
--
-- Provides access to the 'Settings' instance, but applies a function before
-- returning the value:
--
-- > directory <- asks outputDir
settings' :: (Settings -> a) -> App a
settings' f = fmap f settings
