{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Citrous.Integration.Handler where

import           Control.Monad.Cont        (MonadIO)
import           Control.Monad.State.Class
import           Data.Data                 (Proxy (..))
import           Data.Functor.Identity     (Identity, runIdentity)
import           GHC.Generics              (Generic)

class Monad m => Handler' m where
  runHandler' :: m a -> a

newtype Handler a = Handler { runHandler :: a }
  deriving (Functor)

instance Applicative Handler where
  pure = Handler
  (Handler f) <*> (Handler x) = pure (f x)

instance Monad Handler where
  return = pure
  (Handler x) >>= f = f x

instance Handler' Handler where
  runHandler' (Handler x) = x

instance Handler' Identity where
  runHandler' = runIdentity
