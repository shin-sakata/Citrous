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

newtype Handler a = Handler { runHandler :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
