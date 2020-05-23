{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Citrous.Integration.Handler where

import           Citrous.Unit.ServerErr     (ServerErr, responseServerError)
import           Control.Monad.Except
import           Control.Monad.Reader       (Reader, ReaderT, runReader,
                                             runReaderT)
import           Control.Monad.Reader.Class (MonadReader)
import           Data.Functor.Identity      (Identity, runIdentity)
import           GHC.Generics               (Generic)
import           Network.Wai                (Response)

--------------------------
class Monad m => MonadHandler m env where
  runMonadHandler :: env -> m a -> IO a

instance MonadHandler Identity r where
  runMonadHandler _ a = return $ runIdentity a

instance MonadHandler (Reader r) r where
  runMonadHandler r ma = return $ runReader ma r

instance MonadHandler IO r where
  runMonadHandler _ = id

instance MonadHandler (ReaderT r IO) r where
  runMonadHandler r ma = runReaderT ma r

--instance MonadHandler (Except ServerErr) r where
--  runMonadHandler _ ma = return <$> runExcept ma
