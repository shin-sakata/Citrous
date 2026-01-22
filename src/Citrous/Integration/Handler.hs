{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Citrous.Integration.Handler where

import           Citrous.Unit.ServerErr (ServerErr, responseServerError)
import           Control.Monad.Except
import           Control.Monad.Reader   (Reader, ReaderT, runReader, runReaderT)
import           Data.Functor.Identity  (Identity, runIdentity)
import           Network.Wai            (Response)

toResponse :: Either ServerErr (IO Response) -> IO Response
toResponse (Right res) = res
toResponse (Left err)  = return $ responseServerError err

--------------------------
class Monad m => MonadHandler m env where
  runMonadHandler :: env -> m a -> Either ServerErr (IO a)

instance MonadHandler Identity r where
  runMonadHandler _ a = Right $ return $ runIdentity a

instance MonadHandler (Reader r) r where
  runMonadHandler r ma = Right $ return $ runReader ma r

instance MonadHandler IO r where
  runMonadHandler _ = Right

instance MonadHandler (ReaderT r IO) r where
  runMonadHandler r ma = Right $ runReaderT ma r

instance MonadHandler (Except ServerErr) r where
  runMonadHandler _ ma = return <$> runExcept ma
 