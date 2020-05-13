{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Citrous.Unit.Responder where

import           Citrous.Unit.ServerErr
import           Control.Monad.Error.Class      (MonadError)
import           Data.Aeson                     (ToJSON)
import qualified Data.Aeson                     as Aeson
import           Data.ByteString.Builder        (Builder)
import           Data.Convertible.Utf8          (Convertible, convert)
import           Data.Convertible.Utf8.Internal (ByteString, ByteStringBuilder,
                                                 LazyByteString, LazyText,
                                                 ShortByteString, ShortText,
                                                 Text, TextBuilder)
import           Network.HTTP.Types.Header      as Header
import           Network.HTTP.Types.Status      (Status)
import qualified Network.HTTP.Types.Status      as Status
import           Network.Wai                    (Application, Response)
import qualified Network.Wai                    as Wai

class Responder a where
  respond :: a -> Application
  respond a req send = withStatus Status.ok200 a >>= send
  withStatus :: Status -> a -> IO Response

instance {-# OVERLAPPABLE #-} ToJSON a => Responder a where
  withStatus status json =
    return $ Wai.responseLBS status [(Header.hContentType, "application/json; charset=utf-8")] $ Aeson.encode json

instance (Responder a) => Responder (IO a) where
  withStatus status io = io >>= withStatus status

withStatusText :: (Convertible a Builder) => Status -> a -> IO Response
withStatusText status string =
  return $ Wai.responseBuilder status [(Header.hContentType, "text/plain; charset=utf-8")] $ convert string

withStatusByte :: (Convertible a Builder) => Status -> a -> IO Response
withStatusByte status string =
  return $ Wai.responseBuilder status [(Header.hContentType, "application/octet-stream; charset=utf-8")] $ convert string

instance Responder String where
  withStatus = withStatusText

instance Responder Text where
  withStatus = withStatusText

instance Responder LazyText where
  withStatus = withStatusText

instance Responder TextBuilder where
  withStatus = withStatusText

instance Responder ShortText where
  withStatus = withStatusText

instance Responder ByteString where
  withStatus = withStatusByte

instance Responder LazyByteString where
  withStatus = withStatusByte

instance Responder ByteStringBuilder where
  withStatus = withStatusByte

instance Responder ShortByteString where
  withStatus = withStatusByte

instance Responder a => Responder (Maybe a) where
  withStatus status maybe =
    case maybe of
      Just x  -> withStatus status x
      Nothing -> return $ Wai.responseBuilder Status.notFound404 [] mempty
