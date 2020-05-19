{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Citrous.Unit.MediaTypes where

import           Data.Aeson                     (FromJSON, ToJSON,
                                                 eitherDecodeStrict, encode)
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (ByteString, LazyByteString,
                                                 LazyText, Text)
import           Data.Proxy                     (Proxy (..))
import           Network.HTTP.Media             (MediaType, (//), (/:))
import           Prelude                        hiding (head)

data JSON

data TextPlain

class Accept ctype where
  mediaType :: MediaType

instance Accept JSON where
  mediaType =
    "application" // "json" /: ("charset", "UTF-8")

instance Accept TextPlain where
  mediaType =
    "text" // "plain" /: ("charset", "UTF-8")

class Accept ctype => MimeEncode ctype a where
  mimeEncode :: a -> LazyByteString

instance {-# OVERLAPPABLE #-} ToJSON a => MimeEncode JSON a where
  mimeEncode = encode

instance MimeEncode TextPlain LazyText where
  mimeEncode = convert

instance MimeEncode TextPlain Text where
  mimeEncode = convert

instance MimeEncode TextPlain String where
  mimeEncode = convert

class Accept ctype => MimeDecode ctype a where
  mimeDecode :: ByteString -> Either String a

instance FromJSON a => MimeDecode JSON a where
  mimeDecode = eitherDecodeStrict

instance MimeDecode TextPlain LazyText where
  mimeDecode = Right . convert

instance MimeDecode TextPlain Text where
  mimeDecode = Right . convert

instance MimeDecode TextPlain String where
  mimeDecode = Right . convert
