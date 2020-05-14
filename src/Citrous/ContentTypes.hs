{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Citrous.ContentTypes where

import           Data.Aeson                     (ToJSON, eitherDecode, encode, FromJSON)
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (ByteString, LazyByteString,
                                                 LazyText, Text)
import           Data.Proxy                     (Proxy (..))
import           Network.HTTP.Media             (MediaType, (//), (/:))
import           Prelude                        hiding (head)

data JSON

data TextPlain

class Accept ctype where
  contentTypes :: MediaType

instance Accept JSON where
  contentTypes =
    "application" // "json" /: ("charset", "UTF-8")

instance Accept TextPlain where
  contentTypes =
    "text" // "plain" /: ("charset", "utf-8")

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
  mimeDecode :: LazyByteString -> Either String a

instance FromJSON a => MimeDecode JSON a where
  mimeDecode = eitherDecode

instance MimeDecode TextPlain LazyText where
  mimeDecode = Right . convert

instance MimeDecode TextPlain Text where
  mimeDecode = Right . convert

instance MimeDecode TextPlain String where
  mimeDecode = Right . convert
