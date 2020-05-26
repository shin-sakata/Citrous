{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Citrous.Unit.MediaTypes where

import           Control.Arrow                  (left)
import           Data.Aeson                     (FromJSON, ToJSON,
                                                 eitherDecodeStrict, encode)
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (ByteString, LazyByteString,
                                                 LazyText, Text)
import           Network.HTTP.Media             (MediaType, matchAccept, (//),
                                                 (/:))
import           Web.FormUrlEncoded             (FromForm, ToForm,
                                                 urlDecodeAsForm,
                                                 urlEncodeAsForm)

data JSON
data TextPlain
data FormUrlEncoded

type HeaderValue = ByteString

type Body = ByteString

isAccept :: forall ctype. Accept ctype => HeaderValue -> Bool
isAccept header = case matchAccept [mediaType @ctype] header of
  Just _  -> True
  Nothing -> False

class Accepts ctypes where
  mediaTypes :: [MediaType]

instance Accepts '[] where
  mediaTypes = []

instance (Accept ctype, Accepts ctypes) => Accepts (ctype : ctypes) where
  mediaTypes = mediaType @ctype : mediaTypes @ctypes

-- NothingだったらNotAcceptable
-- Just (Left x)だったらデコードエラー
-- Just (Right x)だと成功
class Accepts ctypes => MimesDecode ctypes a where
  mimesDecode :: Body -> HeaderValue -> Maybe (Either String a)

instance MimesDecode '[] a where
  mimesDecode _ _ = Nothing

instance (MimeDecode ctype a, MimesDecode rest a) => MimesDecode (ctype : rest) a where
  mimesDecode body headerValue =
    if isAccept @ctype headerValue
      then Just $ mimeDecode @ctype body
      else mimesDecode @rest body headerValue

class Accept ctype where
  mediaType :: MediaType

instance Accept JSON where
  mediaType =
    "application" // "json" /: ("charset", "UTF-8")

instance Accept TextPlain where
  mediaType =
    "text" // "plain" /: ("charset", "UTF-8")

-- | @application/x-www-form-urlencoded@
instance Accept FormUrlEncoded where
  mediaType = "application" // "x-www-form-urlencoded"

class Accept ctype => MimeEncode ctype a where
  mimeEncode :: a -> LazyByteString

instance {-# OVERLAPPABLE #-} ToJSON a => MimeEncode JSON a where
  mimeEncode = encode

instance {-# OVERLAPPABLE #-} Show s => MimeEncode TextPlain s where
  mimeEncode = convert . show

instance {-# OVERLAPPABLE #-} ToForm a => MimeEncode FormUrlEncoded a where
  mimeEncode = urlEncodeAsForm

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

instance FromForm a => MimeDecode FormUrlEncoded a where
  mimeDecode = left convert . (urlDecodeAsForm . convert)

instance MimeDecode TextPlain LazyText where
  mimeDecode = Right . convert

instance MimeDecode TextPlain Text where
  mimeDecode = Right . convert

instance MimeDecode TextPlain String where
  mimeDecode = Right . convert
