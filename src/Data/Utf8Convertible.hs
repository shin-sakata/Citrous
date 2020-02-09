module Data.Utf8Convertible
  ( convert
  , ConvertTo
  , ConvertFrom
  , Utf8Convertible
  ) where

import qualified Codec.Binary.UTF8.String as UTF8String
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as LE
import Prelude (String, (.), id)

type Text = T.Text

type LText = LT.Text

type ByteString = BS.ByteString

type LByteString = LBS.ByteString

type TextBuilder = TB.Builder

type BSBuilder = BB.Builder

{- |
  Indicates that conversion from A to B is possible
-}
class Utf8Convertible a b where
  convert :: a -> b

{- |
  Can write constraints on values ​​that can be converted to A

  example usage
  convertToText :: ConvertTo Text a => a -> Text
  convertToText convertible = convert convertible
-}
type ConvertTo to from = Utf8Convertible from to

{- |
  Can write constraints on values ​​that can be converted from A

  example usage
  ConvertFromText :: ConvertFrom Text a => a -> String
  ConvertFromText convertible = convert convertible
-}
type ConvertFrom from to = Utf8Convertible from to

--------------------------------
-- from String instances
--------------------------------
instance S.IsString a => Utf8Convertible String a where
  {-# INLINE convert #-}
  convert = S.fromString

--------------------------------
-- from Text instances
--------------------------------
instance Utf8Convertible Text String where
  {-# INLINE convert #-}
  convert = T.unpack

instance Utf8Convertible Text Text where
  {-# INLINE convert #-}
  convert = id

instance Utf8Convertible Text LText where
  {-# INLINE convert #-}
  convert = LT.fromStrict

instance Utf8Convertible Text ByteString where
  {-# INLINE convert #-}
  convert = E.encodeUtf8

instance Utf8Convertible Text LByteString where
  {-# INLINE convert #-}
  convert = LE.encodeUtf8 . convert

instance Utf8Convertible Text TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromText

instance Utf8Convertible Text BSBuilder where
  {-# INLINE convert #-}
  convert = E.encodeUtf8Builder

--------------------------------
-- from LazyText instances
--------------------------------
instance Utf8Convertible LText String where
  {-# INLINE convert #-}
  convert = LT.unpack

instance Utf8Convertible LText Text where
  {-# INLINE convert #-}
  convert = LT.toStrict

instance Utf8Convertible LText LText where
  {-# INLINE convert #-}
  convert = id

instance Utf8Convertible LText ByteString where
  {-# INLINE convert #-}
  convert = LBS.toStrict . LE.encodeUtf8

instance Utf8Convertible LText LByteString where
  {-# INLINE convert #-}
  convert = LE.encodeUtf8

instance Utf8Convertible LText TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText

instance Utf8Convertible LText BSBuilder where
  {-# INLINE convert #-}
  convert = LE.encodeUtf8Builder

--------------------------------
-- from ByteString instances
--------------------------------
instance Utf8Convertible ByteString String where
  {-# INLINE convert #-}
  convert = UTF8String.decode . BS.unpack

instance Utf8Convertible ByteString Text where
  {-# INLINE convert #-}
  convert = E.decodeUtf8

instance Utf8Convertible ByteString LText where
  {-# INLINE convert #-}
  convert = LT.fromStrict . E.decodeUtf8

instance Utf8Convertible ByteString ByteString where
  {-# INLINE convert #-}
  convert = id

instance Utf8Convertible ByteString LByteString where
  {-# INLINE convert #-}
  convert = LBS.fromStrict

instance Utf8Convertible ByteString TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText . convert

instance Utf8Convertible ByteString BSBuilder where
  {-# INLINE convert #-}
  convert = BB.byteString

--------------------------------
-- from LazyByteString instances
--------------------------------
instance Utf8Convertible LByteString String where
  {-# INLINE convert #-}
  convert = UTF8String.decode . LBS.unpack

instance Utf8Convertible LByteString Text where
  {-# INLINE convert #-}
  convert = LT.toStrict . LE.decodeUtf8

instance Utf8Convertible LByteString LText where
  {-# INLINE convert #-}
  convert = LE.decodeUtf8

instance Utf8Convertible LByteString ByteString where
  {-# INLINE convert #-}
  convert = LBS.toStrict

instance Utf8Convertible LByteString LByteString where
  {-# INLINE convert #-}
  convert = id

instance Utf8Convertible LByteString TextBuilder where
  {-# INLINE convert #-}
  convert = TB.fromLazyText . convert

instance Utf8Convertible LByteString BSBuilder where
  {-# INLINE convert #-}
  convert = BB.lazyByteString

--------------------------------
-- from TextLazyBuilder instances
--------------------------------
instance Utf8Convertible TextBuilder String where
  {-# INLINE convert #-}
  convert = convert . TB.toLazyText

instance Utf8Convertible TextBuilder Text where
  {-# INLINE convert #-}
  convert = convert . TB.toLazyText

instance Utf8Convertible TextBuilder LText where
  {-# INLINE convert #-}
  convert = TB.toLazyText

instance Utf8Convertible TextBuilder ByteString where
  {-# INLINE convert #-}
  convert = convert . TB.toLazyText

instance Utf8Convertible TextBuilder LByteString where
  {-# INLINE convert #-}
  convert = convert . TB.toLazyText

instance Utf8Convertible TextBuilder TextBuilder where
  {-# INLINE convert #-}
  convert = id

instance Utf8Convertible TextBuilder BSBuilder where
  {-# INLINE convert #-}
  convert = convert . TB.toLazyText

--------------------------------
-- from ByteStringLazyBuilder instances
--------------------------------
instance Utf8Convertible BSBuilder String where
  {-# INLINE convert #-}
  convert = convert . BB.toLazyByteString

instance Utf8Convertible BSBuilder Text where
  {-# INLINE convert #-}
  convert = convert . BB.toLazyByteString

instance Utf8Convertible BSBuilder LText where
  {-# INLINE convert #-}
  convert = convert . BB.toLazyByteString

instance Utf8Convertible BSBuilder ByteString where
  {-# INLINE convert #-}
  convert = convert . BB.toLazyByteString

instance Utf8Convertible BSBuilder LByteString where
  {-# INLINE convert #-}
  convert = BB.toLazyByteString

instance Utf8Convertible BSBuilder TextBuilder where
  {-# INLINE convert #-}
  convert = convert . BB.toLazyByteString

instance Utf8Convertible BSBuilder BSBuilder where
  {-# INLINE convert #-}
  convert = id