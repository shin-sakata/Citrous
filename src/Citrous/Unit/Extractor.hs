{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Citrous.Unit.Extractor where

import Data.Convertible.Utf8 
import           Data.Convertible.Utf8.Internal
import           Text.Read                      (readMaybe)

class Extractor a where
  extract :: ByteString -> Maybe a

instance {-# OVERLAPPING #-} Extractor Text where
  extract = Just . convert

instance {-# OVERLAPPING #-} Extractor String where
  extract = Just . convert

instance {-# OVERLAPPING #-} Extractor ByteString where
  extract = Just . convert

instance {-# OVERLAPPING #-} Extractor LazyText where
  extract = Just . convert

instance {-# OVERLAPPING #-} Extractor LazyByteString where
  extract = Just . convert

instance {-# OVERLAPPING #-} Extractor TextBuilder where
  extract = Just . convert

instance {-# OVERLAPPING #-} Extractor ShortText where
  extract = Just . convert

instance {-# OVERLAPPING #-} Extractor ShortByteString where
  extract = Just . convert

instance {-# OVERLAPPING #-} Extractor ByteStringBuilder where
  extract = Just . convert

instance {-# OVERLAPPABLE #-} Read a => Extractor a where
  extract = readMaybe . convert
