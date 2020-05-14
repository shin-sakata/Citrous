{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}


module Citrous.Capture where

import           Data.Convertible.Utf8          (convert, Convertible)
import           Data.Convertible.Utf8.Internal (Text, ByteString)
import           GHC.TypeLits                   (Symbol)
import           Text.Read                      (readMaybe)

class FromPath a where
  fromPath :: Text -> Maybe a

instance {-# OVERLAPPING #-} FromPath Text where
  fromPath = Just

instance {-# OVERLAPPING #-} FromPath String where
  fromPath a = Just $ convert a

instance {-# OVERLAPPING #-} FromPath ByteString where
  fromPath a = Just $ convert a

instance {-# OVERLAPPABLE #-} Read a => FromPath a where
  fromPath = readMaybe . convert

data Capture (sym :: Symbol) (a :: *)
