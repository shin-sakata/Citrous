{-# LANGUAGE FlexibleInstances #-}

module Citrous.Unit.Application where

import           Network.Wai (Application)

class ToApplication a where
  toApplication :: a -> Application

instance ToApplication Application where
  toApplication = id
