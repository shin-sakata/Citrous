module Citrous.Unit.Handler where

import           Data.Functor.Identity (Identity, runIdentity)

class Container c where
  runHandler :: c a -> IO a

instance Container Identity where
  runHandler = return . runIdentity
