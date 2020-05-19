{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Citrous.Unit.QueryParam where

import GHC.TypeLits (Symbol)

data QueryParam (param :: Symbol) (a :: *)
