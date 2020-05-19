{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Citrous.Unit.Capture where

import           GHC.TypeLits (Symbol)

data Capture (sym :: Symbol) (a :: *)
