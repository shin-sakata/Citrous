{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Citrous.Unit.RequestBody where

data ReqBody (mediaTypes :: [*]) (a :: *)
