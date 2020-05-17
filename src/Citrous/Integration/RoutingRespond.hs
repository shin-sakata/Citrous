{-# LANGUAGE GADTs #-}

module Citrous.Integration.RoutingRespond where

import           Citrous.Unit.Impl (StdMethod (..))
import           Network.HTTP.Media             (MediaType)


data RoutingRespond = RoutingRespond
    { method :: StdMethod
    , contentType :: MediaType
    , statusCode :: Integer
    }
