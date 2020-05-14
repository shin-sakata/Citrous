{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Citrous.Types
  ( module Citrous.Types,
    StdMethod (..),
  )
where

import Data.Data (Proxy (..))
import GHC.TypeLits (Nat)
import Network.HTTP.Types.Method
  ( Method,
    StdMethod (..),
    methodConnect,
    methodDelete,
    methodGet,
    methodHead,
    methodOptions,
    methodPatch,
    methodPost,
    methodPut,
    methodTrace,
  )

data Impl (method :: k) (statusCode :: Nat) (contentTypes :: [*]) (content :: *)

type Get = Impl 'GET 200

type Post = Impl 'POST 200

type Head = Impl 'HEAD 200

type Put = Impl 'PUT 200

type Delete = Impl 'DELETE 200

type Trace = Impl 'TRACE 200

type Connect = Impl 'CONNECT 200

type Options = Impl 'OPTIONS 200

type Patch = Impl 'PATCH 200

class KnownMethod a where
  methodVal :: Method

instance KnownMethod 'GET where
  methodVal = methodGet

instance KnownMethod 'POST where
  methodVal = methodPost

instance KnownMethod 'PUT where
  methodVal = methodPut

instance KnownMethod 'DELETE where
  methodVal = methodDelete

instance KnownMethod 'PATCH where
  methodVal = methodPatch

instance KnownMethod 'HEAD where
  methodVal = methodHead

instance KnownMethod 'OPTIONS where
  methodVal = methodOptions

instance KnownMethod 'TRACE where
  methodVal = methodTrace

instance KnownMethod 'CONNECT where
  methodVal = methodConnect
