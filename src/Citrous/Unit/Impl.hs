{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Citrous.Unit.Impl
  ( module Citrous.Unit.Impl,
    StdMethod (..),
  )
where

import           Data.Data                 (Proxy (..))
import           GHC.TypeLits              (Nat)
import           Network.HTTP.Types.Method (Method, StdMethod (..),
                                            methodConnect, methodDelete,
                                            methodGet, methodHead,
                                            methodOptions, methodPatch,
                                            methodPost, methodPut, methodTrace)

data Impl (method :: k) (statusCode :: Nat) (mediaTypes :: [*]) (content :: *)

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
  methodStdVal :: StdMethod

instance KnownMethod 'GET where
  methodVal = methodGet
  methodStdVal = GET

instance KnownMethod 'POST where
  methodVal = methodPost
  methodStdVal = POST

instance KnownMethod 'PUT where
  methodVal = methodPut
  methodStdVal = PUT

instance KnownMethod 'DELETE where
  methodVal = methodDelete
  methodStdVal = DELETE

instance KnownMethod 'PATCH where
  methodVal = methodPatch
  methodStdVal = PATCH

instance KnownMethod 'HEAD where
  methodVal = methodHead
  methodStdVal = HEAD

instance KnownMethod 'OPTIONS where
  methodVal = methodOptions
  methodStdVal = OPTIONS

instance KnownMethod 'TRACE where
  methodVal = methodTrace
  methodStdVal = TRACE

instance KnownMethod 'CONNECT where
  methodVal = methodConnect
  methodStdVal = CONNECT

