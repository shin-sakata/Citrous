{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Citrous.Integration.HasHandler where

import           Citrous.Integration.Handler    (MonadHandler, runMonadHandler)
import           Citrous.Integration.Router     (Router, RouterEnv (..),
                                                 RoutingErr (..),
                                                 earlyReturnRoute,
                                                 methodNotAllowed, request)
import           Citrous.Unit.Args
import           Citrous.Unit.Capture           (Capture)
import           Citrous.Unit.Extractor         (Extractor, extract)
import           Citrous.Unit.Impl              (Impl, KnownMethod,
                                                 methodStdVal, methodVal)
import           Citrous.Unit.MediaTypes        (MimeDecode, MimeEncode,
                                                 mimeDecode, mimeEncode)
import           Citrous.Unit.QueryParam        (QueryParam)
import           Citrous.Unit.RequestBody       (ReqBody)
import           Citrous.Unit.ServerErr         (ServerErr, err405,
                                                 responseServerError)
import           Control.Monad                  (join)
import           Control.Monad.Error.Class      (throwError)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (ask, asks, local)
import           Control.Monad.Writer           (tell)
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (LazyByteString, Text)
import           Data.Proxy                     (Proxy (..))
import           Debug.Trace                    (trace)
import           GHC.TypeLits                   (KnownNat, KnownSymbol, natVal,
                                                 symbolVal)
import           Network.HTTP.Types.Status      (mkStatus)
import           Network.Wai                    (Application, Request, Response,
                                                 getRequestBodyChunk, pathInfo,
                                                 queryString, requestMethod,
                                                 responseLBS)

data (path :: k) :> (a :: *)

infixr 4 :>

-- | Handlerと型レベルルーティングを関連付ける為のクラス
class HasHandler layout h env where
  type HandlerT layout (h :: * -> *) env :: *
  route :: HandlerT layout h env -> Router env

-- | Implはレスポンスの実装についての型であり、
--   Handlerの戻り値と関連付けるためのinstance
instance
  {-# OVERLAPPABLE #-}
  (MimeEncode mediaType a, KnownMethod method, KnownNat status, MonadHandler h env) =>
  HasHandler (Impl method status '[mediaType] a) h env
  where
  type HandlerT (Impl method status '[mediaType] a) h env = h a

  route handler = do
    req <- asks request
    env <- asks state
    if not (null $ pathInfo req)
      then tell NotFound
      else
        if requestMethod req == method
          then earlyReturnRoute $ responseLBS status [] <$> body env
          else tell $ methodNotAllowed $ methodStdVal @method
      where
        body env = mimeEncode @mediaType <$> runMonadHandler @h @env env handler
        status = toEnum $ nat @status
        method = methodVal @method



-- | query paramをキャプチャ
instance
  (KnownSymbol queryName, HasHandler next h env, Extractor arg) =>
  HasHandler (QueryParam queryName arg :> next) h env
  where
  type HandlerT (QueryParam queryName arg :> next) h env = arg -> HandlerT next h env

  route handler = do
    query <- asks (queryString . request)
    let maybeParam = join $ lookup (convert $ symbol @queryName) query
    case join $ extract @arg <$> maybeParam of
      Nothing -> tell BadRequest
      Just a  -> route @next @h (handler a)

-- | Pathをキャプチャしてhandlerに渡す
instance
  (KnownSymbol pathName, HasHandler next h env, Extractor arg) =>
  HasHandler (Capture pathName arg :> next) h env
  where
  type HandlerT (Capture pathName arg :> next) h env = arg -> HandlerT next h env

  route handler = do
    path <- asks (pathInfo . request)
    if null path
      then tell NotFound
      else
        ( case extract @arg $ convert $ head path of
            Just arg -> local tailPathInfoEnv $ route @next @h (handler arg)
            Nothing  -> tell NotFound
        )

-- | 静的な文字列のパスを分解する
instance (KnownSymbol path, HasHandler next h env) => HasHandler (path :> next) h env where
  type HandlerT (path :> next) h env = HandlerT next h env

  route handler = do
    path <- asks (pathInfo . request)
    if not (null path) && (head path == symbol @path)
      then local tailPathInfoEnv $ route @next @h handler
      else tell NotFound

-- | unsafe
tailPathInfo :: Request -> Request
tailPathInfo req = req {pathInfo = tail $ pathInfo req}

tailPathInfoEnv :: RouterEnv a -> RouterEnv a
tailPathInfoEnv e = e { request = tailPathInfo $ request e }

-- | ReqBodyをdecodeしてHandlerに渡す
instance
  (MimeDecode mediaType a, HasHandler next h env, MonadHandler h env) =>
  HasHandler (ReqBody '[mediaType] a :> next) h env
  where
  type HandlerT (ReqBody '[mediaType] a :> next) h env = a -> HandlerT next h env
  route handler = do
    req <- asks request
    body <- liftIO $ getRequestBodyChunk req
    case mimeDecode @mediaType @a body of
      Right decodedBody -> route @next @h (handler decodedBody)
      Left err          -> tell BadRequest

nat :: forall k. KnownNat k => Int
nat = fromInteger $ natVal (Proxy :: Proxy k)

symbol :: forall k. KnownSymbol k => Text
symbol = convert $ symbolVal (Proxy :: Proxy k)
