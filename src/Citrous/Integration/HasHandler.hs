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

module Citrous.Integration.HasHandler where

import           Citrous.Integration.Handler    (Handler, Handler', runHandler,
                                                 runHandler')
import           Citrous.Integration.Routes     (Routes, RoutingErr (..),
                                                 badMethod, earlyReturnRoute)
import           Citrous.Unit.Args
import           Citrous.Unit.Capture           (Capture)
import           Citrous.Unit.Extractor         (Extractor, extract)
import           Citrous.Unit.Impl              (Impl, KnownMethod,
                                                 methodStdVal, methodVal)
import           Citrous.Unit.MediaTypes        (MimeDecode, MimeEncode,
                                                 mimeDecode, mimeEncode)
import           Citrous.Unit.QueryParam        (QueryParam)
import           Citrous.Unit.RequestBody       (ReqBody)
import           Citrous.Unit.ServerErr         (ServerErr, err405)
import           Control.Monad                  (join)
import           Control.Monad.Error.Class      (throwError)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (ask, asks, local)
import           Control.Monad.Writer           (tell)
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (Text)
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
class HasHandler layout h where
  type HandlerT layout (h :: * -> *) :: *
  route :: HandlerT layout h -> Routes

-- | query paramをキャプチャ
instance
  (KnownSymbol queryName, HasHandler next h, Extractor arg) =>
  HasHandler (QueryParam queryName arg :> next) h
  where
  type HandlerT (QueryParam queryName arg :> next) h = arg -> HandlerT next h

  route handler = do
    query <- asks queryString
    let maybeParam = join $ lookup (convert $ symbol @queryName) query
    case join $ extract @arg <$> maybeParam of
      Nothing -> tell NotFound
      Just a  -> route @next @h (handler a)

-- | Pathをキャプチャしてhandlerに渡す
instance
  (KnownSymbol pathName, HasHandler next h, Extractor arg) =>
  HasHandler (Capture pathName arg :> next) h
  where
  type HandlerT (Capture pathName arg :> next) h = arg -> HandlerT next h

  route handler = do
    path <- asks pathInfo
    if null path
      then tell NotFound
      else
        ( case extract @arg $ convert $ head path of
            Just arg -> local tailPathInfo $ route @next @h (handler arg)
            Nothing  -> tell NotFound
        )

-- | 静的な文字列のパスを分解する
instance (KnownSymbol path, HasHandler next h) => HasHandler (path :> next) h where
  type HandlerT (path :> next) h = HandlerT next h

  route handler = do
    path <- asks pathInfo
    if not (null path) && (head path == symbol @path)
      then local tailPathInfo $ route @next @h handler
      else tell NotFound

-- | unsafe
tailPathInfo :: Request -> Request
tailPathInfo req = req {pathInfo = tail $ pathInfo req}

-- | ReqBodyをdecodeしてHandlerに渡す
instance
  (MimeDecode mediaType a, HasHandler next h, Handler' h) =>
  HasHandler (ReqBody '[mediaType] a :> next) h
  where
  type HandlerT (ReqBody '[mediaType] a :> next) h = a -> HandlerT next h
  route handler = do
    req <- ask
    body <- liftIO $ getRequestBodyChunk req
    case mimeDecode @mediaType @a body of
      Right decodedBody -> route @next @h (handler decodedBody)
      Left err          -> tell BadRequest

-- | Implはレスポンスの実装についての型であり、
--   Handlerの戻り値と関連付けるためのinstance
instance
  {-# OVERLAPPABLE #-}
  (MimeEncode mediaType a, KnownMethod method, KnownNat status, Handler' h) =>
  HasHandler (Impl method status '[mediaType] a) h
  where
  type HandlerT (Impl method status '[mediaType] a) h = h a

  route handler = do
    req <- ask
    if not (null $ pathInfo req)
      then tell NotFound
      else
        if requestMethod req == method
          then earlyReturnRoute $ responseLBS status [] <$> body
          else tell $ badMethod $ methodStdVal @method
    where
      body = return $ mimeEncode @mediaType $ runHandler' handler
      status = toEnum $ nat @status
      method = methodVal @method

nat :: forall k. KnownNat k => Int
nat = fromInteger $ natVal (Proxy :: Proxy k)

symbol :: forall k. KnownSymbol k => Text
symbol = convert $ symbolVal (Proxy :: Proxy k)
