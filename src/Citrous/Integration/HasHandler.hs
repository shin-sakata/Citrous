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

import           Citrous.Integration.Handler (Handler, runHandler)
import           Citrous.Integration.Routes  (Routes, RoutingErr (..),
                                              badMethod, earlyReturnRoute)
import           Citrous.Unit.Args
import           Citrous.Unit.Impl           (Impl, KnownMethod, methodStdVal,
                                              methodVal)
import           Citrous.Unit.MediaTypes     (MimeDecode, MimeEncode,
                                              mimeDecode, mimeEncode)
import           Citrous.Unit.RequestBody    (ReqBody)
import           Citrous.Unit.ServerErr      (ServerErr, err405)
import           Control.Monad.Error.Class   (throwError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask, asks)
import           Control.Monad.Writer        (tell)
import           Data.Proxy                  (Proxy (..))
import           GHC.TypeLits                (KnownNat, KnownSymbol, natVal)
import           Network.HTTP.Types.Status   (mkStatus)
import           Network.Wai                 (Application, Response,
                                              getRequestBodyChunk,
                                              requestMethod, responseLBS)

data (path :: k) </> (a :: *)
infixr 4 </>


type Server api = HandlerT api Handler

-- | Handlerと型レベルルーティングを関連付ける為のクラス
class HasHandler layout args where
  type HandlerT layout (m :: * -> *) :: *
  route :: Server layout -> Routes

data (path :: k) >>> (a :: *)
infixr 3 >>>

instance (MimeDecode mediaType a, HasHandler api args) =>
  HasHandler (ReqBody '[mediaType] a >>> api ) args where
  type HandlerT (ReqBody '[mediaType] a >>> api) m = a -> HandlerT api m
  route handler = do
    req <- ask
    body <- liftIO $ getRequestBodyChunk req
    case mimeDecode @mediaType @a body of
      Right decodedBody -> route @api @args (handler decodedBody)
      Left err          -> tell BadRequest


-- | Implはレスポンスの実装についての型であり、
--   Handlerの戻り値と関連付けるためのinstance
instance
  {-# OVERLAPPABLE #-}
  (MimeEncode mediaType a, KnownMethod method, KnownNat status) =>
  HasHandler (Impl method status '[mediaType] a) args
  where
  type HandlerT (Impl method status '[mediaType] a) m = m a

  route handler = do
    req <- ask
    if requestMethod req == method
      then earlyReturnRoute $ responseLBS status [] <$> body
      else tell $ badMethod $ methodStdVal @method
    where
      body = mimeEncode @mediaType <$> runHandler handler
      status = toEnum $ nat @status
      method = methodVal @method

nat :: forall k. KnownNat k => Int
nat = fromInteger $ natVal (Proxy :: Proxy k)

-- | 静的なパスの検証をして次に渡す
-- instance (KnownSymbol path, HasHandler api context) => HasHandler (path </> api) context where
--  type HandlerT (path </> api) m = HandlerT api m
----
--  route Proxy context subserver =
--    pathRouter
--      (cs (symbolVal proxyPath))
--      (route (Proxy :: Proxy api) context subserver)
--    where proxyPath = Proxy :: Proxy path
