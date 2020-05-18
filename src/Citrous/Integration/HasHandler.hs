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

import           Citrous.Integration.Handler (Handler, Handler', runHandler,
                                              runHandler')
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


-- | Handlerと型レベルルーティングを関連付ける為のクラス
class HasHandler layout h where
  type HandlerT layout (h :: * -> *) :: *
  route :: HandlerT layout h -> Routes

data (path :: k) >>> (a :: *)
infixr 3 >>>

instance (MimeDecode mediaType a, HasHandler layout h, Handler' h) =>
  HasHandler (ReqBody '[mediaType] a >>> layout ) h where
  type HandlerT (ReqBody '[mediaType] a >>> layout) h = a -> HandlerT layout h
  route handler = do
    req <- ask
    body <- liftIO $ getRequestBodyChunk req
    case mimeDecode @mediaType @a body of
      Right decodedBody -> route @layout @h (handler decodedBody)
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
    if requestMethod req == method
      then earlyReturnRoute $ responseLBS status [] <$> body
      else tell $ badMethod $ methodStdVal @method
    where
      body = return $ mimeEncode @mediaType $ runHandler' handler
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
