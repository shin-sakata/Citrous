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

import           Citrous.Integration.Handler        (Handler, runHandler)
import           Citrous.Integration.RoutingRespond (RoutingRespond)
import           Citrous.Unit.Impl                  (Impl, KnownMethod)
import           Citrous.Unit.MediaTypes            (MimeEncode, mimeEncode)
import           Citrous.Unit.RouteResult           (RouteResult (..))
import           Citrous.Unit.ServerErr             (ServerErr)
import           Data.Proxy                         (Proxy (..))
import           GHC.TypeLits                       (KnownNat, KnownSymbol,
                                                     natVal)
import           Network.HTTP.Types.Status          (mkStatus)
import           Network.Wai                        (Application, Response,
                                                     responseLBS)


data (path :: k) </> (a :: *)
infixr 4 </>

type Server api = HandlerT api Handler

-- | Handlerと型レベルルーティングを関連付ける為のクラス
class HasHandler layout args where
  type HandlerT layout (m :: * -> *) :: *
  route :: Server layout -> RouteResult ServerErr Application

-- | Implはレスポンスの実装についての型であり、
--   Handlerの戻り値と関連付けるためのinstance
instance {-# OVERLAPPABLE #-} (MimeEncode mediaType a, KnownMethod method, KnownNat status)
  => HasHandler (Impl method status '[mediaType] a) args where
  type HandlerT (Impl method status '[mediaType] a) m = m a

  route handler = Route (\req send -> send =<< do
      content <- runHandler handler
      let body = (mimeEncode @mediaType content)
      return $ responseLBS (toEnum $ nat @status) [] body
    )

nat :: forall k . KnownNat k => Int
nat = fromInteger $ natVal (Proxy :: Proxy k)


-- | 静的なパスの検証をして次に渡す
--instance (KnownSymbol path, HasHandler api context) => HasHandler (path </> api) context where
--  type HandlerT (path </> api) m = HandlerT api m
----
--  route Proxy context subserver =
--    pathRouter
--      (cs (symbolVal proxyPath))
--      (route (Proxy :: Proxy api) context subserver)
--    where proxyPath = Proxy :: Proxy path
