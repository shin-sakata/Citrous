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
{-# LANGUAGE UndecidableInstances #-}

module Citrous.Integration.HasHandler where

import Citrous.Unit.Impl (Impl, KnownMethod)
import Citrous.Unit.MediaTypes (MimeEncode)
import Citrous.Unit.Content (Context)
import Citrous.Integration.Handler (Handler)
import GHC.TypeLits (KnownSymbol, KnownNat)


data (path :: k) </> (a :: *)
infixr 4 </>

-- | Handlerと型レベルルーティングを関連付ける為のクラス
class HasHandler layout content where
  type HandlerT layout (m :: * -> *) :: *
--  route :: Context context -> Delayed env (Server api) -> Router env

type Server api = HandlerT api Handler


instance {-# OVERLAPPABLE #-} (MimeEncode mediaType a, KnownMethod method, KnownNat status) 
  => HasHandler (Impl method status '[mediaType] a) context where
  type HandlerT (Impl method status '[mediaType] a) m = m a

--  route Proxy _ = methodRouter ([],) method (Proxy :: Proxy ctypes) status
--    where method = reflectMethod (Proxy :: Proxy method)
--          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

-- | 静的なパスの検証をして次に渡す
instance (KnownSymbol path, HasHandler api context) => HasHandler (path </> api) context where

  type HandlerT (path </> api) m = HandlerT api m
--
--  route Proxy context subserver =
--    pathRouter
--      (cs (symbolVal proxyPath))
--      (route (Proxy :: Proxy api) context subserver)
--    where proxyPath = Proxy :: Proxy path

-- | Implはレスポンスの実装についての型であり、
--   Handlerの戻り値と関連付けるためのinstance
instance (KnownMethod method, MimeEncode mediaType content)
  => HasHandler (Impl method statusCode '[mediaType] content) content where
  type HandlerT (Impl method statusCode '[mediaType] content) m = m content
