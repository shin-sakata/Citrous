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

import Citrous.Unit.Impl (Impl, KnownMethod)
import Citrous.Unit.MediaTypes (MimeEncode)

data (path :: k) </> (a :: *)
infixr 4 </>

-- | Handlerと型レベルルーティングを関連付ける為のクラス
class HasHandler layout content where
  type HandlerT layout (m :: * -> *) :: *

-- | Implはレスポンスの実装についての型であり、
--   Handlerの戻り値と関連付けるためのinstance
instance (KnownMethod method, MimeEncode mediaType content)
  => HasHandler (Impl method statusCode '[mediaType] content) content where
  type HandlerT (Impl method statusCode '[mediaType] content) m = m content
