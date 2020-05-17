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

module Citrous.Lib where

import           Citrous.Unit.Capture
import           Citrous.Unit.Impl
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (Text)
import           Data.Functor.Identity          (Identity (..))
import           Data.Proxy                     (Proxy (..))
import           GHC.TypeLits                   (KnownNat, KnownSymbol, Nat,
                                                 Symbol, symbolVal)

libMain :: IO ()
libMain = print $ match @Foo ["User", "11"]

type Hoge = "Hello" </> "hoge" </> Get '[Int] Int

type Foo = "User" </> Capture "id" Int </> Get '[] Int

type Server a = ServerT Identity a
type ServerT m a = HandlerT a m

hogeServer :: Server Foo
hogeServer n = return n

class API layout content where
  type HandlerT layout (m :: * -> *) :: *
  match :: [Text] -> Bool

instance (KnownSymbol path, API subLayout content) => API (path </> subLayout) content where
  type HandlerT (path </> subLayout) m = HandlerT subLayout m
  match []           = False
  match (first:rest) = first == symbol @path && match @subLayout @content rest

instance (FromPath cap, API subLayout content) => API (Capture path cap </> subLayout) content where
  type HandlerT (Capture path cap </> subLayout) m = cap -> HandlerT subLayout m
  match [] = False
  match (first:rest) = case fromPath @cap first of
    Just a  -> match @subLayout @content rest
    Nothing -> False

instance (KnownMethod method, KnownNat statusCode) => API (Impl method statusCode contentTypes a) content where
  type HandlerT (Impl method statusCode contentTypes a) m = m a
  match [] = True
  match _  = False

symbol :: forall k . KnownSymbol k => Text
symbol = convert $ symbolVal (Proxy :: Proxy k)

data (path :: k) </> (a :: *)
infixr 4 </>
