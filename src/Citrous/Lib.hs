{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Citrous.Lib where

import           Citrous.Capture
import           Citrous.Types
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (Text)
import           Data.Proxy                     (Proxy (..))
import           GHC.TypeLits                   (KnownSymbol, Nat, Symbol,
                                                 symbolVal)

libMain :: IO ()
libMain = print $ match @Foo ["User", "11"]

type Hoge = "Hello" </> "hoge" </> Get '[Int] Int

type Foo = "User" </> Capture "id" Int </> Get '[] Int

class API layout where
  match :: [Text] -> Bool

instance (KnownSymbol path, API subLayout) => API (path </> subLayout) where
  match []             = False
  match (first:rest) = first == symbol @path && match @subLayout rest

instance (FromPath cap, API subLayout) => API (Capture path cap </> subLayout) where
  match [] = False
  match (first:rest) = case fromPath @cap first of
    Just a  -> match @subLayout rest
    Nothing -> False

instance API (Impl method statusCode contentTypes content) where
  match [] = True
  match _  = False

symbol :: forall k . KnownSymbol k => Text
symbol = convert $ symbolVal (Proxy :: Proxy k)

data (path :: k) </> (a :: *)
infixr 4 </>
