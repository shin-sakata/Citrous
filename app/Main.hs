{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Main where

import           Data.Proxy   (Proxy (..), Proxy)
import           GHC.TypeLits (KnownSymbol, symbolVal)


main :: IO ()
main = print $ match @HelloAPI ["hello", "world"]

type HelloAPI = "hello" </> "world" </> Int

class API layout where
  match :: [String] -> Bool

instance API Int where
  match [] = True
  match _ = False

instance KnownSymbol path => API (Proxy path) where
  match [x] = x == symbol @path
  match [] = False

instance (KnownSymbol path, API subLayout) => API (path </> subLayout) where
  match [] = False
  match (first : rest) = first == symbol @path && match @subLayout rest

symbol :: forall k . KnownSymbol k => String
symbol = symbolVal (Proxy :: Proxy k)

data (path :: k) </> (a :: *)
infixr 4 </>

class ResponsePart response where
  respond :: [String] -> Bool

{-|
routes :: Route
routes = do
  route @("" >>> JSON User) userHandler
  route @("user" <//> Cap Int <?> "hoge" >>> JSON User) userHandler
-}