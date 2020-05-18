{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Citrous.API
import           Control.Monad.Identity         (Identity)
import           Data.Aeson.TH                  (defaultOptions, deriveJSON)
import           Data.Convertible.Utf8.Internal
import           Network.Wai.Handler.Warp       (run)

main :: IO ()
main = run 8080 $ runRoutes routes

routes :: Routes
routes = do
  route @(Get '[TextPlain] Text) rootHandler
  -- ^ curl localhost:8080
  -- >>> Hello Citrous!
  route @(ReqBody '[JSON] User >>> Post '[JSON] User) userEchoHandler
  -- ^ curl localhost:8080 -d '{ "age": 24, "name": "入田 関太郎" }'
  -- >>> {"age":24,"name":"入田 関太郎"}

rootHandler :: Handler Text
rootHandler = return "Hello Citrous!"

type MinimumEffHandler = Identity

userEchoHandler :: User -> MinimumEffHandler User
userEchoHandler user = return user

data User = User
    { age  :: Int
    , name :: Text
    }
$(deriveJSON defaultOptions 'User)
