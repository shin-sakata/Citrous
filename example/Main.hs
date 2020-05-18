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
  route @(Get '[TextPlain] String) rootHandler
  -- ^ curl localhost:8080
  -- >>> Hello Citrous!
  route @("hello" </> Get '[TextPlain] String) helloHandler
  -- ^ curl localhost:8080/hello
  -- >>> Hello Handler!
  route @("user" </> "echo" </> ReqBody '[JSON] User >>> Post '[JSON] User) userEchoHandler
  -- ^ curl localhost:8080/user/echo -d '{ "age": 24, "name": "入田 関太郎" }'
  -- >>> {"age":24,"name":"入田 関太郎"}

rootHandler :: Handler String
rootHandler = return "Hello Citrous!"

helloHandler :: Handler String
helloHandler = return "Hello Handler!"

type MinimumEffHandler = Identity
userEchoHandler :: User -> MinimumEffHandler User
userEchoHandler = return

data User = User
    { age  :: Int
    , name :: Text
    }
$(deriveJSON defaultOptions 'User)
