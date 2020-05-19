{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Citrous.API
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
  route @("hello" :> Get '[TextPlain] String) helloHandler
  -- ^ curl localhost:8080/hello
  -- >>> Hello Handler!
  route @("query" :> QueryParam "hogehoge" Int :> Get '[TextPlain] String) queryHandler
  -- ^ curl localhost:8080/query
  -- >>> Hello Handler!
  route @("user" :> "echo" :> ReqBody '[JSON] User :> Post '[JSON] User) userEchoHandler
  -- ^ curl localhost:8080/user/echo -d '{ "age": 24, "name": "入田 関太郎" }'
  -- >>> {"age":24,"name":"入田 関太郎"}
  route @(Capture "age" Int :> Capture "name" Text :> Get '[JSON] User) createUserHandler
  -- ^ curl localhost:8080/24/orange
  -- >>> {"age":24,"name":"orange"}

rootHandler :: Handler String
rootHandler = return "Hello Citrous!"

helloHandler :: Handler String
helloHandler = return "Hello Handler!"

queryHandler :: Int -> Handler String
queryHandler i = return $ show i

userEchoHandler :: User -> Handler User
userEchoHandler = return

createUserHandler :: Int -> Text -> Handler User
createUserHandler age name = return $ User { age = age, name = name }

data User = User
    { age  :: Int
    , name :: Text
    }
$(deriveJSON defaultOptions 'User)
