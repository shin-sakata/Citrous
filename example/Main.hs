{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Citrous.API
import           Control.Monad.Identity         (Identity)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (ReaderT)
import           Control.Monad.RWS.Class        (ask)
import           Data.Aeson.TH                  (defaultOptions, deriveJSON)
import           Data.Convertible.Utf8.Internal
import           Data.IORef                     (IORef, atomicModifyIORef',
                                                 newIORef)
import           Network.Wai.Handler.Warp       (run)

main :: IO ()
main = do
  state <- newIORef 0
  run 8080 $ runRouterWithState state router

router :: Router (IORef Int)
router = do
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
  route @("reader" :> Get '[TextPlain] Int) handler

rootHandler :: Identity String
rootHandler = return "Hello Citrous!"

helloHandler :: Identity String
helloHandler = return "Hello Handler!"

queryHandler :: Int -> Identity String
queryHandler i = return $ show i

userEchoHandler :: User -> Identity User
userEchoHandler = return

createUserHandler :: Int -> Text -> Identity User
createUserHandler age name = return $ User { age = age, name = name }

handler :: ReaderT (IORef Int) IO Int
handler = do
  state <- ask
  liftIO $ atomicModifyIORef' state (\ s -> (s + 1, s))

data User = User
    { age  :: Int
    , name :: Text
    }
$(deriveJSON defaultOptions 'User)
