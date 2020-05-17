{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Citrous.API
import           Data.Aeson.TH                  (defaultOptions, deriveJSON)
import           Data.Convertible.Utf8.Internal
import           Network.Wai.Handler.Warp       (run)

main :: IO ()
main = run 8080 $ runRoutes routes

routes :: Routes
routes = do
  route @(Get '[TextPlain] Text) rootHandler
  route @(Post '[JSON] User) userHandler

rootHandler :: Handler Text
rootHandler = return "Hello Citrous!"

userHandler :: Handler User
userHandler = return $ User 24 "入田 関太郎"

data User = User
    { age  :: Int
    , name :: Text
    }
$(deriveJSON defaultOptions 'User)
