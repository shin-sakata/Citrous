{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Citrous.Integration.Handler
import           Citrous.Integration.HasHandler
import           Citrous.Integration.Routes     (Routes, runRoutes)
import           Citrous.Unit.Impl
import           Citrous.Unit.MediaTypes
import           Data.Convertible.Utf8.Internal
import           Data.Functor.Identity          (Identity)
import           Network.Wai.Handler.Warp       (run)

routes :: Routes
routes = do
  route @(Get '[TextPlain] Text) rootHandler
  route @(Post '[TextPlain] String) htmlHandler

rootHandler :: Handler Text
rootHandler = return "Hello Citrous!"

htmlHandler :: Handler String
htmlHandler = return "<h1>String</h1>"

main :: IO ()
main = run 8080 $ runRoutes routes
