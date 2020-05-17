{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Citrous.Integration.HasHandler
import           Citrous.Integration.Handler
import           Citrous.Unit.Impl
import           Citrous.Unit.MediaTypes
import           Data.Convertible.Utf8.Internal
import           Data.Functor.Identity          (Identity)

type API = Get '[TextPlain] Text
type NoEffHandler a = HandlerT a Identity

minimalHandler :: NoEffHandler API
minimalHandler = return "Hello Citrous!"

{-|
routes :: Routes
routes = do
  route @(Get '[TextPlain] Text) handler
-}

main :: IO ()
main = return ()
