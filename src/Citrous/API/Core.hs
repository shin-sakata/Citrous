module Citrous.API.Core
  ( listenAndServe
  ) where

import           Citrous.API.Router       (Routes, router)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           RIO

app :: Routes -> Application
app routes request respond = do
  handler <- runRIO request (router request routes)
  respond handler

type Port = Int

listenAndServe :: Port -> Routes -> IO ()
listenAndServe port routes = run port $ app routes
