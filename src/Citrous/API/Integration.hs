module Citrous.API.Integration
  ( listenAndServe
  ) where

import           Citrous.API.Action       (Action, runAction)
import           Citrous.API.Router       (Routes, runRoutes)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (Port, run)

app :: Routes Action -> Application
app routes request respond = do
  response <- runAction request (runRoutes routes request)
  respond response

listenAndServe :: Port -> Routes Action -> IO ()
listenAndServe port routes = run port $ app routes
