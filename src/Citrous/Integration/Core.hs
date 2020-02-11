module Citrous.Integration.Core
  ( listenAndServe
  ) where

import           Citrous.Unit.Action       (Action, runAction)
import           Citrous.Unit.Router       (Routes, runRoutes)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (Port, run)

app :: Routes Action -> Application
app routes request respond = do
  response <- runAction request (runRoutes routes request)
  respond response

listenAndServe :: Port -> Routes Action -> IO ()
listenAndServe port routes = run port $ app routes
