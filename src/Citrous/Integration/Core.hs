module Citrous.Integration.Core
  ( listenAndServe
  ) where

import           Citrous.Unit.Application (ToApplication(..))
import           Citrous.Unit.Router      (Routes, runRoutes)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (Port, run)

app :: ToApplication a => Routes a -> Application
app routes request = toApplication (runRoutes routes request) request

listenAndServe :: ToApplication a => Port -> Routes a -> IO ()
listenAndServe port routes = run port $ app routes
