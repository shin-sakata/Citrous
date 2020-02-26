module Citrous.Integration.Core
  ( runCitrous
  , runCitrousEnv
  , runCitrousSettings
  , runCitrousSocket
  , Port
  ) where

import           Citrous.Unit.Application (ToApplication (..))
import           Citrous.Unit.Router      (Routes, runRoutes)
import           Network.Socket           (Socket)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (Port, Settings, run, runEnv,
                                           runSettings, runSettingsSocket)

runCitrous :: ToApplication a => Port -> a -> IO ()
runCitrous port = run port . toApplication

runCitrousEnv :: ToApplication a => Port -> a -> IO ()
runCitrousEnv port = runEnv port . toApplication 

runCitrousSettings :: ToApplication a => Settings -> a -> IO ()
runCitrousSettings settings = runSettings settings . toApplication

runCitrousSocket :: ToApplication a => Settings -> Socket -> a -> IO ()
runCitrousSocket settings socket = runSettingsSocket settings socket . toApplication
