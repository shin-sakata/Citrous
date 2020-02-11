module Main where

import Citrous.API
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

main :: IO ()
main = listenAndServe 8080 routes

routes :: Routes Action
routes = do
  get (match "/") topAction
  get (match "/hello" </> text) helloAction
  get (match "/echoUser" </> text </> int) echoUserAction

topAction :: Action
topAction = textPlain "Hello Citrous!!"

helloAction :: Text -> Action
helloAction name = textPlain ("Hello " <> name <> "!!")

echoUserAction :: Text -> Int -> Action
echoUserAction name age = json $ User name age

data User =
  User
    { name :: Text
    , age  :: Int
    }
  deriving (Generic, Show)
instance ToJSON User
instance FromJSON User
