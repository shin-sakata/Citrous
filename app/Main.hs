module Main where

import Citrous.API
import Data.Text (Text)

main :: IO ()
main = listenAndServe 8080 routes

routes :: Routes
routes = do
  get (match "/") topAction
  get (match "/hello" </> text) helloAction

topAction :: Action
topAction = textPlain "Hello Citrous!!"

helloAction :: Text -> Action
helloAction name = textPlain ("Hello " <> name <> "!!")
