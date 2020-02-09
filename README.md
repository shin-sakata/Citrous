# Citrous

### Easy, Fast and Type-Safe Web Application Framework

(in development)

## Examples

### text/plain

localhost:8080          --> "Hello Citrous!!"  
localhost:8080/yourName --> "Hello yourName!!"

```haskell
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
```

### application/json

localhost:8080/echoUser/20/Citrous ---> `{"age":20,"name":"Citrous"}`

```haskell
import Citrous.API
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

main :: IO ()
main = listenAndServe 8080 routes

routes :: Routes
routes = do
  get (match "/echoUser" </> int </> text) echoUserAction

echoUserAction :: Int -> Text -> Action
echoUserAction age name = json $ User age name

data User =
  User
    { age  :: Int
    , name :: Text
    }
  deriving (Generic, Show)
instance ToJSON User
instance FromJSON User
```
