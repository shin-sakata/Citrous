# Citrous

### A simple, fast and type-safe Web Application Framework

(in development)

## Examples

### Hello Citrous!

|HTTP-Method|URL|Response|
|---|---|---|
|GET|localhost:8080|Hello Citrous!|

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Citrous.API
import           Network.Wai.Handler.Warp       (run)

main :: IO ()
main = run 8080 $ runRouter router

router :: Router
router = do
  route @(Get '[TextPlain] String) rootHandler

rootHandler :: Handler String
rootHandler = return "Hello Citrous!"
```

## Routing

### Static path

|HTTP-Method|URL|Response|
|---|---|---|
|GET|localhost:8080|Hello Citrous!|
|GET|localhost:8080/hello|Hello Handler!|
|GET|localhost:8080/hello/world|Hello World!|

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Citrous.API
import           Network.Wai.Handler.Warp       (run)

main :: IO ()
main = run 8080 $ runRouter router

router :: Router
router = do
  route @(Get '[TextPlain] String) rootHandler
  route @("hello" :> Get '[TextPlain] String) helloHandler
  route @("hello" :> "world" :> Get '[TextPlain] String) helloWorldHandler

rootHandler :: Handler String
rootHandler = return "Hello Citrous!"

helloHandler :: Handler String
helloHandler = return "Hello Handler!"

helloWorldHandler :: Handler String
helloWorldHandler = return "Hello World!"
```

### Extracting Query Params and Paths

|HTTP-Method|URL|Response|
|---|---|---|
|GET|localhost:8080/query?id=100|100|
|GET|localhost:8080/114514|114514|

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Citrous.API
import           Data.Aeson.TH                  (defaultOptions, deriveJSON)
import           Data.Convertible.Utf8.Internal
import           Network.Wai.Handler.Warp       (run)

main :: IO ()
main = run 8080 $ runRouter router

router :: Router
router = do
  route @("query" :> QueryParam "id" Int :> Get '[TextPlain] String) queryHandler
  route @(Capture "id" Int :> Get '[JSON] User) pathHandler

queryHandler :: Int -> Handler String
queryHandler id = return $ show id

pathHandler :: Int -> Handler String
pathHandler id = return $ show id
```

### JSON

|HTTP-Method|URL|Response|
|---|---|---|
|GET|localhost:8080/24/orange|{"age":24,"name":"orange"}|

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Citrous.API
import           Data.Aeson.TH                  (defaultOptions, deriveJSON)
import           Network.Wai.Handler.Warp       (run)

main :: IO ()
main = run 8080 $ runRouter router

router :: Router
router = do
  route @(Capture "age" Int :> Capture "name" String :> Get '[JSON] User) userHandler

userHandler :: Int -> String -> Handler User
userHandler age name = return $ User { age = age, name = name }

data User = User
    { age  :: Int
    , name :: String
    }
$(deriveJSON defaultOptions 'User)
```

### Decode the request body

|HTTP-Method|URL|BODY|Response|
|---|---|---|---|
|POST|localhost:8080/user|{"age":24,"name":"orange"}|"24才 orange"|

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Citrous.API
import           Data.Aeson.TH                  (defaultOptions, deriveJSON)
import           Network.Wai.Handler.Warp       (run)

main :: IO ()
main = run 8080 $ runRouter router

router :: Router
router = do
  route @("user" :> ReqBody '[JSON] User :> Post '[PlainText] String) userEchoHandler

userEchoHandler :: User -> Handler String
userEchoHandler user = (show $ age user) ++ "才 " ++ (name user)

data User = User
    { age  :: Int
    , name :: String
    }
$(deriveJSON defaultOptions 'User)
```