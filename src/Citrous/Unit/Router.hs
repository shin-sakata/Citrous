module Citrous.Unit.Router
  ( get
  , post
  , int
  , match
  , text
  , runRoutes
  , Routes
  , absolute
  , (</>)
  ) where

import           Citrous.Unit.Application (ToApplication(..))
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.Trans (lift)
import           Data.Attoparsec.ByteString (Parser, endOfInput, many1, parseOnly, string, takeWhile1)
import           Data.Attoparsec.ByteString.Char8 (char, digit)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Utf8Convertible (convert)
import           Network.Wai (Request, rawPathInfo, requestMethod)

{-| Represent routing by returning the first matching Handler by the Either monad
-}
type Routes a = ReaderT BS.ByteString (Either a) ()

instance (ToApplication a) => ToApplication (Routes a) where
  toApplication routes req = toApplication (runRoutes routes req) req

{-| Resolve Routes
-}
runRoutes :: Routes a -> Request -> a
runRoutes routes req = do
  let route = requestMethod req <> rawPathInfo req
  case runReaderT routes route of
    Right _ -> undefined
    Left action -> action

{-| Generate GET Routes
-}
get :: Parser (HList a) -> Fn a t -> Routes t
get = methodPathParser "GET"

{-| Generate POST Routes
-}
post :: Parser (HList a) -> Fn a t -> Routes t
post = methodPathParser "POST"

{-| Always generate Routes
-}
absolute :: a -> Routes a
absolute = lift . Left

{-| Generate Routes
-}
methodPathParser :: ByteString -> Parser (HList a) -> Fn a t -> Routes t
methodPathParser method pathParser action = do
  let parser = match method >> apply action <$> (pathParser <* endOfInput)
  route <- ask
  lift $
    case parseOnly parser route of
      Right action -> Left action
      Left _ -> Right ()

{-| Pick Int from path
-}
int :: Parser (HList '[ Int])
int = do
  i <- read <$> many1 digit
  return $ i ::: HNil

{-| Pick Text from path
-}
text :: Parser (HList '[ Text])
text = do
  str <- convert <$> takeWhile1 (/= BS.head "/")
  return $ str ::: HNil

{-| Matches a Bytestring but does not return a value
-}
match :: ByteString -> Parser (HList '[])
match txt = do
  string txt
  return HNil

{-| Type Level List (Heterogeneous List)
-}
data HList :: [*] -> * where
  HNil :: HList '[]
  (:::) :: a -> HList xs -> HList (a ': xs)

infixr 5 :::

{-| Function Type for applying HList to function

    Example :
    Fn '[Int, Text] Bool == Int -> Text -> Bool
-}
type family Fn (as :: [*]) r

type instance Fn '[] r = r

type instance Fn (x ': xs) r = x -> Fn xs r

{-| TypeFamily to represent the type after appending of HList
-}
type family (ls :: [k]) ++ (rs :: [k]) :: [k]

type instance '[] ++ ys = ys

type instance (x ': xs) ++ ys = x ': (xs ++ ys)

{-| Append HList
-}
hAppend :: HList xs -> HList ys -> HList (xs ++ ys)
hAppend HNil ys = ys
hAppend (x1 ::: HNil) ys = x1 ::: ys

{-| Append HList Parser
-}
(</>) :: Parser (HList xs) -> Parser (HList ys) -> Parser (HList (xs ++ ys))
(</>) pl pr = do
  l <- pl
  char '/'
  r <- pr
  return $ hAppend l r

infixr 5 </>

{-| Apply HList to function
-}
apply :: Fn xs r -> HList xs -> r
apply v HNil = v
apply f (a ::: as) = apply (f a) as