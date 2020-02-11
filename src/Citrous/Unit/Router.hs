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

import Data.Attoparsec.ByteString (Parser, endOfInput, many1, parseOnly, string, takeWhile1)
import Data.Attoparsec.ByteString.Char8 (char, digit)
import Data.Utf8Convertible (convert)
import Network.Wai (Request, rawPathInfo, requestMethod)
import RIO
import Citrous.Unit.Application (ToApplication(..))

{-|
  ルートを記述するための型
  doモナドで順番に書いていき、最初にマッチしたパスのActionをLeftとして早期にリターンすることにより、
  ルーティングを表している。
-}
type Routes a = ReaderT ByteString (Either a) ()

instance (ToApplication a) => ToApplication (Routes a) where
  toApplication routes req = toApplication (runRoutes routes req) req

{-|
  getやpost等で生成したRoutesとRequestを元に関数を返す
-}
runRoutes :: Routes a -> Request -> a
runRoutes routes req = do
  let route = requestMethod req <> rawPathInfo req
  case runReaderT routes route of
    Right _ -> undefined
    Left action -> action

{-|
  Routesの生成
-}
get :: Parser (HList a) -> Fn a t -> Routes t
get = methodPathParser "GET"

post :: Parser (HList a) -> Fn a t -> Routes t
post = methodPathParser "POST"

{-|
  必ずRouteを返す関数
-}
absolute :: a -> Routes a
absolute = lift . Left

{-|
  http methodを引数にして、Routesを生成する
-}
methodPathParser :: ByteString -> Parser (HList a) -> Fn a t -> Routes t
methodPathParser method pathParser action = do
  let parser = match method >> apply action <$> (pathParser <* endOfInput)
  route <- ask
  lift $
    case parseOnly parser route of
      Right action -> Left action
      Left _ -> Right ()

{-|
  パスからIntを取り出す
-}
int :: Parser (HList '[ Int])
int = do
  i <- read <$> many1 digit
  return $ i ::: HNil

{-|
  パスからTextを取り出す
-}
text :: Parser (HList '[ Text])
text = do
  str <- convert <$> takeWhile1 (/= 47)
  return $ str ::: HNil

{-|
  文字列にマッチさせて値は返さない
-}
match :: ByteString -> Parser (HList '[])
match txt = do
  string txt
  return HNil

{-|
  ルーターのパスから取得する値を型レベルで保持しておくヘテロリスト
-}
data HList :: [*] -> * where
  HNil :: HList '[]
  (:::) :: a -> HList xs -> HList (a ': xs)

infixr 5 :::

{-|
  ヘテロリストを関数に適用するための型レベル関数

  HList '[Int, Text, String] -> (Int -> Text -> String -> Foo) -> Foo
  これを動かすことができる
-}
type family Fn (as :: [*]) r

type instance Fn '[] r = r

type instance Fn (x ': xs) r = x -> Fn xs r

{-|
  ヘテロリストの連結後の型を生成する型レベル関数
-}
type family (ls :: [k]) ++ (rs :: [k]) :: [k]

type instance '[] ++ ys = ys

type instance (x ': xs) ++ ys = x ': (xs ++ ys)

{-|
  ヘテロリストの連結
-}
hAppend :: HList xs -> HList ys -> HList (xs ++ ys)
hAppend HNil ys = ys
hAppend (x1 ::: HNil) ys = x1 ::: ys

{-|
  ヘテロリストレベルのパーサーの結合
-}
(</>) :: Parser (HList xs) -> Parser (HList ys) -> Parser (HList (xs ++ ys))
(</>) pl pr = do
  l <- pl
  char '/'
  r <- pr
  return $ hAppend l r

infixr 5 </>

{-|
  ヘテロリストの関数適用する
-}
apply :: Fn xs r -> HList xs -> r
apply v HNil = v
apply f (a ::: as) = apply (f a) as
