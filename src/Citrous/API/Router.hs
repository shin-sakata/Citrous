module Citrous.API.Router
  ( get
  , post
  , int
  , match
  , text
  , router
  , Routes
  , (</>)
  ) where

import Citrous.API.Action (Action)
import Data.Attoparsec.ByteString (IResult(..), Parser, feed, many1, parse, string, takeWhile1)
import Data.Attoparsec.ByteString.Char8 (char, digit)
import Data.List ((++))
import Data.Typeable (TypeRep)
import Network.HTTP.Types
import Network.Wai (Request, Response, rawPathInfo, requestMethod)
import Prelude (read)
import RIO
import RIO.Text (Text, pack)
import qualified RIO.Text as T
import RIO.Writer (Writer, execWriter, tell)
import Data.Utf8Convertible (convert)

{-|
  ルートを記述するための型
  doモナドで順番に書いていき、最初にマッチしたパスのActionをLeftとして早期にリターンすることにより、
  ルーティングを表している。
-}
type Routes = ReaderT ByteString (Either Action) ()

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

{-|
  getやpost等で生成したRoutesとRequestを元に正しいActionを返す
-}
router :: Request -> Routes -> Action
router req routes = do
  let route = requestMethod req <> rawPathInfo req
  case runReaderT routes route of
    Right _ -> undefined
    Left action -> action

{-|
  Routesの生成
-}
get :: Parser (HList a) -> Fn a Action -> Routes
get = methodPathParser "GET"

post :: Parser (HList a) -> Fn a Action -> Routes
post = methodPathParser "POST"

{-|
  http methodを引数にして、Routesを生成する
-}
methodPathParser :: ByteString -> Parser (HList a) -> Fn a Action -> Routes
methodPathParser method pathParser action = do
  let parser = match method >> apply action <$> pathParser
  route <- ask
  lift $
    case parse parser route `feed` "" of
      Done "" action -> Left action
      _ -> Right ()

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