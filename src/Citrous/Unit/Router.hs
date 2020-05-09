module Citrous.Unit.Router
  ( get,
    post,
    head,
    put,
    delete,
    trace,
    connect,
    options,
    patch,
    int,
    match,
    text,
    runRoutes,
    Routes,
    absolute,
    (</>),
  )
where

import           Citrous.Unit.Application         (ToApplication (..))
import           Citrous.Unit.ServerErr           (err404, responseServerError)
import           Control.Monad.Error.Class        (throwError)
import           Control.Monad.Reader             (ReaderT, ask, runReaderT)
import           Control.Monad.Trans              (lift)
import           Data.Attoparsec.ByteString       (Parser, endOfInput, many1,
                                                   parseOnly, string,
                                                   takeWhile1)
import           Data.Attoparsec.ByteString.Char8 (char, digit)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.Extensible                  (type (>:))
import           Data.Extensible.Effect           (Eff, leaveEff, liftEff)
import           Data.Extensible.Effect.Default
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Utf8Convertible             (convert)
import           Network.HTTP.Types               (Method, methodConnect,
                                                   methodDelete, methodGet,
                                                   methodHead, methodOptions,
                                                   methodPatch, methodPost,
                                                   methodPut, methodTrace)
import           Network.Wai                      (Application, Request,
                                                   rawPathInfo, requestMethod)
import           Prelude                          hiding (head)

-- | Represent routing by returning the first matching Handler by the Either monad
type Routes a = Eff '[ReaderDef BS.ByteString, EitherDef a] ()

runRoutesEff :: Routes a -> BS.ByteString -> Either a ()
runRoutesEff route = leaveEff . runEitherDef . runReaderDef route

-- | To achieve an early return, the value goes into the Left of Either
earlyReturn :: a -> Routes a
earlyReturn = throwError

-- | Resolve Routes
runRoutes :: Routes a -> Request -> Maybe a
runRoutes routes req = do
  let route = requestMethod req <> rawPathInfo req
  case runRoutesEff routes route of
    Right _     -> Nothing
    Left action -> Just action

instance ToApplication a => ToApplication (Routes a) where
  toApplication routes req = maybe defaultNotFound toApplication (runRoutes routes req) req

defaultNotFound :: Application
defaultNotFound req respond = respond $ responseServerError err404

-- | Generate Routes
get, post, head, put, delete, trace, connect, options, patch :: Parser (HList xs) -> Fn xs r -> Routes r
get = routesParser methodGet
post = routesParser methodPost
head = routesParser methodHead
put = routesParser methodPut
delete = routesParser methodDelete
trace = routesParser methodTrace
connect = routesParser methodConnect
options = routesParser methodOptions
patch = routesParser methodPatch

-- | Always generate Routes
absolute :: a -> Routes a
absolute = earlyReturn

-- | Generate Routes
routesParser :: Method -> Parser (HList xs) -> Fn xs r -> Routes r
routesParser method pathParser action = do
  let parser = match method >> apply action <$> (pathParser <* endOfInput)
  route <- ask
  case parseOnly parser route of
    Right action -> earlyReturn action
    Left _       -> return ()

-- | Pick Int from path
int :: Parser (HList '[Int])
int = do
  i <- read <$> many1 digit
  return $ i ::: HNil

-- | Pick Text from path
text :: Parser (HList '[Text])
text = do
  str <- convert <$> takeWhile1 (/= BS.head "/")
  return $ str ::: HNil

-- | Matches a Bytestring but does not return a value
match :: ByteString -> Parser (HList '[])
match txt = do
  string txt
  return HNil

-- | Type Level List (Heterogeneous List)
data HList :: [*] -> * where
  HNil :: HList '[]
  (:::) :: a -> HList xs -> HList (a ': xs)

infixr 5 :::

-- | Function Type for applying HList to function
--
--    Example :
--    Fn '[Int, Text] Bool == Int -> Text -> Bool
type family Fn (as :: [*]) r

type instance Fn '[] r = r

type instance Fn (x ': xs) r = x -> Fn xs r

-- | TypeFamily to represent the type after appending of HList
type family (ls :: [k]) ++ (rs :: [k]) :: [k]

type instance '[] ++ ys = ys

type instance (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Append HList
hAppend :: HList xs -> HList ys -> HList (xs ++ ys)
hAppend HNil ys          = ys
hAppend (x1 ::: HNil) ys = x1 ::: ys

-- | Append HList Parser
(</>) :: Parser (HList xs) -> Parser (HList ys) -> Parser (HList (xs ++ ys))
(</>) pl pr = do
  l <- pl
  char '/'
  r <- pr
  return $ hAppend l r

infixr 5 </>

-- | Apply HList to function
apply :: Fn xs r -> HList xs -> r
apply v HNil       = v
apply f (a ::: as) = apply (f a) as
