module Citrous.Unit.Handler
  ( Handler
  , toApplication
  , responseLBS
  , maybeJson
  , textPlain
  , textHtml
  , json
  , RequestBodyLength(..)
  , requestMethod
  , httpVersion
  , rawPathInfo
  , rawQueryString
  , requestHeaders
  , isSecure
  , remoteHost
  , pathInfo
  , queryString
  , requestBodyChunk
  , vault
  , requestBodyLength
  , requestHeaderHost
  , requestHeaderRange
  , requestHeaderReferer
  , requestHeaderUserAgent
  , getQuery
  , runHandler
  ) where

import           Citrous.Unit.Application (ToApplication (..))
import           Control.Monad            (join)
import           Control.Monad.Identity   (Identity, runIdentity)
import           Control.Monad.Trans      (liftIO, lift)
import           Data.Aeson               (FromJSON, ToJSON, encode)
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           Data.Utf8Convertible     (ConvertTo, convert)
import           Data.Vault.Lazy          (Vault)
import           Network.HTTP.Types       (HttpVersion, Method, Query,
                                           RequestHeaders, ok200)
import           Network.Socket           (SockAddr)
import           Network.Wai              (Application, Request,
                                           RequestBodyLength, Response,
                                           responseLBS)
import qualified Network.Wai              as Wai
import           Control.Monad.Reader     (Reader, ReaderT, ask, asks,
                                           runReader, runReaderT)
import           Control.Monad.Trans.Except
                 (ExceptT, runExceptT)
import           Citrous.Unit.ServerErr
                 (ServerErr, responseServerError)

type Handler = ReaderT Request (ExceptT ServerErr IO) Response

type HasRequestT m a = Monad m => ReaderT Request m a

instance ToApplication Handler where
  toApplication hdr req respond = do
     hdr' <- runHandler hdr req
     let response = unEither hdr'
     respond response
    where
      unEither :: Either ServerErr Response -> Response
      unEither (Right a)  = a
      unEither (Left err) = responseServerError err

runHandler :: Handler -> Request -> IO (Either ServerErr Response)
runHandler hdr req = runExceptT (runReaderT hdr req)

json :: (FromJSON a, ToJSON a) => a -> Handler
json jsonData = return $ responseLBS ok200 [("Content-Type", "application/json; charset=utf-8")] (encode jsonData)

maybeJson :: (FromJSON a, ToJSON a) => Maybe a -> Handler
maybeJson jsonData =
  return $ responseLBS ok200 [("Content-Type", "application/json; charset=utf-8")] (maybe "null" encode jsonData)

textPlain :: Text -> Handler
textPlain txt = return $ responseLBS ok200 [("Content-Type", "text/plain; charset=utf-8")] (convert txt)

textHtml :: Text -> Handler
textHtml html = return $ responseLBS ok200 [("Content-Type", "text/html; charset=utf-8")] (convert html)

requestMethod :: HasRequestT m Method
requestMethod = asks Wai.requestMethod

httpVersion :: HasRequestT m HttpVersion
httpVersion = asks Wai.httpVersion

rawPathInfo :: HasRequestT m ByteString
rawPathInfo = asks Wai.rawPathInfo

rawQueryString :: HasRequestT m ByteString
rawQueryString = asks Wai.rawQueryString

requestHeaders :: HasRequestT m RequestHeaders
requestHeaders = asks Wai.requestHeaders

isSecure :: HasRequestT m Bool
isSecure = asks Wai.isSecure

remoteHost :: HasRequestT m SockAddr
remoteHost = asks Wai.remoteHost

pathInfo :: HasRequestT m [Text]
pathInfo = asks Wai.pathInfo

queryString :: HasRequestT m Query
queryString = asks Wai.queryString

requestBodyChunk :: HasRequestT m (IO ByteString)
requestBodyChunk = asks Wai.getRequestBodyChunk

vault :: HasRequestT m Vault
vault = asks Wai.vault

requestBodyLength :: HasRequestT m RequestBodyLength
requestBodyLength = asks Wai.requestBodyLength

requestHeaderHost :: HasRequestT m (Maybe ByteString)
requestHeaderHost = asks Wai.requestHeaderHost

requestHeaderRange :: HasRequestT m (Maybe ByteString)
requestHeaderRange = asks Wai.requestHeaderRange

requestHeaderReferer :: HasRequestT m (Maybe ByteString)
requestHeaderReferer = asks Wai.requestHeaderReferer

requestHeaderUserAgent :: HasRequestT m (Maybe ByteString)
requestHeaderUserAgent = asks Wai.requestHeaderUserAgent

getQuery :: ByteString -> HasRequestT m (Maybe ByteString)
getQuery key = join <$> (lookup key <$> queryString)
