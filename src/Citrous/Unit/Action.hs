module Citrous.Unit.Action
  ( Action
  , HasRequest
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
  , runAction
  ) where

import           Citrous.Unit.Application (ToApplication(..))
import           Data.Aeson               (FromJSON, ToJSON, encode)
import           Data.Utf8Convertible     (ConvertTo, convert)
import           Data.Vault.Lazy          (Vault)
import           Network.HTTP.Types       (HttpVersion, Method, Query,
                                           RequestHeaders, ok200)
import           Network.Socket           (SockAddr)
import           Network.Wai              (Application, Request,
                                           RequestBodyLength, Response,
                                           responseLBS)
import qualified Network.Wai              as Wai
import           RIO                      (ByteString, LByteString, RIO, Text,
                                           ask, join, liftIO, runRIO)

type HasRequest a = RIO Request a

type Action = RIO Request Response

instance ToApplication Action where
  toApplication action req respond = runAction req action >>= respond

runAction :: Request -> Action -> IO Response
runAction = runRIO

json :: (FromJSON a, ToJSON a) => a -> Action
json jsonData = pure $ responseLBS ok200 [("Content-Type", "application/json; charset=utf-8")] (encode jsonData)

maybeJson :: (FromJSON a, ToJSON a) => Maybe a -> Action
maybeJson jsonData =
  pure $ responseLBS ok200 [("Content-Type", "application/json; charset=utf-8")] (maybe "null" encode jsonData)

textPlain :: Text -> Action
textPlain txt = pure $ responseLBS ok200 [("Content-Type", "text/plain; charset=utf-8")] (convert txt)

textHtml :: Text -> Action
textHtml html = pure $ responseLBS ok200 [("Content-Type", "text/html; charset=utf-8")] (convert html)

requestMethod :: HasRequest Method
requestMethod = Wai.requestMethod <$> ask

httpVersion :: HasRequest HttpVersion
httpVersion = Wai.httpVersion <$> ask

rawPathInfo :: HasRequest ByteString
rawPathInfo = Wai.rawPathInfo <$> ask

rawQueryString :: HasRequest ByteString
rawQueryString = Wai.rawQueryString <$> ask

requestHeaders :: HasRequest RequestHeaders
requestHeaders = Wai.requestHeaders <$> ask

isSecure :: HasRequest Bool
isSecure = Wai.isSecure <$> ask

remoteHost :: HasRequest SockAddr
remoteHost = Wai.remoteHost <$> ask

pathInfo :: HasRequest [Text]
pathInfo = Wai.pathInfo <$> ask

queryString :: HasRequest Query
queryString = Wai.queryString <$> ask

requestBodyChunk :: HasRequest ByteString
requestBodyChunk = do
  req <- ask
  liftIO (Wai.getRequestBodyChunk req)

vault :: HasRequest Vault
vault = Wai.vault <$> ask

requestBodyLength :: HasRequest RequestBodyLength
requestBodyLength = Wai.requestBodyLength <$> ask

requestHeaderHost :: HasRequest (Maybe ByteString)
requestHeaderHost = Wai.requestHeaderHost <$> ask

requestHeaderRange :: HasRequest (Maybe ByteString)
requestHeaderRange = Wai.requestHeaderRange <$> ask

requestHeaderReferer :: HasRequest (Maybe ByteString)
requestHeaderReferer = Wai.requestHeaderReferer <$> ask

requestHeaderUserAgent :: HasRequest (Maybe ByteString)
requestHeaderUserAgent = Wai.requestHeaderUserAgent <$> ask

getQuery :: ByteString -> HasRequest (Maybe ByteString)
getQuery key = join <$> (lookup key <$> queryString)
