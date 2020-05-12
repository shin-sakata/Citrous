{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Citrous.Unit.Handler
  ( Handler,
    toApplication,
    responseLBS,
    maybeJson,
    textPlain,
    textHtml,
    json,
    RequestBodyLength (..),
    requestMethod,
    httpVersion,
    rawPathInfo,
    rawQueryString,
    requestHeaders,
    isSecure,
    remoteHost,
    pathInfo,
    queryString,
    requestBodyChunk,
    vault,
    requestBodyLength,
    requestHeaderHost,
    requestHeaderRange,
    requestHeaderReferer,
    requestHeaderUserAgent,
    getQuery,
    runHandler,
  )
where

import           Citrous.Unit.Application       (ToApplication (..))
import           Citrous.Unit.ServerErr         (ServerErr, responseServerError)
import           Control.Monad                  (join)
import           Control.Monad.Identity         (Identity, runIdentity)
import           Control.Monad.Reader           (Reader, ReaderT, ask, asks,
                                                 runReader, runReaderT)
import           Control.Monad.Reader.Class     (MonadReader)
import           Control.Monad.Trans            (lift, liftIO)
import           Control.Monad.Trans.Except     (ExceptT, runExceptT)
import           Data.Aeson                     (FromJSON, ToJSON, encode)
import           Data.ByteString                (ByteString)
import           Data.Convertible.Utf8          (convert)
import           Data.Extensible                (type (>:))
import           Data.Extensible.Class          (Member)
import           Data.Extensible.Effect         (Eff, ReaderEff, leaveEff,
                                                 liftEff, retractEff)
import           Data.Extensible.Effect.Default
import           Data.Text                      (Text)
import           Data.Vault.Lazy                (Vault)
import           Network.HTTP.Types             (HttpVersion, Method, Query,
                                                 RequestHeaders, ok200)
import           Network.Socket                 (SockAddr)
import           Network.Wai                    (Application, Request,
                                                 RequestBodyLength, Response,
                                                 responseLBS)
import qualified Network.Wai                    as Wai

type Handler = Eff '[ReaderDef Request, EitherDef ServerErr, "IO" >: IO] Response

instance ToApplication Handler where
  toApplication hdr req respond = do
    eitherResponse <- runHandler hdr req
    let response = unEither eitherResponse
    respond response
    where
      unEither :: Either ServerErr Response -> Response
      unEither (Right a)  = a
      unEither (Left err) = responseServerError err

runHandler :: Handler -> Request -> IO (Either ServerErr Response)
runHandler hdr req = (retractEff . runEitherDef . flip runReaderDef req) hdr

json :: (FromJSON a, ToJSON a, Monad m) => a -> m Response
json jsonData = return $ responseLBS ok200 [("Content-Type", "application/json; charset=utf-8")] (encode jsonData)

maybeJson :: (FromJSON a, ToJSON a, Monad m) => Maybe a -> m Response
maybeJson jsonData =
  return $ responseLBS ok200 [("Content-Type", "application/json; charset=utf-8")] (maybe "null" encode jsonData)

textPlain :: Monad m => Text -> m Response
textPlain txt = return $ responseLBS ok200 [("Content-Type", "text/plain; charset=utf-8")] (convert txt)

textHtml :: Monad m => Text -> m Response
textHtml html = return $ responseLBS ok200 [("Content-Type", "text/html; charset=utf-8")] (convert html)

type HasRequestT m a = MonadReader Request m => m a

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
