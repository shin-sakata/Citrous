{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Citrous.Integration.Routes where

import           Citrous.Unit.ServerErr         (ServerErr, err400, err401,
                                                 err404, err405, err406, err415,
                                                 errHeaders,
                                                 responseServerError)
import           Control.Monad.Error.Class      (throwError)
import           Control.Monad.Writer           (tell)
import           Data.ByteString.Char8          (intercalate)
import           Data.Convertible.Utf8.Internal (ByteString)
import           Data.Extensible.Class          (type (>:))
import           Data.Extensible.Effect         (Eff, liftEff, retractEff)
import           Data.Extensible.Effect.Default
import           Data.Set                       (Set, singleton, toList)
import           Network.HTTP.Types.Header      (Header, hAllow)
import           Network.HTTP.Types.Method      (StdMethod (..),
                                                 renderStdMethod)
import           Network.Wai                    (Application, Response)
import           Network.Wai.Internal           (Request)

type Routes = Eff '[ReaderDef Request, EitherDef (IO Response), WriterDef RoutingErr, "IO" >: IO] ()

-- throwErrorを利用して最初にマッチしたRouteを返す。
earlyReturnRoute :: IO Response -> Routes
earlyReturnRoute = throwError

runRoutes :: Routes -> Application
runRoutes routes req send = send =<< do
  result <- retractEff $ runWriterDef $ runEitherDef $ runReaderDef routes req
  case result of
    (Left application, _) -> application
    (_, serverErr)        -> return $ responseRoutingErr serverErr

-- Ordを導出している
-- 比較して小さい方がエラーの優先度が高い
-- methodNotAllowedの場合はAllowヘッダーを出力する為に可能なMethodを伝搬させる
data RoutingErr = BadRequest
    | NotAcceptable
    | UnsupportedMediaType
    | Unauthorized
    | MethodMotAllowed (Set StdMethod)
    | NotFound
    deriving (Show, Eq, Ord)

toServerErr :: RoutingErr -> ServerErr
toServerErr BadRequest           = err400
toServerErr NotAcceptable        = err406
toServerErr UnsupportedMediaType = err415
toServerErr Unauthorized         = err401
toServerErr (MethodMotAllowed _) = err405
toServerErr NotFound             = err404

responseRoutingErr :: RoutingErr -> Response
responseRoutingErr routingErr@(MethodMotAllowed methods) =
  responseServerError $ (toServerErr routingErr) {errHeaders = [renderAllowedMethods methods]}
responseRoutingErr routingErr =
  responseServerError $ toServerErr routingErr

renderAllowedMethods :: Set StdMethod -> Header
renderAllowedMethods methods = (hAllow, renderMethods methods)
  where
    renderMethods :: Set StdMethod -> ByteString
    renderMethods methods = intercalate ", " (map renderStdMethod (toList methods))

-- smart constructor
badMethod :: StdMethod -> RoutingErr
badMethod method = MethodMotAllowed $ singleton method

instance Semigroup RoutingErr where
  (MethodMotAllowed methodsL) <> (MethodMotAllowed methodsR) = MethodMotAllowed (methodsL <> methodsR)
  l <> r = if l < r then l else r

instance Monoid RoutingErr where
  mempty = NotFound -- It's the lowest priority.
