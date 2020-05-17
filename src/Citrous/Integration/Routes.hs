{-# LANGUAGE DataKinds #-}

module Citrous.Integration.Routes where


import           Citrous.Unit.ServerErr         (ServerErr, responseServerError)
import           Control.Monad.Error.Class      (throwError)
import           Data.Convertible.Utf8.Internal (ByteString)
import           Data.Extensible.Effect         (Eff, leaveEff, liftEff)
import           Data.Extensible.Effect.Default
import           Network.Wai                    (Application, Response)
import           Network.Wai.Internal           (Request)

type Routes = Eff '[ReaderDef Request, EitherDef (IO Response)] ServerErr

earlyReturnRoute :: IO Response -> Routes
earlyReturnRoute = throwError

accumFail :: ServerErr -> Routes
accumFail = return

runRoutes :: Routes -> Application
runRoutes routes req send = send =<< case leaveEff $ runEitherDef $ runReaderDef routes req of
  Left application -> application
  Right serverErr  -> return $ responseServerError serverErr
