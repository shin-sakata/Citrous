{-# LANGUAGE DataKinds #-}

module Citrous.Integration.Routes where


import           Citrous.Unit.ServerErr         (ServerErr)
import           Data.Convertible.Utf8.Internal (ByteString)
import           Data.Extensible.Effect         (Eff, leaveEff, liftEff)
import           Data.Extensible.Effect.Default

type Routes a = Eff '[ReaderDef ByteString, EitherDef a] ServerErr
