{-# LANGUAGE GADTs #-}

module Citrous.Unit.RouteResult where

data RouteResult err a where
  Fail :: err -> RouteResult err a
  FailFatal :: !err -> RouteResult err a
  Route :: !a -> RouteResult err a
