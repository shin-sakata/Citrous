{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Citrous.Unit.ServerErr
  ( module Citrous.Unit.ServerErr,
  )
where

import Data.Convertible.Utf8.Internal (LazyByteString)
import Network.HTTP.Types
import Network.Wai (Response, responseLBS)

data ServerErr
  = ServerErr
      { errHttpStatus :: Status,
        errBody :: LazyByteString,
        errHeaders :: [Header]
      }
  deriving (Show, Eq)

responseServerError :: ServerErr -> Response
responseServerError ServerErr {..} = responseLBS errHttpStatus errHeaders errBody

serverErrWithStatus :: Status -> ServerErr
serverErrWithStatus status =
  ServerErr
    { errHttpStatus = status,
      errBody = "",
      errHeaders = []
    }

err300 :: ServerErr
err300 = serverErrWithStatus status300

err301 :: ServerErr
err301 = serverErrWithStatus status301

err302 :: ServerErr
err302 = serverErrWithStatus status302

err303 :: ServerErr
err303 = serverErrWithStatus status303

err304 :: ServerErr
err304 = serverErrWithStatus status304

err305 :: ServerErr
err305 = serverErrWithStatus status305

err307 :: ServerErr
err307 = serverErrWithStatus status307

err400 :: ServerErr
err400 = serverErrWithStatus status400

err401 :: ServerErr
err401 = serverErrWithStatus status401

err402 :: ServerErr
err402 = serverErrWithStatus status402

err403 :: ServerErr
err403 = serverErrWithStatus status403

err404 :: ServerErr
err404 = serverErrWithStatus status404

err405 :: ServerErr
err405 = serverErrWithStatus status405

err406 :: ServerErr
err406 = serverErrWithStatus status406

err407 :: ServerErr
err407 = serverErrWithStatus status407

err409 :: ServerErr
err409 = serverErrWithStatus status409

err410 :: ServerErr
err410 = serverErrWithStatus status410

err411 :: ServerErr
err411 = serverErrWithStatus status411

err412 :: ServerErr
err412 = serverErrWithStatus status412

err413 :: ServerErr
err413 = serverErrWithStatus status413

err414 :: ServerErr
err414 = serverErrWithStatus status414

err415 :: ServerErr
err415 = serverErrWithStatus status415

err416 :: ServerErr
err416 = serverErrWithStatus status416

err417 :: ServerErr
err417 = serverErrWithStatus status417

err418 :: ServerErr
err418 = serverErrWithStatus status418

err422 :: ServerErr
err422 = serverErrWithStatus status422

err500 :: ServerErr
err500 = serverErrWithStatus status500

err501 :: ServerErr
err501 = serverErrWithStatus status501

err502 :: ServerErr
err502 = serverErrWithStatus status502

err503 :: ServerErr
err503 = serverErrWithStatus status503

err504 :: ServerErr
err504 = serverErrWithStatus status504

err505 :: ServerErr
err505 = serverErrWithStatus status505
