{-# LANGUAGE RecordWildCards #-}

module Citrous.Unit.ServerErr where

import           Data.ByteString.Lazy (ByteString)
import           Network.HTTP.Types
import           Network.Wai          (Response, responseLBS)

class ResponseError e where
  errorResponse :: e -> Response
  errorResponse _ =
    responseServerError err500

instance ResponseError ServerErr where
   errorResponse = responseServerError

data ServerErr = ServerErr
    { errHttpStatus :: Status
    , errBody       :: ByteString
    , errHeaders    :: [Header]
    }
    deriving (Show, Eq)

responseServerError :: ServerErr -> Response
responseServerError ServerErr {..} = responseLBS errHttpStatus errHeaders errBody

mkServerErr :: Status -> ByteString -> [Header] -> ServerErr
mkServerErr = ServerErr

-- re-export
mkStatus = Network.HTTP.Types.mkStatus

errWithStatus :: Status -> ServerErr
errWithStatus status =
  ServerErr
    { errHttpStatus = status,
      errBody = mempty,
      errHeaders = mempty
    }

err300 :: ServerErr
err300  = errWithStatus status300

err301 :: ServerErr
err301  = errWithStatus status301

err302 :: ServerErr
err302  = errWithStatus status302

err303 :: ServerErr
err303  = errWithStatus status303

err304 :: ServerErr
err304  = errWithStatus status304

err305 :: ServerErr
err305  = errWithStatus status305

err307 :: ServerErr
err307  = errWithStatus status307

err400 :: ServerErr
err400  = errWithStatus status400

err401 :: ServerErr
err401  = errWithStatus status401

err402 :: ServerErr
err402  = errWithStatus status402

err403 :: ServerErr
err403  = errWithStatus status403

err404 :: ServerErr
err404  = errWithStatus status404

err405 :: ServerErr
err405  = errWithStatus status405

err406 :: ServerErr
err406  = errWithStatus status406

err407 :: ServerErr
err407  = errWithStatus status407

err409 :: ServerErr
err409  = errWithStatus status409

err410 :: ServerErr
err410  = errWithStatus status410

err411 :: ServerErr
err411  = errWithStatus status411

err412 :: ServerErr
err412  = errWithStatus status412

err413 :: ServerErr
err413  = errWithStatus status413

err414 :: ServerErr
err414  = errWithStatus status414

err415 :: ServerErr
err415  = errWithStatus status415

err416 :: ServerErr
err416  = errWithStatus status416

err417 :: ServerErr
err417  = errWithStatus status417

err418 :: ServerErr
err418  = errWithStatus status418

err422 :: ServerErr
err422  = errWithStatus status422

err500 :: ServerErr
err500  = errWithStatus status500

err501 :: ServerErr
err501  = errWithStatus status501

err502 :: ServerErr
err502  = errWithStatus status502

err503 :: ServerErr
err503  = errWithStatus status503

err504 :: ServerErr
err504  = errWithStatus status504

err505 :: ServerErr
err505  = errWithStatus status505
