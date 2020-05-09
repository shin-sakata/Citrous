module Citrous.Unit.ServerErr where

import           Data.ByteString.Lazy (ByteString)
import           Data.Utf8Convertible (convert)
import           Network.HTTP.Types
import           Network.Wai          (Response, responseLBS)

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

err300 :: ServerErr
err300 =
  ServerErr
    { errHttpStatus = status300,
      errBody = "",
      errHeaders = []
    }

err301 :: ServerErr
err301 =
  ServerErr
    { errHttpStatus = status301,
      errBody = "",
      errHeaders = []
    }

err302 :: ServerErr
err302 =
  ServerErr
    { errHttpStatus = status302,
      errBody = "",
      errHeaders = []
    }

err303 :: ServerErr
err303 =
  ServerErr
    { errHttpStatus = status303,
      errBody = "",
      errHeaders = []
    }

err304 :: ServerErr
err304 =
  ServerErr
    { errHttpStatus = status304,
      errBody = "",
      errHeaders = []
    }

err305 :: ServerErr
err305 =
  ServerErr
    { errHttpStatus = status305,
      errBody = "",
      errHeaders = []
    }

err307 :: ServerErr
err307 =
  ServerErr
    { errHttpStatus = status307,
      errBody = "",
      errHeaders = []
    }

err400 :: ServerErr
err400 =
  ServerErr
    { errHttpStatus = status400,
      errBody = "",
      errHeaders = []
    }

err401 :: ServerErr
err401 =
  ServerErr
    { errHttpStatus = status401,
      errBody = "",
      errHeaders = []
    }

err402 :: ServerErr
err402 =
  ServerErr
    { errHttpStatus = status402,
      errBody = "",
      errHeaders = []
    }

err403 :: ServerErr
err403 =
  ServerErr
    { errHttpStatus = status403,
      errBody = "",
      errHeaders = []
    }

err404 :: ServerErr
err404 =
  ServerErr
    { errHttpStatus = status404,
      errBody = "",
      errHeaders = []
    }

err405 :: ServerErr
err405 =
  ServerErr
    { errHttpStatus = status405,
      errBody = "",
      errHeaders = []
    }

err406 :: ServerErr
err406 =
  ServerErr
    { errHttpStatus = status406,
      errBody = "",
      errHeaders = []
    }

err407 :: ServerErr
err407 =
  ServerErr
    { errHttpStatus = status407,
      errBody = "",
      errHeaders = []
    }

err409 :: ServerErr
err409 =
  ServerErr
    { errHttpStatus = status409,
      errBody = "",
      errHeaders = []
    }

err410 :: ServerErr
err410 =
  ServerErr
    { errHttpStatus = status410,
      errBody = "",
      errHeaders = []
    }

err411 :: ServerErr
err411 =
  ServerErr
    { errHttpStatus = status411,
      errBody = "",
      errHeaders = []
    }

err412 :: ServerErr
err412 =
  ServerErr
    { errHttpStatus = status412,
      errBody = "",
      errHeaders = []
    }

err413 :: ServerErr
err413 =
  ServerErr
    { errHttpStatus = status413,
      errBody = "",
      errHeaders = []
    }

err414 :: ServerErr
err414 =
  ServerErr
    { errHttpStatus = status414,
      errBody = "",
      errHeaders = []
    }

err415 :: ServerErr
err415 =
  ServerErr
    { errHttpStatus = status415,
      errBody = "",
      errHeaders = []
    }

err416 :: ServerErr
err416 =
  ServerErr
    { errHttpStatus = status416,
      errBody = "",
      errHeaders = []
    }

err417 :: ServerErr
err417 =
  ServerErr
    { errHttpStatus = status417,
      errBody = "",
      errHeaders = []
    }

err418 :: ServerErr
err418 =
  ServerErr
    { errHttpStatus = status418,
      errBody = "",
      errHeaders = []
    }

err422 :: ServerErr
err422 =
  ServerErr
    { errHttpStatus = status422,
      errBody = "",
      errHeaders = []
    }

err500 :: ServerErr
err500 =
  ServerErr
    { errHttpStatus = status500,
      errBody = "",
      errHeaders = []
    }

err501 :: ServerErr
err501 =
  ServerErr
    { errHttpStatus = status501,
      errBody = "",
      errHeaders = []
    }

err502 :: ServerErr
err502 =
  ServerErr
    { errHttpStatus = status502,
      errBody = "",
      errHeaders = []
    }

err503 :: ServerErr
err503 =
  ServerErr
    { errHttpStatus = status503,
      errBody = "",
      errHeaders = []
    }

err504 :: ServerErr
err504 =
  ServerErr
    { errHttpStatus = status504,
      errBody = "",
      errHeaders = []
    }

err505 :: ServerErr
err505 =
  ServerErr
    { errHttpStatus = status505,
      errBody = "",
      errHeaders = []
    }
