module Citrous.Unit.ServerErr where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types (Header, mkStatus)
import Network.Wai (Response, responseLBS)
import Data.Utf8Convertible (convert)

data ServerErr = ServerErr
    { errHTTPCode     :: Int
    , errReasonPhrase :: String
    , errBody         :: ByteString
    , errHeaders      :: [Header]
    }
  deriving (Show, Eq, Read)

responseServerError :: ServerErr -> Response
responseServerError ServerErr{..} = responseLBS status errHeaders errBody
  where
    status = mkStatus errHTTPCode (convert errReasonPhrase)


err300 :: ServerErr
err300 = ServerErr { errHTTPCode = 300
                    , errReasonPhrase = "Multiple Choices"
                    , errBody = ""
                    , errHeaders = []
                    }

err301 :: ServerErr
err301 = ServerErr { errHTTPCode = 301
                    , errReasonPhrase = "Moved Permanently"
                    , errBody = ""
                    , errHeaders = []
                    }


err302 :: ServerErr
err302 = ServerErr { errHTTPCode = 302
                    , errReasonPhrase = "Found"
                    , errBody = ""
                    , errHeaders = []
                    }


err303 :: ServerErr
err303 = ServerErr { errHTTPCode = 303
                    , errReasonPhrase = "See Other"
                    , errBody = ""
                    , errHeaders = []
                    }


err304 :: ServerErr
err304 = ServerErr { errHTTPCode = 304
                    , errReasonPhrase = "Not Modified"
                    , errBody = ""
                    , errHeaders = []
                    }

err305 :: ServerErr
err305 = ServerErr { errHTTPCode = 305
                    , errReasonPhrase = "Use Proxy"
                    , errBody = ""
                    , errHeaders = []
                    }

err307 :: ServerErr
err307 = ServerErr { errHTTPCode = 307
                    , errReasonPhrase = "Temporary Redirect"
                    , errBody = ""
                    , errHeaders = []
                    }

err400 :: ServerErr
err400 = ServerErr { errHTTPCode = 400
                    , errReasonPhrase = "Bad Request"
                    , errBody = ""
                    , errHeaders = []
                    }

err401 :: ServerErr
err401 = ServerErr { errHTTPCode = 401
                    , errReasonPhrase = "Unauthorized"
                    , errBody = ""
                    , errHeaders = []
                    }

err402 :: ServerErr
err402 = ServerErr { errHTTPCode = 402
                    , errReasonPhrase = "Payment Required"
                    , errBody = ""
                    , errHeaders = []
                    }

err403 :: ServerErr
err403 = ServerErr { errHTTPCode = 403
                    , errReasonPhrase = "Forbidden"
                    , errBody = ""
                    , errHeaders = []
                    }

err404 :: ServerErr
err404 = ServerErr { errHTTPCode = 404
                    , errReasonPhrase = "Not Found"
                    , errBody = ""
                    , errHeaders = []
                    }

err405 :: ServerErr
err405 = ServerErr { errHTTPCode = 405
                    , errReasonPhrase = "Method Not Allowed"
                    , errBody = ""
                    , errHeaders = []
                    }

err406 :: ServerErr
err406 = ServerErr { errHTTPCode = 406
                    , errReasonPhrase = "Not Acceptable"
                    , errBody = ""
                    , errHeaders = []
                    }

err407 :: ServerErr
err407 = ServerErr { errHTTPCode = 407
                    , errReasonPhrase = "Proxy Authentication Required"
                    , errBody = ""
                    , errHeaders = []
                    }

err409 :: ServerErr
err409 = ServerErr { errHTTPCode = 409
                    , errReasonPhrase = "Conflict"
                    , errBody = ""
                    , errHeaders = []
                    }

err410 :: ServerErr
err410 = ServerErr { errHTTPCode = 410
                    , errReasonPhrase = "Gone"
                    , errBody = ""
                    , errHeaders = []
                    }

err411 :: ServerErr
err411 = ServerErr { errHTTPCode = 411
                    , errReasonPhrase = "Length Required"
                    , errBody = ""
                    , errHeaders = []
                    }

err412 :: ServerErr
err412 = ServerErr { errHTTPCode = 412
                    , errReasonPhrase = "Precondition Failed"
                    , errBody = ""
                    , errHeaders = []
                    }

err413 :: ServerErr
err413 = ServerErr { errHTTPCode = 413
                    , errReasonPhrase = "Request Entity Too Large"
                    , errBody = ""
                    , errHeaders = []
                    }

err414 :: ServerErr
err414 = ServerErr { errHTTPCode = 414
                    , errReasonPhrase = "Request-URI Too Large"
                    , errBody = ""
                    , errHeaders = []
                    }

err415 :: ServerErr
err415 = ServerErr { errHTTPCode = 415
                    , errReasonPhrase = "Unsupported Media Type"
                    , errBody = ""
                    , errHeaders = []
                    }

err416 :: ServerErr
err416 = ServerErr { errHTTPCode = 416
                    , errReasonPhrase = "Request range not satisfiable"
                    , errBody = ""
                    , errHeaders = []
                    }

err417 :: ServerErr
err417 = ServerErr { errHTTPCode = 417
                    , errReasonPhrase = "Expectation Failed"
                    , errBody = ""
                    , errHeaders = []
                    }

err418 :: ServerErr
err418 = ServerErr { errHTTPCode = 418
                    , errReasonPhrase = "I'm a teapot"
                    , errBody = ""
                    , errHeaders = []
                    }

err422 :: ServerErr
err422 = ServerErr { errHTTPCode = 422
                    , errReasonPhrase = "Unprocessable Entity"
                    , errBody = ""
                    , errHeaders = []
                    }

err500 :: ServerErr
err500 = ServerErr { errHTTPCode = 500
                    , errReasonPhrase = "Internal Server Error"
                    , errBody = ""
                    , errHeaders = []
                    }

err501 :: ServerErr
err501 = ServerErr { errHTTPCode = 501
                    , errReasonPhrase = "Not Implemented"
                    , errBody = ""
                    , errHeaders = []
                    }

err502 :: ServerErr
err502 = ServerErr { errHTTPCode = 502
                    , errReasonPhrase = "Bad Gateway"
                    , errBody = ""
                    , errHeaders = []
                    }

err503 :: ServerErr
err503 = ServerErr { errHTTPCode = 503
                    , errReasonPhrase = "Service Unavailable"
                    , errBody = ""
                    , errHeaders = []
                    }

err504 :: ServerErr
err504 = ServerErr { errHTTPCode = 504
                    , errReasonPhrase = "Gateway Time-out"
                    , errBody = ""
                    , errHeaders = []
                    }

err505 :: ServerErr
err505 = ServerErr { errHTTPCode = 505
                    , errReasonPhrase = "HTTP Version not supported"
                    , errBody = ""
                    , errHeaders = []
                    }