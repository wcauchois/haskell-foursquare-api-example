module Endpoint where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Conduit

class Endpoint a where
  buildURI :: a -> String

callJsonEndpoint :: (FromJSON j, Endpoint e) => e -> IO j
callJsonEndpoint e =
  do responseBody <- simpleHttp (buildURI e)
     responseJson <- case decode responseBody of
                       Nothing -> fail "endpoint responded with invalid json"
                       Just json -> return json
     case parseEither parseJSON responseJson of
       Left err -> fail err
       Right res -> return res
