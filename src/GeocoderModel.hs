module GeocoderModel where

import Core
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V

data GeocodeResponse = GeocodeResponse LatLng deriving Show

instance FromJSON GeocodeResponse where
  parseJSON (Object obj) =
    do (Array results) <- obj .: "results"
       (Object location) <- navigateJson (results V.! 0) ["geometry", "location"]
       (Number lat) <- location .: "lat"
       (Number lng) <- location .: "lng"
       return $ GeocodeResponse (convertRational lat, convertRational lng)
