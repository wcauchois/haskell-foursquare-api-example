module GeocoderModel where

import Core
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V

data GeocodeResponse = GeocodeResponse LatLng deriving Show

instance FromJSON GeocodeResponse where
  parseJSON val =
    do let Object obj = val
       (Array results) <- obj .: "results"
       (Object location) <- navigateJson (results V.! 0) ["geometry", "location"]
       (Number lat) <- location .: "lat"
       (Number lng) <- location .: "lng"
       return $ GeocodeResponse (realToFrac lat, realToFrac lng)
