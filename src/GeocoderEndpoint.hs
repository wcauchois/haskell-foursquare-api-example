module GeocoderEndpoint where

import Core
import Data.Char(toLower)
import Endpoint

-- https://developers.google.com/maps/documentation/geocoding/
data GeocoderEndpoint =
  GeocodeEndpoint { address :: String, sensor :: Bool }

instance Endpoint GeocoderEndpoint where
  buildURI GeocodeEndpoint { address = address, sensor = sensor } =
    let params = [("address", Just address), ("sensor", Just $ map toLower $ show sensor)]
    in "http://maps.googleapis.com/maps/api/geocode/json" ++ renderQuery True params