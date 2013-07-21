-- | Main entry point to the application.

module Main where

import Network.HTTP
import Data.Aeson
import Network.URI
import Network.HTTP.Types.URI(renderQuery)
import qualified Data.ByteString.Char8 as BS
import Control.Arrow
import Data.Maybe(catMaybes, fromJust)

targetAddress = "568 Broadway, New York, NY"

-- | Simpler variant of renderQuery that deals with Strings.
renderQueryS :: [(String, Maybe String)] -> String
renderQueryS = BS.unpack . renderQuery True . packItems
  where packItems = map (BS.pack *** fmap BS.pack)

type LatLng = (Double, Double)

renderLatLng :: LatLng -> String
renderLatLng (lat, lng) = show lat ++ "," ++ show lng

class Endpoint a where
  buildURI :: a -> URI

data FoursquareEndpoint =
    VenuesTrendingEndpoint { ll :: LatLng, limit :: Maybe Int, radius :: Maybe Double } -- v2/venues/trending

withFoursquareResponse :: (Object -> Parser a) -> Value -> Parser a
withFoursquareResponse f v = withObject "" parseResponse v
  where parseResponse o = do res <- o .: "response"
                             withObject "" f res

data Venue = Venue { id :: String, name :: String }

data VenuesTrendingResponse = VenuesTrendingResponse { venues :: [Venue] }
instance FromJSON VenuesTrendingResponse where
  --parseJSON = withObject "" parseResponse

callJsonEndpoint :: (FromJSON j, Endpoint e) => e -> IO j

-- https://developers.google.com/maps/documentation/geocoding/
data GeocoderEndpoint =
  GeocodeEndpoint { address :: String, sensor :: Bool }
  
instance Endpoint FoursquareEndpoint where
  buildURI VenuesTrendingEndpoint {ll = ll, limit = limit, radius = radius} =
    let params = [("ll", Just $ renderLatLng ll), ("limit", fmap show limit), ("radius", fmap show radius)]
    in fromJust $ parseURI $ "https://api.foursquare.com/v2/venues/trending" ++ renderQueryS params

instance Endpoint GeocoderEndpoint where
  buildURI GeocodeEndpoint { address = address, sensor = sensor } =
    let params = [("address", Just address), ("sensor", Just $ show sensor)]
    in fromJust $ parseURI $ "http://maps.googleapis.com/maps/api/geocode/json" ++ renderQueryS params

-- | The main entry point.
main :: IO ()
main =
  do geocodeResponse :: GeocodeResponse
     geocodeResponse <- callJsonEndpoint (GeocodeEndpoint targetAddress False)
--main = do putStrLn "YO WHATS UP"
  --putStrLn $ show $ buildURI $ VenuesTrending (2.0, 3.0) Nothing Nothing
  --res <- simpleHTTP (getRequest "http://www.haskell.org/") >>= fmap (take 100) . getResponseBody
  --BS.putStrLn $ renderQuery False [(BS.pack "hey", Just $ BS.pack "you")]
  --putStrLn $ show $ buildURI $ VenuesTrending (2.0, 3.0) Nothing Nothing
