-- | Main entry point to the application.

module Main where

import Network.HTTP.Conduit
import Data.Aeson
import Data.Aeson.Types
import Network.URI
import Network.HTTP.Types.URI(renderQuery)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Control.Arrow
import Data.Maybe(catMaybes, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Vector as V

import Debug.Trace(trace)

targetAddress = "568 Broadway, New York, NY"

-- | Simpler variant of renderQuery that deals with plain Strings.
renderQueryS :: [(String, Maybe String)] -> String
renderQueryS = B.unpack . renderQuery True . packItems
  where packItems = map (B.pack *** fmap B.pack)

type LatLng = (Double, Double)

renderLatLng :: LatLng -> String
renderLatLng (lat, lng) = show lat ++ "," ++ show lng

class Endpoint a where
  buildURI :: a -> String

data FoursquareEndpoint =
    VenuesTrendingEndpoint { ll :: LatLng, limit :: Maybe Int, radius :: Maybe Double }

withFoursquareResponse :: (Object -> Parser a) -> Value -> Parser a
withFoursquareResponse f (Object obj) =
  do meta <- obj .: "meta"
     (Number code) <- meta .: "code"
     if floor code /= 200
       then do errorDetail <- meta .: "errorDetail"
               fail $ "received non-200 response code (" ++ errorDetail ++ ")"
       else return ()
     res <- obj .: "response"
     f res
withFoursquareResponse _ _ = fail "expected response object"

data Venue = Venue { venueId :: String, name :: String } deriving Show

data VenuesTrendingResponse = VenuesTrendingResponse { venues :: [Venue] } deriving Show

instance FromJSON VenuesTrendingResponse where
  parseJSON = withFoursquareResponse parseResponse
    where parseResponse :: Object -> Parser VenuesTrendingResponse
          parseResponse obj = do (Array venues) <- obj .: "venues"
                                 parsedVenues <- V.mapM (\(Object o) -> parseVenue o) venues
                                 return $ VenuesTrendingResponse $ V.toList parsedVenues
          parseVenue :: Object -> Parser Venue
          parseVenue obj = do (String idText) <- obj .: "id"
                              (String nameText) <- obj .: "name"
                              return $ Venue { venueId = T.unpack idText, name = T.unpack nameText }

packLazyByteString :: String -> BL.ByteString
packLazyByteString = BL.pack . map (toEnum . fromEnum)

callJsonEndpoint :: (FromJSON j, Endpoint e) => e -> IO j
callJsonEndpoint e =
  do responseBody <- simpleHttp (buildURI e)
     responseJson <- case decode responseBody of
                       Nothing -> fail "endpoint responded with invalid json"
                       (Just json) -> return json
     case parseEither parseJSON responseJson of
       (Left err) -> fail err
       (Right res) -> return res

-- https://developers.google.com/maps/documentation/geocoding/
data GeocoderEndpoint =
  GeocodeEndpoint { address :: String, sensor :: Bool }
  
instance Endpoint FoursquareEndpoint where
  buildURI VenuesTrendingEndpoint {ll = ll, limit = limit, radius = radius} =
    let params = [("ll", Just $ renderLatLng ll), ("limit", fmap show limit), ("radius", fmap show radius)]
    in "https://api.foursquare.com/v2/venues/trending" ++ renderQueryS params

instance Endpoint GeocoderEndpoint where
  buildURI GeocodeEndpoint { address = address, sensor = sensor } =
    let params = [("address", Just address), ("sensor", Just $ show sensor)]
    in "http://maps.googleapis.com/maps/api/geocode/json" ++ renderQueryS params

-- | The main entry point.
main :: IO ()
main =
  do putStrLn "API key?"
     apiKey <- getLine
     putStrLn "API secret?"
     apiSecret <- getLine
     venuesTrendingResponse <- (callJsonEndpoint $ VenuesTrendingEndpoint (0.0, 0.0) Nothing Nothing :: IO VenuesTrendingResponse)
     print venuesTrendingResponse

--

     --geocodeResponse <- callJsonEndpoint (GeocodeEndpoint targetAddress False)
--main = do putStrLn "YO WHATS UP"
  --putStrLn $ show $ buildURI $ VenuesTrending (2.0, 3.0) Nothing Nothing
  --res <- simpleHTTP (getRequest "http://www.haskell.org/") >>= fmap (take 100) . getResponseBody
  --B.putStrLn $ renderQuery False [(B.pack "hey", Just $ B.pack "you")]
  --putStrLn $ show $ buildURI $ VenuesTrending (2.0, 3.0) Nothing Nothing
