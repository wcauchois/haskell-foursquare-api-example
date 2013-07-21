-- | Main entry point to the application.

module Main where

import Network.HTTP.Conduit
import Data.Aeson
import Data.Aeson.Types
import Network.URI
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Control.Arrow
import Data.Maybe(catMaybes, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Vector as V
import Data.List(intercalate)
import Control.Monad
import Network.HTTP(urlEncode)
import Data.Char(toLower)

import Debug.Trace(trace)

targetAddress = "568 Broadway, New York, NY"
foursquareApiVersion = "20130721"

renderQuery :: Bool -> [(String, Maybe String)] -> String
renderQuery b params = (if b then "?" else "") ++ intercalate "&" serializedParams
  where serializedParams = catMaybes $ map renderParam params
        renderParam (key, Just val) = Just $ key ++ "=" ++ (urlEncode val)
        renderParam (_, Nothing) = Nothing

type LatLng = (Double, Double)

renderLatLng :: LatLng -> String
renderLatLng (lat, lng) = show lat ++ "," ++ show lng

class Endpoint a where
  buildURI :: a -> String

data FoursquareEndpoint =
    VenuesTrendingEndpoint { ll :: LatLng, limit :: Maybe Int, radius :: Maybe Double }

data FoursquareCredentials = FoursquareCredentials { clientId :: String, clientSecret :: String }

data AuthorizedFoursquareEndpoint = AuthorizedFoursquareEndpoint FoursquareCredentials FoursquareEndpoint
authorizeWith = flip AuthorizedFoursquareEndpoint

instance Endpoint AuthorizedFoursquareEndpoint where
  buildURI (AuthorizedFoursquareEndpoint creds e) =
    originalUri ++ "&" ++ renderQuery False authorizationParams
    where originalUri = buildURI e
          authorizationParams = [("client_id", Just $ clientId creds),
                                 ("client_secret", Just $ clientSecret creds),
                                 ("v", Just foursquareApiVersion)]

withFoursquareResponse :: (Object -> Parser a) -> Value -> Parser a
withFoursquareResponse f (Object obj) = obj .: "response" >>= f
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

data GeocodeResponse = GeocodeResponse LatLng deriving Show

navigateJson :: Value-> [String] -> Parser Value
navigateJson (Object obj) (first : second : rest) =
  do next <- obj .: (T.pack first)
     navigateJson next (second : rest)
navigateJson (Object obj) [last] = obj .: (T.pack last)

convertRational :: (Real a, Fractional b) => a -> b
convertRational = fromRational . toRational

instance FromJSON GeocodeResponse where
  parseJSON (Object obj) =
    do (Array results) <- obj .: "results"
       (Object location) <- navigateJson (results V.! 0) ["geometry", "location"]
       (Number lat) <- location .: "lat"
       (Number lng) <- location .: "lng"
       return $ GeocodeResponse (convertRational lat, convertRational lng)

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
    in "https://api.foursquare.com/v2/venues/trending" ++ renderQuery True params

instance Endpoint GeocoderEndpoint where
  buildURI GeocodeEndpoint { address = address, sensor = sensor } =
    let params = [("address", Just address), ("sensor", Just $ map toLower $ show sensor)]
    in "http://maps.googleapis.com/maps/api/geocode/json" ++ renderQuery True params

-- | The main entry point.
main :: IO ()
main =
  do geocodeResponse <- (callJsonEndpoint $ GeocodeEndpoint targetAddress False :: IO GeocodeResponse)
     print geocodeResponse
{-
  do putStrLn "API key?"
     apiKey <- getLine
     putStrLn "API secret?"
     apiSecret <- getLine
     let creds = FoursquareCredentials apiKey apiSecret
     let ll = (40.774848,-73.953368)
     venuesTrendingResponse <- (callJsonEndpoint $ VenuesTrendingEndpoint ll Nothing Nothing `authorizeWith` creds :: IO VenuesTrendingResponse)
     print venuesTrendingResponse
     -}

--

     --geocodeResponse <- callJsonEndpoint (GeocodeEndpoint targetAddress False)
--main = do putStrLn "YO WHATS UP"
  --putStrLn $ show $ buildURI $ VenuesTrending (2.0, 3.0) Nothing Nothing
  --res <- simpleHTTP (getRequest "http://www.haskell.org/") >>= fmap (take 100) . getResponseBody
  --B.putStrLn $ renderQuery False [(B.pack "hey", Just $ B.pack "you")]
  --putStrLn $ show $ buildURI $ VenuesTrending (2.0, 3.0) Nothing Nothing
