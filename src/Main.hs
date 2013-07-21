-- | Main entry point to the application.

module Main where

import Core
import Endpoint
import FoursquareEndpoint
import FoursquareModel
import GeocoderEndpoint
import GeocoderModel

targetAddress = "568 Broadway, New York, NY"

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
