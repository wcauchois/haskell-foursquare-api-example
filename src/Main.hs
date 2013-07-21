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
  do putStrLn "API key?"
     apiKey <- getLine
     putStrLn "API secret?"
     apiSecret <- getLine
     let creds = FoursquareCredentials apiKey apiSecret

     (GeocodeResponse latLng) <- (callJsonEndpoint $ GeocodeEndpoint targetAddress False :: IO GeocodeResponse)
     let venuesTrendingEndpoint = VenuesTrendingEndpoint latLng Nothing Nothing `authorizeWith` creds
     (VenuesTrendingResponse venues) <- (callJsonEndpoint venuesTrendingEndpoint :: IO VenuesTrendingResponse)
     let printVenue v = putStrLn $ "- " ++ name v
     mapM_ printVenue venues
