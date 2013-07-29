module FoursquareModel where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V

withFoursquareResponse :: (Object -> Parser a) -> Value -> Parser a
withFoursquareResponse func val = do let Object obj = val
                                     response <- obj .: "response"
                                     func response

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
