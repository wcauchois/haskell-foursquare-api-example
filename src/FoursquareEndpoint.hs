module FoursquareEndpoint where

import Core
import Endpoint

foursquareApiVersion = "20130721"

data FoursquareCredentials = FoursquareCredentials { clientId :: String, clientSecret :: String }

data FoursquareEndpoint =
    VenuesTrendingEndpoint { ll :: LatLng, limit :: Maybe Int, radius :: Maybe Double }

instance Endpoint FoursquareEndpoint where
  buildURI VenuesTrendingEndpoint {ll = ll, limit = limit, radius = radius} =
    let params = [("ll", Just $ renderLatLng ll), ("limit", fmap show limit), ("radius", fmap show radius)]
    in "https://api.foursquare.com/v2/venues/trending" ++ renderQuery True params

data AuthorizedFoursquareEndpoint = AuthorizedFoursquareEndpoint FoursquareCredentials FoursquareEndpoint

instance Endpoint AuthorizedFoursquareEndpoint where
  buildURI (AuthorizedFoursquareEndpoint creds e) = appendParams originalUri authorizationParams
    where originalUri = buildURI e
          authorizationParams = [("client_id", Just $ clientId creds),
                                 ("client_secret", Just $ clientSecret creds),
                                 ("v", Just foursquareApiVersion)]

authorizeWith :: FoursquareEndpoint -> FoursquareCredentials -> AuthorizedFoursquareEndpoint
authorizeWith = flip AuthorizedFoursquareEndpoint
