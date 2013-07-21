module Core where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.List(intercalate)
import qualified Data.Text as T
import Network.HTTP(urlEncode)

type LatLng = (Double, Double)

renderLatLng :: LatLng -> String
renderLatLng (lat, lng) = show lat ++ "," ++ show lng

navigateJson :: Value-> [String] -> Parser Value
navigateJson (Object obj) (first : second : rest) =
  do next <- obj .: (T.pack first)
     navigateJson next (second : rest)
navigateJson (Object obj) [last] = obj .: (T.pack last)

renderQuery :: Bool -> [(String, Maybe String)] -> String
renderQuery b params = (if b then "?" else "") ++ intercalate "&" serializedParams
  where serializedParams = catMaybes $ map renderParam params
        renderParam (key, Just val) = Just $ key ++ "=" ++ (urlEncode val)
        renderParam (_, Nothing) = Nothing

convertRational :: (Real a, Fractional b) => a -> b
convertRational = fromRational . toRational
