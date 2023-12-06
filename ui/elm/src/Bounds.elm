module Bounds exposing (..)

import Json.Decode exposing ( Decoder, map2, field, float)
import Json.Encode as E

type alias Bounds = 
  { northEast : Coord
  , southWest : Coord
  }

-- ex: small `isInside` big 
isInside : Bounds -> Bounds -> Bool
isInside i o =
     i.northEast.lat <= o.northEast.lat
  && i.northEast.lon <= o.northEast.lon
  && i.southWest.lat >= o.southWest.lat
  && i.southWest.lon >= o.southWest.lon

decodeBounds : Decoder Bounds
decodeBounds =
  map2 Bounds
    (field "northEast" decodeCoord)
    (field "southWest" decodeCoord)

type alias Coord = 
  { lat : Float
  , lon : Float
  }

decodeCoord : Decoder Coord
decodeCoord =
  map2 Coord
    (field "lat" float)
    (field "lon" float)