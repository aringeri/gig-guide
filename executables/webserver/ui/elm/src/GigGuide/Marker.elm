module GigGuide.Marker exposing (..)

import Json.Encode as E
import Tuple exposing (..)

type alias Url = String
type alias StyleClass = String

type alias Marker =
  { iconUrl : Url
  , iconSize: (Int, Int)
  , iconAnchor: (Int, Int)
  , popupAnchor: (Int, Int)
  , shadowSize: (Int, Int)
  , shadowAnchor: (Int, Int)
  , shadowUrl: Url
  }

encodeMarker : Marker -> E.Value
encodeMarker m =
  E.object 
    [  ("iconUrl",      E.string m.iconUrl)
    ,  ("iconSize",     encodeIntTuple m.iconSize)
    ,  ("iconAnchor",   encodeIntTuple m.iconAnchor)
    ,  ("popupAnchor",  encodeIntTuple m.popupAnchor)
    ,  ("shadowSize",   encodeIntTuple m.shadowSize)
    ,  ("shadowAnchor", encodeIntTuple m.shadowAnchor)
    ,  ("shadowUrl",    E.string m.shadowUrl)
    ]

encodeIntTuple = encode2Tuple E.int E.int

encode2Tuple : (a -> E.Value) -> (b -> E.Value) -> (a, b) -> E.Value
encode2Tuple a b t =
  let (a2, b2) = t |> mapSecond b << mapFirst a
  in E.list identity [a2, b2]

makeMarker : Url -> Marker
makeMarker u =
  { iconUrl = u
  , iconSize     = (33, 33)
  , iconAnchor   = (17, 33)
  , popupAnchor  = (0, -25)
  , shadowSize   = (35, 30)
  , shadowAnchor = (10, 30)
  , shadowUrl = defaultShadowIcon
  }

defaultShadowIcon : Url
defaultShadowIcon = "https://unpkg.com/leaflet@1.4.0/dist/images/marker-shadow.png"

markerByColour : Colour -> Marker
markerByColour c =
  makeMarker <| markerIconUrl c

type Colour =
    Red
  | Cyan
  | Purple
  | Blue
  | Indigo
  | Lime
  | Amber
  | Black
  | PaleRed
  | Orange
  | Green

colourClass : Colour -> StyleClass
colourClass c =
  case c of
    Red     -> "red"
    Cyan    -> "cyan"
    Purple  -> "purple"
    Blue    -> "blue"
    Indigo  -> "indigo"
    Lime    -> "lime"
    Amber   -> "amber"
    Black   -> "black"
    PaleRed -> "pale-red"
    Orange  -> "orange"
    Green   -> "green"

markerIconUrl : Colour -> Url
markerIconUrl c =
  "icon/" ++
    case c of
      Red     -> "icon-red.png"
      Cyan    -> "icon-cyan.png"
      Purple  -> "icon-purple.png"
      Blue    -> "icon-blue.png"
      Indigo  -> "icon-blue.png"
      Lime    -> "icon-lime.png"
      Amber   -> "icon-amber.png"
      Black   -> "icon-black.png"
      PaleRed -> "icon-pale-red.png"
      Orange  -> "icon-orange.png"
      Green   -> "icon-green.png"
