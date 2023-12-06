module GigGuide exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import Date
import Tuple exposing (..)

import Json.Decode exposing (Decoder, decodeValue)
import Json.Encode as E
import GeoJson exposing (GeoJson, FeatureObject, GeoJsonObject(..), decoder, encode)
import Url.Builder as U

import GigGuide.Events exposing (..)
import GigGuide.Genre exposing (..)
import GigGuide.Marker exposing (..)
import Bounds exposing (..)
import DateRange exposing (..)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

subscriptions model =
  Browser.Events.onResize (\w -> WindowResized << Size w)

type alias Size =
  { width  : Int
  , height : Int
  }

-- MODEL
type alias Model =
  { bounds : Maybe Bounds
  , points : E.Value
  , searchArea : Maybe Bounds
  , selectedDate : Maybe Date.Date
  , validDates : Maybe DateRange
  , selectedFeature : Maybe VenueEvent
  , cachedDate : Maybe Date.Date
  , sidebar : Bool
  , mapId : String
  , canApplyFilter : Bool
  , loading : Bool
  , windowSize : Maybe Size
  }

init : String -> (Model, Cmd Msg)
init mapId =
  ({ bounds = Nothing
   , points = encode (FeatureCollection [], Nothing)
   , selectedDate = Nothing
   , validDates = Nothing
   , selectedFeature = Nothing
   , sidebar = False
   , searchArea = Nothing
   , cachedDate = Nothing
   , mapId = mapId
   , canApplyFilter = False
   , loading = True
   , windowSize = Nothing
   }
  , Cmd.batch [ Task.perform GotTodaysDate Date.today
              , Task.perform GotViewPort Browser.Dom.getViewport])

type Msg 
  = GotInitialBounds Bounds
  | BoundsChanged Bounds
  | DateChanged String
  | GotTodaysDate Date.Date
  | GetPoints
  | GotPoints (Result Http.Error (Bounds, GeoJson))
  | FeatureSelected FeatureObject
  | ShowSidebar Bool
  | WindowResized Size
  | GotViewPort Browser.Dom.Viewport

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotTodaysDate d -> 
      ({ model | selectedDate = Just d 
               , validDates   = Just <| mkDateRange d }
      , Cmd.none)
    GotInitialBounds b ->
      ({ model | bounds = Just b, loading = True }, getPoints model.selectedDate b)
    BoundsChanged b ->
      ({ model | bounds = Just b }, Cmd.none)
    DateChanged s ->
      case Date.fromIsoString s of
        Ok d  -> ({model | selectedDate = Just d
                         , canApplyFilter = Just d /= model.cachedDate
                                            && Maybe.withDefault False (Maybe.map (isInRange d) model.validDates) }
                 , Cmd.none)
        _     -> (model, Cmd.none)
    GetPoints -> 
      case model.bounds of
        Just b -> ({model | loading = True}, getPoints model.selectedDate b)
        _      -> (model, Cmd.none)
    GotPoints r ->
      case r of
        Ok  (b,p) -> ({model | points = encode p
                             , selectedFeature = Nothing
                             , canApplyFilter = False
                             , searchArea = Just b
                             , cachedDate = model.selectedDate
                             , loading = False}
                     , Cmd.none)
        _         -> (model, Cmd.none)
    FeatureSelected f -> 
      case (decodeSelectedVenueEvent f.properties) of
        Ok v  -> ({model | selectedFeature = Just v
                         , sidebar = True}, Cmd.none)
        _     -> (model, Cmd.none)
    ShowSidebar b -> ({model | sidebar = b}, Cmd.none)
    WindowResized s ->
      ({model | windowSize = Just s}, Cmd.none)
    GotViewPort v ->
      let vp = v.viewport
          w = round vp.width
          h = round vp.height
      in ({model | windowSize = Just <| Size w h}, Cmd.none)

mkDateRange d = 
  let s   = modBy 7 <| Date.weekdayNumber d -- index by zero - starting on Sunday
      max = 7 - s
  in makeDateRange d max Date.Days

decodeSelectedVenueEvent =
  decodeValue decodeVenueEvent

getPoints selectedDate bounds = 
  let north = bounds.northEast.lat
      east  = bounds.northEast.lon
      south = bounds.southWest.lat
      west  = bounds.southWest.lon
      date = fmtMaybeDate selectedDate
      uFloat p = U.string p << String.fromFloat
  in Http.get
      { url = U.relative ["search", "FeatureCollection.json"] []
      , expect = Http.expectJson (GotPoints << Result.map (\p -> (bounds, p))) decoder
      }

fmtMaybeDate : Maybe Date.Date -> String
fmtMaybeDate d =
  Maybe.withDefault "" <| Maybe.map Date.toIsoString d

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ div [ class "sidebar animate-left card", sidebarStyle model ]
      [ div [class "teal"] 
        [ label [class "bar-item large filter-label"] [text "Filters"]
        , button [onClick (ShowSidebar False), class "button close-button"] [text <| uniChar 215]
        ]
      , barDiv [] [label [] [text "Date:"], dateInput model]
      , barDiv [class "teal apply-btn-parent"] 
               [applyButton model]
      , barDiv [] <| viewVenueEvent model
      ]
    , div [ class "main-content" ] 
      [ div [ class "top-bar teal"]
            [ sidebarButton model
            , span [] [text "Gig Guide"]
            , a [class "git-link", href "https://github.com/aringeri/gig-guide", title "View at GitHub"]
                [i [class "fa fa-github", attribute "aria-hidden" "true"] []]
      ]
      , searchOverlay model
      , Html.node "custom-map" 
         [ class "map"
         , id <| model.mapId
         , property "view" initialView
         , onInitialized GotInitialBounds decodeBoundsEvent
         , property "tileProperties" tileProperties
         , property "defaultIconProps" defaultIcon
         , property "markerMap" genreIconMap
         , property "zoomPosition" <| E.string "topright"
         , onBoundsChanged BoundsChanged
         , Html.Attributes.property "geoJson" model.points
         , onMarkerClick FeatureSelected decodeFeatureObject
         ]
         []
      ]
    ]

defaultIcon : E.Value
defaultIcon =
  encodeMarker <| markerByColour Green

applyButton model =
  let loadingDisplay = 
        if model.loading then "inline-block" else "none"
  in button ([class "button apply-button", onClick GetPoints] 
              ++ canApplyStyle model)
            [text "Apply", i [ style "display" loadingDisplay
                             , class "fa fa-circle-o-notch fa-spin"] []
            ]

canApplyStyle model =
  if model.canApplyFilter
    then []
    else [attribute "disabled" "true"]

onInitialized f d =
  Html.Events.on "initialized"
    <| Json.Decode.map f d

searchOverlay : Model -> Html Msg
searchOverlay model = 
  div [class "overlay leaflet-bar", style "visibility" "hidden"]
      [button [ class "search-button", onClick GetPoints
              , style "visibility" <| if canSearch model then "inherit" else "hidden"] 
              [i [class "fa fa-search"] [], text "Search this area"]
      ]

canSearch : Model -> Bool
canSearch model =
  Maybe.withDefault False
    <| Maybe.map2 boundsDifferent model.bounds model.searchArea

boundsDifferent : Bounds -> Bounds -> Bool
boundsDifferent b cache =
     b /= cache
  && not (isInside b cache)

sidebarButton : Model -> Html Msg
sidebarButton model =
  button [onClick  (toggleSidebar model), class "button xlarge"]
         [text <| uniChar 187]

uniChar : Int -> String
uniChar = String.fromChar << Char.fromCode

barDiv : List (Html.Attribute msg) -> List (Html msg) -> Html msg
barDiv a = div (class "bar-item" :: a)

genreIconMap : E.Value
genreIconMap =
  E.list (\t -> t |> mapFirst E.string |> mapSecond encodeMarker |> encode2Tuple identity identity) genreIcons

genreIcons : List (String, Marker)
genreIcons =
  List.map (\(g, (_, m)) -> (encodeGenre g, m)) enumGenreMarkers

tileProperties : E.Value
tileProperties =
  E.object
    [ ("url", E.string "https://api.mapbox.com/styles/v1/mapbox/streets-v10/tiles/{z}/{x}/{y}?access_token={accessToken}")
    , ("options", E.object 
        [ ("attribution", E.string """Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>""")
        , ("maxZoom", E.int 18)
        , ("accessToken", E.string "pk.eyJ1IjoiYXJpbmciLCJhIjoiY2p6N3prbmVwMGlvdjNjbWhlMzJtb240dCJ9.UzSWkQCDedp7y9LXp7BY4g")]
      )
    ]

initialView : E.Value
initialView =
  E.object [ ("coords", E.list E.float [-37.813611, 144.963056])
           , ("zoom", E.int 13)]

dateInput model =
  let min = fmtMaybeDate <| Maybe.map .min model.validDates
      max = fmtMaybeDate <| Maybe.map .max model.validDates
  in input [ attribute "type" "date"
           , value <| fmtMaybeDate model.selectedDate
           , attribute "min" min
           , attribute "max" max
           , onInput DateChanged] 
           []

toggleSidebar m = 
  ShowSidebar (not m.sidebar)
sidebarStyle m =
  class <|
    if m.sidebar then 
      if (smallWindow m) then
        "sidebar-open-small"
      else
        "sidebar-open"
    else 
      "sidebar-closed" 

smallWindow model =
  Maybe.withDefault False 
          <| Maybe.map (\s -> s.width <= 700) 
                       model.windowSize

viewVenueEvent model =
  case model.selectedFeature of
    Nothing -> []
    Just f  -> 
        label [] [text "Venue:", span [] [text f.venue.name]]
        :: viewEvents f.events


viewEvents : List Event -> List (Html msg)
viewEvents = 
  List.map viewEvent

viewEvent : Event -> Html msg
viewEvent e = div [] <|
  [ div [] [h3 [] <| [text e.name] ++ ticketLink e]
  , div [] [gigLabel [text "Time:"], span [] [text e.time]]
  , div [] [gigLabel [text "Price:"], span [] [text e.price]]
  ]
  ++ List.filterMap identity [ Maybe.map viewGenre e.genre
                             , viewSupports e.supports]
  ++ [hr [] []]

ticketLink : Event -> List (Html msg)
ticketLink e =
  case e.ticketLink of
    Just url ->
      [ a [target "_blank", rel "noopener noreferrer", href url]
          [i [class "fa fa-ticket", attribute "aria-hidden" "true"] []]
      ]
    _        -> []

viewGenre : Genre -> Html msg
viewGenre g =
  let genreStyle = class << first << genreProperties <| g
  in div [] [ gigLabel [text "Genre:"]
            , span [class "tag", genreStyle] 
                   [text <| encodeGenre g]
            ]

viewSupports : List String -> Maybe (Html msg)
viewSupports s =
  case s of
    [] -> Nothing
    ss -> Just <| div [] [gigLabel [ text "Supports:"]
                                   , span [class "multi-line-content"] 
                                          [text <| String.join "," ss]
                                   ]

gigLabel : List (Html msg) -> Html msg
gigLabel = label [class "gig-label"]

decodeFeatureObject : Decoder FeatureObject
decodeFeatureObject =
  let f a = 
        case a of
          (Feature o, _) -> Json.Decode.succeed o
          b              -> Json.Decode.fail "Non feature found in decoder: decodeFeatureObject" 

  in Json.Decode.at ["target", "_selected"] 
                    (decoder |> Json.Decode.andThen f)

onMarkerClick : (a -> Msg) -> Decoder a -> Attribute Msg
onMarkerClick f d =
  Html.Events.on "markerClicked" (Json.Decode.map f d)

onBoundsChanged : (Bounds -> Msg) -> Attribute Msg
onBoundsChanged f =
  Html.Events.on "boundsChanged"
    <| Json.Decode.map f 
    <| Json.Decode.at ["target", "bounds"] decodeBounds

decodeBoundsEvent : Decoder Bounds
decodeBoundsEvent =
  Json.Decode.at ["target", "bounds"] decodeBounds
