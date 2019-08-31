module GigGuide.Events exposing (..)

import Json.Decode exposing ( Decoder, list, map, map2, map6
                            , field, string, nullable, andThen
                            , succeed, fail)

import GigGuide.Genre exposing (..)

type alias VenueEvent =
  { venue : Venue
  , events : List Event
  }

decodeVenueEvent : Decoder VenueEvent
decodeVenueEvent =
  map2 VenueEvent
    decodeVenue
    <| field "events" (list decodeEvent)

type alias Venue =
  { name : String
  }
decodeVenue : Decoder Venue
decodeVenue = map Venue 
  <| field "vName" string

type alias Event =
  { name : String
  , time : String
  , price : String
  , genre : Maybe Genre
  , supports : List String
  , ticketLink : Maybe String
  }

decodeEvent : Decoder Event
decodeEvent =
  map6 Event
    (field "eName" string)
    (field "time"  string)
    (field "price" string)
    (field "genre" <| nullable decodeGenre)
    (field "supports" <| list string)
    (field "ticketUrl" <| nullable string)
