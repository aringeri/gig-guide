module GigGuide.Genre exposing (..)

import Json.Decode exposing (..)
import Json.Encode as E
import GigGuide.Marker exposing (..)

type Genre =
    Rock
  | Global
  | Jazz
  | Electronic
  | HipHop
  | Classical
  | RnB
  | Punk
  | Metal
  | Pop
  | Acoustic
  | CountryFolk
  | Blues
  | SoulFunk
  | Experimental
  | WorldMusic
  | Indie

enumGenreMarkers : List (Genre, (StyleClass, Marker))
enumGenreMarkers = genrePropMap enumGenre

genrePropMap : List Genre -> List (Genre, (StyleClass, Marker))
genrePropMap =
  List.map (\g -> (g, genreProperties g))

genreProperties : Genre -> (StyleClass, Marker)
genreProperties g =
  let c = genreColour g
  in (colourClass c, markerByColour c)

genreColour : Genre -> Colour
genreColour g =
  case g of
    Rock         -> Red
    Indie        -> Red
    Global       -> Cyan
    WorldMusic   -> Cyan
    Jazz         -> Purple
    SoulFunk     -> Purple
    Experimental -> Purple
    RnB          -> Purple
    HipHop       -> Purple
    Blues        -> Indigo
    Electronic   -> Lime
    Classical    -> Amber
    Punk         -> Black
    Metal        -> Black
    Pop          -> PaleRed
    Acoustic     -> Orange
    CountryFolk  -> Orange

decodeGenre : Decoder Genre
decodeGenre = 
  string
    |> andThen decodeGenreS

decodeGenreS : String -> Decoder Genre
decodeGenreS s =
  case s of
    "Rock"         -> succeed Rock
    "Global"       -> succeed Global
    "Jazz"         -> succeed Jazz
    "Electronic"   -> succeed Electronic
    "Hip Hop"      -> succeed HipHop
    "Classical"    -> succeed Classical
    "R&B"          -> succeed RnB
    "Punk"         -> succeed Punk
    "Metal"        -> succeed Metal
    "Pop"          -> succeed Pop
    "Acoustic"     -> succeed Acoustic
    "Country/Folk" -> succeed CountryFolk
    "Blues"        -> succeed Blues
    "Soul/Funk"    -> succeed SoulFunk
    "Experimental" -> succeed Experimental
    "World Music"  -> succeed WorldMusic
    "Indie"        -> succeed Indie
    _              -> fail <| "Unable to decode genre: " ++ s

encodeGenre : Genre -> String
encodeGenre s =
  case s of
    Rock         -> "Rock"
    Global       -> "Global"
    Jazz         -> "Jazz"
    Electronic   -> "Electronic"
    HipHop       -> "Hip Hop"
    Classical    -> "Classical"
    RnB          -> "R&B"
    Punk         -> "Punk"
    Metal        -> "Metal"
    Pop          -> "Pop"
    Acoustic     -> "Acoustic"
    CountryFolk  -> "Country/Folk"
    Blues        -> "Blues"
    SoulFunk     -> "Soul/Funk"
    Experimental -> "Experimental"
    WorldMusic   -> "World Music"
    Indie        -> "Indie"

enumGenre = [
    Rock
  , Global
  , Jazz
  , Electronic
  , HipHop
  , Classical
  , RnB
  , Punk
  , Metal
  , Pop
  , Acoustic
  , CountryFolk
  , Blues
  , SoulFunk
  , Experimental
  , WorldMusic
  , Indie
  ]