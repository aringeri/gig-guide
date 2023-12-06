module DateRange exposing (..)

import Date exposing (..)

type alias DateRange =
  { min : Date
  , max : Date
  }

makeDateRange : Date -> Int -> Unit -> DateRange
makeDateRange min i u =
  DateRange min <| add u i min

isInRange : Date -> DateRange -> Bool
isInRange d r =
  isBetween r.min r.max d