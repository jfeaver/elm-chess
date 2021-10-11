module Tuple.Extra exposing (..)


add : ( number, number ) -> ( number, number ) -> ( number, number )
add ( a, b ) =
    Tuple.mapBoth ((+) a) ((+) b)
