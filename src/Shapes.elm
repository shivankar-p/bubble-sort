module Shapes exposing (getshape)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array
import Maybe exposing (..)


getshape a b c d e = svg
    [ viewBox "0 0 200 200"
    , width "200"
    , height "200"
    ]
    [ circle
        [ cx "50"
        , cy "50"
        , r "40"
        , if b == e then 
            fill "#32CD32"
          else
            fill "yellow"
        , stroke a
        , strokeWidth "3"
        ]
        []
        , text_
        [ x "50"
        , y "50"
        , fill "black"
        , textAnchor "middle"
        , dominantBaseline "central"
        ]
        [ text (String.fromInt (withDefault 0 b))
        ]
        , circle
        [ cx "50"
        , cy "130"
        , fill "white"
        , r "20"
        , if a == "red" then
            stroke a
          else 
            stroke "white"
        , strokeWidth "3"
        ]
        []
        , text_
        [ x "50"
        , y "130"
        , fill "black"
        , textAnchor "middle"
        , dominantBaseline "central"
        ]
        [ text (String.fromInt c)]
        , text_
        [ x "50"
        , y "170"
        , fill "black"
        , textAnchor "middle"
        , dominantBaseline "central"
        ]
        [ text d]
    ]

