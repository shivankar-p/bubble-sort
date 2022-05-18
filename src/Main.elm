module Main exposing (..)
import Css exposing (..)
import Browser
import Html exposing (Html, button, div, form, h1, input, text, Attribute)
import Html.Attributes exposing (id, type_, style)
import Html.Events exposing (onClick)
import Shapes
import Array
import List.Extra exposing (..)
import List exposing (..)
import Random
import Basics exposing (..)

-- MAIN

main =
  Browser.element { init = init
                    , update = update
                    , subscriptions = subscriptions
                    , view = view 
                  }



-- MODEL
type Msg = Swap | Decrement_b | Increment_i | Reset_i | Reset | Undo

type alias State= 
    {
          b : Int
        , i : Int
        , arr : List (Int)
        , sorted_array : List (Int)
        , logs: List String
        , prev_i: List Int
    }

init : () -> ( State, Cmd Msg )
init _ =
    ( let
        lst = [28, 34, 16, 1, 40]
      in
      {
       i = 0
      , arr = lst
      , b = length lst
      , sorted_array = sort lst
      , logs = []
      , prev_i = []
      }
    , Cmd.none
    )


subscriptions : State-> Sub Msg
subscriptions state =
  Sub.none


-- UPDATE


update : Msg -> State-> (State, Cmd Msg)
update msg state =
  case msg of
    Decrement_b ->
        if state.b==0 then 
          (state, Cmd.none)
        else 
          ( 
            { state | b = state.b-1
              , logs = state.logs ++ ["Decrement_b"] 
            }
            , Cmd.none
           )
    Increment_i -> 
        if state.i==3 then 
          (state, Cmd.none)
        else 
          ( { state | i = state.i+1, logs = state.logs ++ ["Increment_i"] }, Cmd.none )
    Reset_i ->
        ( 
          { 
            state | prev_i = append state.prev_i [state.i]
            , i = 0
            , logs = state.logs ++ ["Reset_i"]
          }
          , Cmd.none 
        )
    Swap ->
        ({ state | arr = (swapAt state.i ((state.i)+1) state.arr), logs = state.logs ++ ["Swap"] }, Cmd.none)
    Reset ->
        init()
    Undo ->
        if Maybe.withDefault "" (last state.logs) == "Swap" then
          (
            { 
              state | arr = (swapAt state.i ((state.i)+1) state.arr)
            , logs = removeAt ((length state.logs)-1) state.logs 
            }
            , Cmd.none
          )
        else if Maybe.withDefault "" (last state.logs) == "Increment_i" then
          ( 
            { 
              state | i = state.i-1 
            ,  logs = removeAt ((length state.logs)-1) state.logs
            }
            , Cmd.none
          )
        else if Maybe.withDefault "" (last state.logs) == "Decrement_b" then
          ( 
            { 
              state | b = state.b+1 
              ,  logs = removeAt ((length state.logs)-1) state.logs
            }
            , Cmd.none 
          )
        else if Maybe.withDefault "" (last state.logs) == "Reset_i" then
          ( 
            { 
              state | i = Maybe.withDefault 0 (getAt ((length state.prev_i)-1) state.prev_i)
              ,  logs = removeAt ((length state.logs)-1) state.logs
              ,  prev_i = removeAt ((length state.prev_i)-1) state.prev_i
            }
            , Cmd.none 
          )
        else
          (state, Cmd.none)
          



-- VIEW
--button [ onClick Increment_i ] [ text "increase i"]

buttonStyle : List (Attribute msg)
buttonStyle =
    [ style "width" "95px"
    , style "background-color" "#397cd5"
    , style "color" "white"
    , style "padding" "14px 20px"
    , style "margin-left" "10px"
    , style "border" "none"
    , style "border-radius" "4px"
    , style "font-size" "10px"
    ]

buttonStyle2 : List (Attribute msg)
buttonStyle2 =
    [ style "width" "75px"
    , style "background-color" "#808080"
    , style "color" "black"
    , style "padding" "14px 20px"
    , style "margin-left" "10px"
    , style "border" "none"
    , style "border-radius" "4px"
    , style "font-size" "12px"
    ]

view state =
  div []
    [ if state.arr == state.sorted_array then
        div ([style "padding-left" "30cm"]++[style "padding-top" "1cm"])[text "Array sorted!"]
      else
        div ([style "padding-left" "30cm"]++[style "padding-top" "1cm"])[text "Array Not sorted!"]


      , div ([style "padding-left" "1cm"]++[style "padding-top" "2cm"])
         [
           text ("index(i): " ++ String.fromInt state.i)
         ]
    , div [style "padding-left" "1cm"][ text ("boundary(b): " ++ String.fromInt state.b) ] 
    , div [style "padding-left" "6cm"]
      [
          if state.i == 0 then
            Shapes.getshape "red" (getAt 0 state.arr) 0 "i" (getAt 0 state.sorted_array)
          else
            Shapes.getshape "black" (getAt 0 state.arr) 0 "" (getAt 0 state.sorted_array)
          ,
          if state.i == 1 || state.i == 0 then
            if state.i == 1 then
              Shapes.getshape "red" (getAt 1 state.arr) 1 "i" (getAt 1 state.sorted_array)
            else 
              Shapes.getshape "red" (getAt 1 state.arr) 1 "" (getAt 1 state.sorted_array)
          else
            Shapes.getshape "black" (getAt 1 state.arr) 1 "" (getAt 1 state.sorted_array)
          
          ,
          if state.i == 1 || state.i == 2 then
            if state.i == 2 then
              Shapes.getshape "red" (getAt 2 state.arr) 2 "i" (getAt 2 state.sorted_array)
            else 
              Shapes.getshape "red" (getAt 2 state.arr) 2 "" (getAt 2 state.sorted_array)
          else
            Shapes.getshape "black" (getAt 2 state.arr) 2 "" (getAt 2 state.sorted_array)
          
          ,
          if state.i == 2 || state.i == 3 then
            if state.i == 3 then
              Shapes.getshape "red" (getAt 3 state.arr) 3 "i" (getAt 3 state.sorted_array)
            else
              Shapes.getshape "red" (getAt 3 state.arr) 3 "" (getAt 3 state.sorted_array)
          else
            Shapes.getshape "black" (getAt 3 state.arr) 3 "" (getAt 3 state.sorted_array)

          ,
          if state.i == 3 then
            Shapes.getshape "red" (getAt 4 state.arr) 4 "" (getAt 4 state.sorted_array)
          else
            Shapes.getshape "black" (getAt 4 state.arr) 4 "" (getAt 4 state.sorted_array)
      ]
    , div([style "padding-left" "1cm"])
      [
           button ([ onClick Swap ]++buttonStyle) [ text "swap" ]
        ,  button ([ onClick Increment_i ]++buttonStyle) [ text "increase i" ]  
        , button ([ onClick Decrement_b ]++buttonStyle) [ text "Decrease b" ]
        , button ([ onClick Reset_i ]++buttonStyle) [ text "Reset i" ]
      ]
    , div([style "padding-left" "25cm"])
      [   
          if (length state.logs /= 0) then
            button ([ onClick Undo ]++buttonStyle2) [ text "Undo" ]
          else
            div[][]
        , button ([ onClick Reset ]++buttonStyle2) [ text "Reset" ]
      ]
    ]