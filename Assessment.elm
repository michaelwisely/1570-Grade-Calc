module Assessment (Assessment, init, Action, update, view) where

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)


-- MODEL

type alias Assessment =
  { earned : Int
  , worth : Int
  }

init : Assessment
init =
  { earned = 0
  , worth = 100
  }


-- UPDATE

type Action = Earned Int | Worth Int

toEarned : Int -> Action
toEarned x = Earned x

toWorth : Int -> Action
toWorth x = Worth x

update : Action -> Assessment -> Assessment
update action eval =
  case action of
    Earned x ->
      if x >= 0 && x <= eval.worth then
        Debug.watch "meh" { eval | earned <- x }
      else
        eval
    Worth x ->
      if x > 0 then
        { eval | worth <- x }
      else
        eval


-- VIEW

toMessage : Signal.Address Action -> Assessment -> (Int -> Action)-> String -> Signal.Message
toMessage address previous toAction  value =
  case toInt value of
    Ok intVal ->
      Signal.message address (toAction intVal)
    Err _ ->
      Signal.message address (toAction previous.earned)

view : Signal.Address Action -> Assessment -> Html
view address eval =
  let
    toMessage' = toMessage address eval
  in
    div []
      [ input
          [ value (toString eval.earned)
          , on "input" targetValue (toMessage' toEarned)
          , countStyle
          ]
        []
      , input
          [ value (toString eval.worth)
          , on "input" targetValue (toMessage' toWorth)
          , countStyle
          ]
        []
      , text (toString eval)
      ]

(=:) = (,)

countStyle : Attribute
countStyle =
  style
    [ "font-size" =: "20px"
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]
