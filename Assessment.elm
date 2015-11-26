module Assessment (Model, Kind(Assignment,Project,Exam,Final), Action, update, view) where

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)


-- MODEL

type Kind = Assignment | Project | Exam | Final
type alias Model =
  { earned : Int
  , worth : Int
  , kind : Kind
  , name : String
  }


-- UPDATE

type Action = Earned Int | Worth Int

update : Action -> Model -> Model
update action model =
  case action of
    Earned x ->
      if x >= 0 && x <= model.worth then
        { model | earned <- x }
      else
        model
    Worth x ->
      if x > 0 then
        { model | worth <- x }
      else
        model


-- VIEW

toMessage : Signal.Address Action -> Model -> (Int -> Action)-> String -> Signal.Message
toMessage address previous toAction  value =
  case toInt value of
    Ok intVal ->
      Signal.message address (toAction intVal)
    Err _ ->
      Signal.message address (toAction previous.earned)

view : Signal.Address Action -> Model -> Html
view address model =
  let
    toMessage' = toMessage address model
  in
    tr []
      [ td [] [ text (model.name ++ " ") ]

      , td [] [ input
                    [ value (toString model.earned)
                    , on "input" targetValue (toMessage' Earned)
                    ]
                    []
              , text (" / " ++ (toString model.worth))
              ]
      ]
