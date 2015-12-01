module Assessment (Model, Kind(Assignment,Project,Exam,Final), Action, update, view) where

import Debug
import Maybe exposing (Maybe(Just, Nothing))
import Result exposing (Result(Ok, Err))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)


-- MODEL

type Kind = Assignment | Project | Exam | Final
type alias Model =
  { earned : Result String Int
  , worth : Int
  , kind : Kind
  , name : String
  }


-- UPDATE

type Action = SetEarned String

validateEarned : String -> Model -> Model
validateEarned val model =
  case toInt val of
    Ok earned ->
      if earned >= 0 && earned <= model.worth then
        { model | earned = Ok earned }
      else
        { model | earned = Err "Out of range" }
    Err _ ->
      { model | earned = Err "Not an integer" }

update : Action -> Model -> Model
update action model =
  case action of
    SetEarned enteredValue ->
      validateEarned enteredValue model

-- VIEW

toMessage : Signal.Address Action -> String -> Signal.Message
toMessage address value =
  Signal.message address (SetEarned value)

view : Signal.Address Action -> Model -> Html
view address model =
  let
    toMessage' = toMessage address
  in
    tr []
      [ td [] [ text (model.name ++ " ") ]

      , td [] [ input
                    [ value (toString model.earned)
                    , on "input" targetValue toMessage'
                    ]
                    []
              , text (" / " ++ (toString model.worth))
              ]
      ]
