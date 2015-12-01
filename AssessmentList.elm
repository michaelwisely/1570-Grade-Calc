module AssessmentList where

import Debug
import Assessment exposing (Kind(Assignment,Exam,Project,Final))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import String exposing (toInt)


-- MODEL

type alias Index = Int
type alias Model = List Assessment.Model

-- UPDATE

type Action = Update Index Assessment.Action

updateAtIndex : Int -> Assessment.Action -> Model -> Model
updateAtIndex index action model =
  let
    front = take index model
    back = drop (index + 1) model
    toUpdate = head (drop index model)
  in
    case toUpdate of
      Just item ->
        front ++ ((Assessment.update action item) :: back)
      Nothing ->
        model

update : Action -> Model -> Model
update action model =
  case action of
    Update index assessmentAction ->
      updateAtIndex index assessmentAction model

-- VIEW

sumResults : List (Result String Int) -> Result String Int
sumResults lst =
  List.foldl (Result.map2 (+)) (Ok 0) lst

toPercentage : Result String Int -> Int -> Result String Float
toPercentage lhs rhs =
  Result.map (\x -> x / (toFloat rhs)) (Result.map toFloat lhs)

averageResults : Model -> Result String Float
averageResults model =
  let
    numer = sumResults <| List.map .earned model
    denom = sum <| List.map .worth model
  in
    toPercentage numer denom

filterKind : Assessment.Kind -> Model -> Model
filterKind kind = List.filter (\x -> x.kind == kind)

resultList : List (Result a b) -> Result a (List b)
resultList results =
  case results of
    [] -> Ok []
    x :: xs ->
      case x of
        Ok val ->
          case resultList xs of
            Ok others -> Ok (val :: others)
            Err msg -> Err msg
        Err msg -> Err msg

replacedFinal : Model -> Result String Float
replacedFinal model =
  let
    percentage = \x -> (toFloat <| Result.withDefault 0 x.earned) / (toFloat x.worth)
    exams = filterKind Exam model
    finals = filterKind Final model
    highestExams = drop 1 (sortBy percentage exams)
    relevant = highestExams ++ finals
  in
    averageResults relevant

weightedAverage : Float -> Float -> Float -> Float
weightedAverage assignments project exams =
  0.4 * assignments + 0.1 * project + 0.5 * exams

courseAverage : Model -> Result String Float
courseAverage model =
  let
    assignments = averageResults <| filterKind Assignment model
    project = averageResults <| filterKind Project model
    exams = averageResults <| filterKind Exam model
    withFinal = replacedFinal model
  in
    case Result.map2 (>) exams withFinal of
      Ok betterExams ->
        if betterExams then
          Result.map3 weightedAverage assignments project exams
        else
          Result.map3 weightedAverage assignments project withFinal
      Err err ->
        Err err

view : Signal.Address Action -> Model -> Html
view address model =
  let
    counters = List.indexedMap (viewAssessment address) model
    assignmentAverage = averageResults <| filterKind Assignment model
    examAverage = averageResults <| filterKind Exam model
    withFinalAverage = replacedFinal model
  in
    div []
          [ table [] counters
          , p []
                [ text "Assignment Average: "
                , text (toString assignmentAverage)
                ]
          , p []
                [ text "Regular Exam Average: "
                , text (toString examAverage)
                ]
          , p []
                [ text "Replaced Final Exam Average: "
                , text (toString withFinalAverage)
                ]
          , p []
                [ text "Weighted Course Average: "
                , text (toString (courseAverage model))
                ]
          ]


viewAssessment : Signal.Address Action -> Index -> Assessment.Model -> Html
viewAssessment address index model =
  Assessment.view (Signal.forwardTo address (Update index)) model

chosen : Attribute
chosen = style [("color", "green")]

unchosen : Attribute
unchosen = style [("color", "black")]
