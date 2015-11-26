module AssessmentList where

import Debug
import Assessment exposing (Kind(Assignment,Exam,Project,Final))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)


-- MODEL

type alias Index = Int
type alias Model = List Assessment.Model

init : Model
init =
    [ { name = "Assignment 1", kind = Assignment, earned = 0, worth = 50 }
    , { name = "Assignment 2", kind = Assignment, earned = 0, worth = 100 }
    , { name = "Assignment 3", kind = Assignment, earned = 0, worth = 100 }
    , { name = "Assignment 4", kind = Assignment, earned = 0, worth = 100 }
    , { name = "Assignment 5", kind = Assignment, earned = 0, worth = 100 }
    , { name = "Assignment 6", kind = Assignment, earned = 0, worth = 100 }
    , { name = "Assignment 7", kind = Assignment, earned = 0, worth = 100 }
    , { name = "Assignment 8", kind = Assignment, earned = 0, worth = 100 }
    , { name = "Assignment 9", kind = Assignment, earned = 0, worth = 100 }

    , { name = "Assignment 10", kind = Project, earned = 0, worth = 100 }

    , { name = "Exam 1", kind = Exam, earned = 0, worth = 100 }
    , { name = "Exam 2", kind = Exam, earned = 0, worth = 100 }
    , { name = "Exam 3", kind = Exam, earned = 0, worth = 100 }

    , { name = "Final", kind = Final, earned = 0, worth = 150 }
    ]

-- UPDATE

type Action = Update Index Assessment.Action

update : Action -> Model -> Model
update (Update index action) model =
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

-- VIEW

ssum : (Assessment.Model -> Int) -> List Assessment.Model -> Float
ssum acc lst = toFloat << sum << map acc <| lst

average : Assessment.Kind -> Model -> Float
average kind model =
  let
    relevant = filter (\x -> x.kind == kind) model
  in
    (ssum .earned relevant) / (ssum .worth relevant)

replacedFinal : Model -> Float
replacedFinal model =
  let
    percentage = (\x -> (toFloat x.earned) / (toFloat x.worth))
    exams = filter (\x -> x.kind == Exam) model
    finals = filter (\x -> x.kind == Final) model
    highestExams = drop 1 (sortBy percentage exams)
    relevant = highestExams ++ finals
  in
    (ssum .earned relevant) / (ssum .worth relevant)

courseAverage : Model -> Float
courseAverage model =
  let
    assignments = average Assignment model
    project = average Project model
    exams = average Exam model
    withFinal = replacedFinal model
    betterExam = if exams > withFinal then exams else withFinal
  in
    0.4 * assignments + 0.1 * project + 0.5 * betterExam

view : Signal.Address Action -> Model -> Html
view address model =
  let
    counters = List.indexedMap (viewAssessment address) model
    assignmentAverage = average Assignment model
    examAverage = average Exam model
    withFinalAverage = replacedFinal model
    finalHelps = withFinalAverage > examAverage
  in
    div []
          [ table [] counters
          , p []
                [ text "Assignment Average: "
                , text (toString assignmentAverage)
                ]
          , p [if finalHelps then unchosen else chosen]
                [ text "Regular Exam Average: "
                , text (toString examAverage)
                ]
          , p [if finalHelps then chosen else unchosen]
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
