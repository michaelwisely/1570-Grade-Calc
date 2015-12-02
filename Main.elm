import Assessment exposing (Kind(Assignment,Exam,Project,Final))
import AssessmentList exposing (update, view)
import StartApp.Simple exposing (start)
import Html

init : AssessmentList.Model
init =
    [ { name = "Assignment 1", kind = Assignment, earned = Err "Blank!", worth = 50 }
    , { name = "Assignment 2", kind = Assignment, earned = Err "Blank!", worth = 100 }
    , { name = "Assignment 3", kind = Assignment, earned = Err "Blank!", worth = 100 }
    , { name = "Assignment 4", kind = Assignment, earned = Err "Blank!", worth = 100 }
    , { name = "Assignment 5", kind = Assignment, earned = Err "Blank!", worth = 100 }
    , { name = "Assignment 6", kind = Assignment, earned = Err "Blank!", worth = 100 }
    , { name = "Assignment 7", kind = Assignment, earned = Err "Blank!", worth = 100 }
    , { name = "Assignment 8", kind = Assignment, earned = Err "Blank!", worth = 100 }
    , { name = "Assignment 9", kind = Assignment, earned = Err "Blank!", worth = 100 }

    , { name = "Assignment 10", kind = Project, earned = Err "Blank!", worth = 100 }

    , { name = "Exam 1", kind = Exam, earned = Err "Blank!", worth = 100 }
    , { name = "Exam 2", kind = Exam, earned = Err "Blank!", worth = 100 }
    , { name = "Exam 3", kind = Exam, earned = Err "Blank!", worth = 100 }

    , { name = "Final", kind = Final, earned = Err "Blank!", worth = 150 }
    ]

main : Signal Html.Html
main =
  start
    { model = init
    , update = update
    , view = view
    }
