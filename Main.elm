import Assessment exposing (Kind(Assignment,Exam,Project,Final))
import AssessmentList exposing (update, view)
import StartApp.Simple exposing (start)

init : AssessmentList.Model
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

main =
  start
    { model = init
    , update = update
    , view = view
    }
