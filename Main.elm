import Effects exposing (Never)
import App exposing (init, update, view)
import StartApp
import Task

app =
  StartApp.start
    { init = init "1"
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks