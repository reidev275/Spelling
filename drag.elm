import Html exposing (..)
import Html.App as App
--import Html.Attributes exposing (..)
import Html.Events exposing (on)
--import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Letter as Letter 
import String exposing (split)

main = 
  App.program
    { init = init
    , view = view
    , update = Letter.update 
    , subscriptions = Letter.subscriptions
    }
 
type alias Model =
  { letters : List Letter.Model
  }


init : Model
init = 
  Model <| List.map Letter.init (split "" "no")

{-
type Msg
  = Letter Letter.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    Letter msg ->
-}   

  

-- VIEW

view : Model -> Html Letter.Msg
view model =
  div
    []
    [ text "D"
    ]
