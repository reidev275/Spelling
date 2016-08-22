import Html exposing (..)
import Html.App as App
import Html.Events exposing (on)
import Mouse exposing (Position)
import Letter

main = 
  App.program
    { init = init
    , view = view
    , update = update 
    , subscriptions = subscriptions
    }
 
type alias Model =
  { letters : List IndexedLetter 
  , uid : Int
  }

type alias IndexedLetter =
  { id : Int
  , model : Letter.Model
  }


init : ( Model, Cmd Msg )
init = 
  ( Model 
    [ IndexedLetter 0 (Letter.init "h" 40)
    , IndexedLetter 1 (Letter.init "e" 0)
    , IndexedLetter 2 (Letter.init "l" 180)
    , IndexedLetter 3 (Letter.init "l" 160)
    , IndexedLetter 4 (Letter.init "o" 120)
    ] 0, Cmd.none)


-- UPDATE

type Msg
  = SubMsg Int Letter.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SubMsg id subMsg ->
      ( 
        { model 
          | letters = List.map (updateHelp id subMsg) model.letters
          }
        , Cmd.none )

updateHelp : Int -> Letter.Msg -> IndexedLetter -> IndexedLetter
updateHelp id msg letter =
  IndexedLetter
    id
    ( if letter.id == id
      then Letter.update msg letter.model
      else letter.model )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch (List.map subHelp model.letters)

subHelp : IndexedLetter -> Sub Msg
subHelp { id, model } = 
  Sub.map (SubMsg id) (Letter.subscriptions model)

-- VIEW

view : Model -> Html Msg
view model =
  div
    []
    [ div
      []
      [ text "drag the letters to spell 'hello'" ]
    , div
      []
      (List.map viewLetter model.letters)
    ]


    
 
viewLetter : IndexedLetter -> Html Msg
viewLetter {id, model} =
  App.map (SubMsg id) (Letter.view model)
