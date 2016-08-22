module Letter exposing (Msg, Model, update, view, init, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)


-- MODEL

type alias Model =
  { letter : String
  , position : Position
  , drag : Maybe Drag
  }


type alias Drag = 
  { start : Position
  , current : Position
  }

init : String -> Int -> Model
init s x =
  Model s (Position x 200) Nothing

-- UPDATE

type Msg
  = DragStart Position
  | DragAt Position
  | DragEnd Position

update : Msg -> Model -> Model
update msg ({letter, position, drag} as model) =
  case msg of
    DragStart xy ->
      Model letter position (Just (Drag xy xy))

    DragAt xy ->
      Model letter position (Maybe.map (\{start} -> Drag start xy) drag)

    DragEnd _ ->
      Model letter (getPosition model) Nothing


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing -> 
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


-- VIEW

(=>) = (,)

view : Model -> Html Msg
view model =
  let 
    realPosition = getPosition model
  in
    div
      [ onMouseDown
      , style
        [ "cursor" => "move"
        , "position" => "absolute"
        , "left" => px realPosition.x
        , "top" => px realPosition.y
        , "font-size" => "48px"
        ]
      ]
      [ text model.letter
      ]

px : Int -> String
px number =
  toString number ++ "px"

getPosition : Model -> Position
getPosition {letter, position, drag} =
  case drag of
    Nothing -> 
      position

    Just { start, current } ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)

