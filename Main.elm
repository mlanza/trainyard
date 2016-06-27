module Main exposing (..)
import Html exposing (Html, div, span, text)
import Html.App
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Char
import String exposing (fromChar)
import Dict exposing (Dict, singleton)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Set
import Keyboard exposing (KeyCode, ups)
import Debug exposing (log)

main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Coord = (Int, Int)
type alias Offset = (Int, Int)
type alias Piece = { color : String, coords : List Coord }
type alias Model = { lowerRightCorner : Coord, selected : Maybe Char, solution : List Piece, pieces : List (Maybe Char, Piece) }
type Msg = KeyUp KeyCode

scale : Int
scale = 3

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ ups KeyUp ]

up    = (0, -1)
left  = (-1, 0)
down  = (0, 1)
right = (1, 0)

direct : KeyCode -> Offset
direct key = 
  if key == 39 then right else
  if key == 40 then down  else
  if key == 37 then left  else
  if key == 38 then up    else (0, 0)

prepend str prefix = prefix ++ str

add : Offset -> Coord -> Coord
add (ox, oy) (cx, cy) = (cx + ox, cy + oy) 

build : (Coord -> Coord) -> List Coord -> Int -> Coord -> List Coord
build step coords count coord  =
  if count == 0 then coords else build step (coord :: coords) (count - 1) (step coord) 

row = build (add (1, 0)) [] 
col = build (add (0, 1)) [] 

spots : Coord -> Coord -> List Coord
spots coord (width, height) =
  row width coord |> List.concatMap (col height) |> List.sort

piece : Color -> Coord -> Coord -> Piece
piece color coord dimensions =
  { color = (colorToHex color), coords = spots coord dimensions }

init : (Model, Cmd Msg)
init = (
  { lowerRightCorner = (6, 6)
  , selected = Just 'a'
  , solution = [ piece Color.red (2, 3) (2, 1) ]
  , pieces = 
    [ (Just 'a', piece Color.red    (1, 3) (2, 1))
    , (Just 'b', piece Color.orange (0, 0) (1, 2))
    , (Just 'c', piece Color.green  (1, 0) (1, 3))
    ]
  }, Cmd.none)

occupied : Model -> List Coord
occupied model = .pieces model |> List.map snd |> List.concatMap .coords

colored : Piece -> List (String, Coord)
colored piece =  
  .coords piece |> List.map (\ coord -> (.color piece, coord))

solved : Model -> Bool
solved model =
  let
    have = .pieces model |> List.map snd |> List.concatMap colored |> Set.fromList
    want = .solution model |> List.concatMap colored |> Set.fromList
    same = Set.intersect have want
  in
    want == same

collisions : Model -> Bool
collisions model = 
  let
    occ  = occupied model
    cnt  = occ |> List.length
    ucnt = occ |> Set.fromList |> Set.toList |> List.length
  in
    cnt /= ucnt

unbound : Coord -> Coord -> Bool
unbound (cx, cy) (x, y) =
  x < 0 || y < 0 || x > cx || y > cy

breach : Model -> Bool
breach model =
  model 
  |> occupied 
  |> List.filter (unbound model.lowerRightCorner) 
  |> List.isEmpty 
  |> not

invalid : Model -> Bool
invalid model = (breach model) || (collisions model)

move : Offset -> Piece -> Piece
move offset piece  = 
  { piece | coords = List.map (add offset) piece.coords }

moveSelected : Offset -> Char -> (Maybe Char, Piece) -> (Maybe Char, Piece)
moveSelected offset ch (key, piece) =
  (key, if (Maybe.withDefault ' ' key) == ch then (move offset piece) else piece)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyUp key ->
      let
        k  = Char.fromCode key |> Char.toLower
        ch = model.selected |> Maybe.withDefault ' ' |> Char.toLower
        offset = direct key
        isPiece = offset == (0, 0)
        proposed = { model | pieces = model.pieces |> List.map (moveSelected offset ch) }
      in
        if isPiece then ({ model | selected = Just k }, Cmd.none)
        else (if (invalid proposed) then model else proposed, Cmd.none)

ems : Int -> String
ems number = scale * number |> toString |> prepend "em"

board : Coord -> Bool -> Coord -> ((Maybe Char, Piece) -> Bool) -> List (Maybe Char, Piece) -> Html Msg
board (x, y) solved lowerRightCorner selected pieces =
  let
    border = if solved then "solid 1px black" else "dotted 1px gray"
    divs = pieces |> List.concatMap (\ piece -> drawPiece (solved || (selected piece)) piece ) 
  in
    div [ style [ ("border", border)
                , ("width",  fst lowerRightCorner |> (+) 1 |> ems)
                , ("height", snd lowerRightCorner |> (+) 1 |> ems)
                , ("position", "absolute")
                , ("top", y |> ems)
                , ("left", x |> ems) 
                ] 
        ] divs

always : a -> b -> a
always value f = value

view : Model -> Html Msg
view model =
  let
    corner = (.lowerRightCorner model)
    (x, y) = corner
    pos1 = (1, 1)
    pos2 = add (x + 2, 0) pos1
  in
    div []
    [ board pos1 (solved model) corner (\ (key, piece) -> model.selected == key ) (.pieces model),
      board pos2 True corner (always True) (.solution model |> List.map (\ piece -> (Nothing, piece) )) ]

drawLabel letter =
  span [ 
    style [ 
      ("font-size", "1.5em"), 
      ("padding", ".5em") 
    ] 
  ] [
    text letter 
  ]

drawCoord : Bool -> Bool -> Maybe Char -> String -> Coord -> Html Msg
drawCoord labeled selected key color coord =
  let
    left = fst coord |> ems
    top = snd coord |> ems
    kc = Char.toCode (Maybe.withDefault ' ' key)
    letter = if labeled then fromChar (Maybe.withDefault ' ' key) else ""
  in
    div [
      style [ 
        ("position", "absolute"),
        ("top", top),
        ("left", left),
        ("opacity", if selected then "1" else ".4"),
        ("backgroundColor", color),
        ("height", 1 |> ems),
        ("width",  1 |> ems) 
      ],
      onClick (KeyUp kc)
    ] [ drawLabel letter ]

drawPiece : Bool -> (Maybe Char, Piece) -> List (Html Msg)
drawPiece selected (key, {color, coords}) =
  coords |> List.indexedMap (\ idx coord -> drawCoord (idx == 0) selected key color coord)