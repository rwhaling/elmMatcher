import Html exposing (Html, button, div, text, span)
import Html.App as Html
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List
import List.Extra exposing (groupWhile, replaceIf)
import Debug
import Json.Decode exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type Player = XPlayer | OPlayer
type BoxState = Empty | X | O
type alias Move = { pos : Pos, player : Player }
type alias Pos = { state: BoxState, row: Int, col: Int }

type alias Model = { currentPlayer: Player, grid: List Pos }

range : Int -> Int -> List Int
range x y = makeRange x y []

makeRange : Int -> Int -> List Int -> List Int
makeRange start stop l = 
  if stop > start then
    let 
      next : Int 
      next = stop - 1
    in 
      List.concat [makeRange start next [], stop :: l]
  else if stop == start then
    stop :: l
  else
    l

intToPos : Int -> Pos
intToPos n =
  { state=Empty, row=(n // 3) + 1, col=(n % 3) + 1 }

model : Model
model =
  let 
    nine = range 0 8
    logged = Debug.log "nine" nine
  in
    { currentPlayer=XPlayer, grid=List.map intToPos nine }


-- UPDATE

type alias Msg = Pos

doMove newPos pos =
  if ( ( pos.row == newPos.row) && ( pos.col == newPos.col) ) then newPos
  else pos

update : Msg -> Model -> Model
update msg model =
  let 
    log = Debug.log "msg" (toString msg.state)
    nextPlayer = 
      case msg.state of 
        X -> OPlayer
        O -> XPlayer
        Empty -> XPlayer
    nextGrid = List.map (doMove msg) model.grid
  in
    { currentPlayer=nextPlayer, grid=nextGrid }

-- VIEW

posToText newState pos =
  case pos.state of 
    Empty -> button [ style [("margin","5px")], 
                      onClick { state=newState, row=pos.row, col=pos.col } ] [text "[ ]"]
    X -> button [ style [("margin","5px")] ] [text "[X]"]
    O -> button [ style [("margin","5px")] ] [text "[O]"]

rowToHTML currentPlayer row = 
  let newState = 
    case currentPlayer of 
      XPlayer -> X
      OPlayer -> O
  in
    div [] (List.map (posToText newState) row)

gridToHTML grid currentPlayer = 
  let
    rows = groupWhile (\x y -> x.row == y.row) grid
  in 
    div [] (List.map (rowToHTML currentPlayer) rows)

view : Model -> Html Msg
view model =
  div [ style [("margin","30px") ] ] [ gridToHTML model.grid model.currentPlayer ]