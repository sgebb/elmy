module Minesweep exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Array exposing (Array)



-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL


type alias Model =
  { 
    board : Board,
    timeleft : Int,
    dead : Bool
  }

type alias Board = Array ( Array Tile)

type alias Tile =
   {
        isBomb: Bool,
        isClicked: Bool
   }


indexOf : Array Tile -> Tile -> Int
indexOf array tile = 
    let
        indexedList = Array.toIndexedList array
        filtered = elementWithIndex indexedList tile
    in 
        filtered |> Tuple.first

elementWithIndex : List (Int, Tile) -> Tile -> (Int, Tile)
elementWithIndex list tile =
    let
        something = List.filter ((==) tile) (List.map Tuple.second list)
    in
        something

testTile : Tile
testTile = 
    Tile True False

testTile2 : Tile
testTile2 = 
    Tile False False
 
testBoard : Board
testBoard = 
    Array.push (Array.push testTile2 (Array.push testTile Array.empty))  Array.empty 

areNeighbours : Tile -> Tile -> Bool
areNeighbours tile other =
    abs(tile.locationX - other.locationX) < 2 &&
    abs(tile.locationY - other.locationY) < 2 &&
    tile /= other

numberOfNeighbourBombs : Tile -> Model -> Int
numberOfNeighbourBombs tile model  = 
    1

isBomb : Tile -> Bool
isBomb tile =
    tile.isBomb

init : () -> (Model, Cmd Msg)
init _ =
  ( Model testBoard 100 False
  , Cmd.none
  )


-- UPDATE

type Msg
  = None
  | Tick Time.Posix
  | TileClicked Tile
  | Die

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    if model.dead then
        (model, Cmd.none)
    else
        case msg of
            None ->
                (model, Cmd.none)
            Tick newTime ->
                if model.timeleft > 0 then
                    ( { model | timeleft = model.timeleft - 1}
                    , Cmd.none
                    )
                else update Die model
            TileClicked tile ->
                ({model | board = tileClicked model.board tile}, Cmd.none)
            Die -> 
                ({model | dead = True}, Cmd.none)

clickTile :  Tile -> Tile
clickTile tile = 
    {tile | isClicked = True}

tileClicked : Board -> Tile -> Board
tileClicked tiles tile =
    let
        clickIfSame other =     
            if other == tile then
                clickTile tile
            else
                other
    in
        Array.map (Array.map clickIfSame) tiles

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = 
    --if model.dead
    --then 
        Sub.none
    --else Time.every 1000 Tick

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [
        text (String.fromInt model.timeleft),
        div [][
            viewBoard model
        ]
        ]

viewBoard : Model -> Html Msg
viewBoard model =
    let
        funcArray = Array.toList(Array.map rowView (model.board))
    in
    
    table [] (List.map (evalFunc model) funcArray)
                

rowView : Array Tile -> Model -> Html Msg
rowView array model = 
    let
        funcArray = Array.toList(Array.map cellView array)
    in
        tr [] (List.map (evalFunc model) funcArray)


cellView : Tile -> Model -> Html Msg
cellView tile model = 
    if tile.isClicked then
        if tile.isBomb then
            td [] [text "X"]
        else    
            td [] [text (String.fromInt (numberOfNeighbourBombs tile model))]
    else 
        td [] [button [onClick (TileClicked tile)][]]

evalFunc : Model -> (Model -> Html Msg)  -> Html Msg
evalFunc  model func =
    func model
    
    
--HELPER
find : a -> Array a -> Maybe Int
find item array = 
  Tuple.second <| Array.foldl (\currentItem (i, match) ->
    if currentItem == item then
      (i + 1, Just i)
    else
      (i + 1, match)
  ) (0, Nothing) array
  
findArray : a -> Array (Array a) -> Maybe Int
findArray item array = 
    Tuple.second <| Array.foldl (\currentArray (i, match) ->
    case (find item currentArray) of 
      Just _ -> 
        (i + 1, Just i)
      Nothing ->
        (i + 1, match)
  ) (0, Nothing) array

coordinates : a -> Array (Array a) -> (Int,Int)
coordinates item board =
    let
        maybeCol = findArray item board
        col = maybeCol |> Maybe.withDefault 0
        maybeRow = find item ((Array.get col board)|> Maybe.withDefault Array.empty)
        row = maybeRow |> Maybe.withDefault 0
    in
        (row, col)
