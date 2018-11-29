import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Url.Builder as Url
import Styles exposing (..)
import Random
import Array



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

characterList : List Character
characterList = 
    List.map Character ["Mario","Luigi","Bowser","Bowser Jr", "Pit", "Dark Pit", "Lucas", "Ness"]

type alias Model =
  { charOne: Character
  , charTwo: Character
  , lastPickedChar : Character
  }

type alias Character = 
    { name: String}

init : () -> (Model, Cmd Msg)
init _ =
    let
        char1 = Character "Mario"
        char2 = Character "Bowser Jr"
    in
    
  ( Model 
  char1 
  char2
  char1
  , Cmd.none
  )

-- UPDATE



type Msg
  = CharacterPicked Character
  | RollCharOne Int
  | RollCharTwo Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CharacterPicked char->
        ({model | lastPickedChar = char},
         Cmd.batch
            [Random.generate RollCharOne randomInt
            ,Random.generate RollCharTwo randomInt])
    RollCharOne int ->
        ({model | charOne = charNumber int},
        Cmd.none)
    RollCharTwo int ->
         ({model | charTwo = charNumber int},
        Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP

--"https://www.smashbros.com/assets_v2/img/fighter/thumb_v/mario.png"
toSmashUrl : String -> String
toSmashUrl char =
  Url.crossOrigin "https://www.smashbros.com" ["assets_v2","img","fighter","thumb_v",char ++ ".png"]
  []

-- STUFF 

urlForChar : Character -> String
urlForChar char = 
    toSmashUrl (urlName char)

    
urlName : Character -> String
urlName char =
    String.toLower (String.map(\c -> if c == ' ' then '_' else c) char.name)

randomInt : Random.Generator Int
randomInt =
    Random.int 1 (List.length characterList)

charNumber : Int -> Character
charNumber int   =
    let 
        maybeChar = Array.get int (Array.fromList (List.filter charNotUsed characterList))
    in
        case maybeChar of
            Nothing ->
                Character "Mario"
            Just char ->
                char

-- Dont want to show the same ones again. Dropped for now
charNotUsed : Character  -> Bool
charNotUsed char =
    True

    -- VIEW


view : Model -> Html Msg
view model =
    div []  
    [
    div [class "gridContainer", style "display" "flex"]
        [ 
        div [class "gridElement"]
        [
            img [src (urlForChar model.charOne), alt "char1" , onClick (CharacterPicked model.charOne)][]
        ]
        , div [class "gridElement"]
        [
            
            img [src (urlForChar model.charTwo), alt "char2", onClick (CharacterPicked model.charTwo)][]
        ]
        ]
    , div []
    [text model.lastPickedChar.name]
    ]
