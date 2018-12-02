import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Url.Builder as Url
import Css exposing (..)
import Random
import Array
import Browser.Dom as Dom



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
    List.map Character ["Mario","Link", "Kirby", "Yoshi","Donkey Kong","Samus"
    ,"Fox","Pikachu","Jigglypuff","Captain Falcon","Ness","Luigi", "Peach","Bowser"
    ,"Zelda","Sheik","Pichu","Falco","Dr Mario", "Ice Climbers", "Young Link"
    , "Marth","Ganondorf","Mewtwo","Roy","Mr Game and Watch","Pit","Meta Knight"
    ,"Wario","Lucas","Ike","Pokemon Trainer","Snake","Sonic","Zero Suit Samus"
    ,"Olimar","Diddy Kong","Lucario","Toon Link","Wolf","ROB","Mega Man"
    ,"Rosalina and Luma","Villager","Palutena","Little Mac","Dark Pit","Lucina"
    ,"Greninja","Wii Fit Trainer","Pac-Man","Robin","Bowser Jr","Shulk","Ryu","Cloud"
    ,"Corrin","Bayonetta","Inkling","Ridley","Simon Belmont","Daisy","Richter","Chrom"
    ,"Dark Samus","King K Rool","Isabelle","Ken","Incineroar"]

type alias Model =
  { charOne: Character
  , charTwo: Character
  , lastPickedChar : Character
  }

type alias Character = 
    { name: String}

nullCharacter : Character
nullCharacter =
    Character ""

init : () -> (Model, Cmd Msg)
init _ =
    let
        char1 = nullCharacter
        char2 = nullCharacter
    in
    
  ( Model 
  char1 
  char2
  nullCharacter
  , Cmd.batch
            [Random.generate RollCharOne randomCharNumber
            ,Random.generate RollCharTwo randomCharNumber]
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
            [Random.generate RollCharOne randomCharNumber
            ,Random.generate RollCharTwo randomCharNumber])
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


--"https://www.smashbros.com/assets_v2/img/fighter/isabelle/main.png"
bigSmashUrl : String -> String
bigSmashUrl char = 
    Url.crossOrigin "https://www.smashbros.com" ["assets_v2","img","fighter",char,"main.png"] []

-- STUFF 

urlForChar : Character -> String
urlForChar char = 
    bigSmashUrl (urlName char)

    
urlName : Character -> String
urlName char =
    String.toLower (String.map(\c -> if c == ' ' then '_' else c) char.name)

randomCharNumber : Random.Generator Int
randomCharNumber =
    Random.int 0 ((List.length characterList - 1))

charNumber : Int -> Character
charNumber int   =
    let 
        maybeChar = Array.get int (Array.fromList (List.filter charNotUsed characterList))
    in
        case maybeChar of
            Nothing ->
                nullCharacter
            Just char ->
                char

-- Dont want to show the same ones again. Dropped for now
charNotUsed : Character  -> Bool
charNotUsed char =
    True

-- VIEW


view : Model -> Html Msg
view model =
    div [ class  "container" ]
    [
        header [class "item", id "header-item"]
        [
            h1[][text "Who wins?"]
        ]
        , div [class "item", id "leftChar-item"]
        [
            img [class "charImage", src (urlForChar model.charOne), alt "char1" , onClick (CharacterPicked model.charOne)][]
        ]
        , h1 [class "item", id "vstext-item"][text "VS"]
        , div [class "item", id "rightChar-item"]
        [
            
            img [class "charImage", src (urlForChar model.charTwo), alt "char2", onClick (CharacterPicked model.charTwo)][]
        ]
        , div [class "item", id "underText-item"] [text (youPickedText model.lastPickedChar)]
    ]

youPickedText : Character -> String
youPickedText char =
    if char == nullCharacter then
        ""
    else
        "You picked " ++ char.name

