import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, string, index, int, map2, list)
import Json.Encode as Encode
import Url.Builder as Url
import Random
import Array
import Browser.Dom as Dom
import Task
import Tuple



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

-- characterList : List Character
-- characterList = 
--     List.map Character  ["Mario","Link", "Kirby", "Yoshi","Donkey Kong","Samus"
--     ,"Fox","Pikachu","Jigglypuff","Captain Falcon","Ness","Luigi", "Peach","Bowser"
--     ,"Zelda","Sheik","Pichu","Falco","Dr Mario", "Ice Climbers", "Young Link"
--     , "Marth","Ganondorf","Mewtwo","Roy","Mr Game and Watch","Pit","Meta Knight"
--     ,"Wario","Lucas","Ike","Pokemon Trainer","Snake","Sonic","Zero Suit Samus"
--     ,"Olimar","Diddy Kong","Lucario","Toon Link","Wolf","ROB","Mega Man"
--     ,"Rosalina and Luma","Villager","Palutena","Little Mac","Dark Pit","Lucina"
--     ,"Greninja","Wii Fit Trainer","Pac-Man","Robin","Bowser Jr","Shulk","Ryu","Cloud"
--     ,"Corrin","Bayonetta","Inkling","Ridley","Simon Belmont","Daisy","Richter","Chrom"
--     ,"Dark Samus","King K Rool","Isabelle","Ken","Incineroar"] []

-- Mocklist that works with new model
-- characterList : List Character
-- characterList =
--     [Character 1 "Mario" [], Character 2 "Bowser Jr" [], Character 3 "Mewtwo" []]

type alias Model =
  { charOne: Character
  , charTwo: Character
  , lastPickedChar : Character
  , lastNotPickedChar: Character
  , charList: List Character
  , lastResult: MatchResult
  }

type alias Character = 
    { id: Int,
    name: String}

type alias MatchResult = 
    { winVotes: Int
    , loseVotes: Int}

nullCharacter : Character
nullCharacter =
    Character 0 ""

nullResult : MatchResult
nullResult = 
    MatchResult 0 0

init : () -> (Model, Cmd Msg)
init _ =
    let 
        a = Cmd.batch [getCharacters]
        char1 = nullCharacter
        char2 = nullCharacter
    in
    
  ( Model 
  char1 
  char2
  nullCharacter
  nullCharacter
  []
  nullResult
  ,getCharacters
  )

-- UPDATE

type Msg
  = CharPicked Character Character
  | GotCharacters (Result Http.Error (List Character))
  | GotResult (Result Http.Error (MatchResult))
  | CharsRolled (Character, Character)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotCharacters result ->
        case result of
            Err _ ->
                (model, Cmd.none)
            Ok charList ->
                ({model | charList = charList}, generateNewPicks charList)
    CharPicked winChar loseChar ->
        ({model | lastPickedChar = winChar, lastNotPickedChar = loseChar},
        Cmd.batch [generateNewPicks model.charList, castVote winChar loseChar])
    CharsRolled tuple ->
        ({model | charOne = Tuple.first tuple, charTwo = Tuple.second tuple}, Cmd.none)
    GotResult result ->
        case result of
            Err _ ->
                ({model | lastResult = nullResult}, Cmd.none)
            Ok res ->
                ({model | lastResult = res}, Cmd.none)

generateNewPicks : List Character -> Cmd Msg
generateNewPicks charlist = 
    Random.generate CharsRolled (twoDifferent charlist)

-- voteWinner : Model -> Cmd Msg
-- voteWinner = 


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

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

takeAny : List Character -> Random.Generator Character
takeAny list =
    case list of
        first :: rest ->
            Random.uniform first rest

        [] ->
            -- won't happen
            Random.constant nullCharacter

twoDifferent : List Character -> Random.Generator (Character,  Character)
twoDifferent charlist =
    takeAny charlist
        |> Random.andThen
            (\first ->
                Random.map (\second ->  ( first, second )) <|
                    takeAny (List.filter (\x -> x /= first) charlist)
            )


-- VIEW


view : Model -> Html Msg
view model =
    div [ class  "container", id "site" ]
    [
        header [class "item", id "header-item"]
        [
            h1[][text "Who wins?"]
        ]
        , nav [class "item", id "nav"][text "nav"]
        , div [class "item", id "leftChar-item"]
        [
            img [class "charImage",  src (urlForChar model.charOne), alt "char1" , onClick (CharPicked model.charOne model.charTwo)][]
        ]
        , h1 [class "item", id "vstext-item"][text "VS"]
        , div [class "item", id "rightChar-item"]
        [
            
            img [class "charImage",  src (urlForChar model.charTwo), alt "char2", onClick (CharPicked model.charTwo model.charOne)][]
        ]
        , div [class "item", id "underText-item"] [text (youPickedText model.lastPickedChar model.lastResult)]
        --, div [] [debugList model.charList]
        --, div [] [text (model.charOne.name ++ model.charTwo.name)]
    ]

youPickedText : Character -> MatchResult -> String
youPickedText char res =
    if char == nullCharacter then
        ""
    else
        "You picked " ++ char.name ++". Total votes for " ++ char.name ++ ": " ++ String.fromInt(res.winVotes) ++ ". Votes against: " ++ String.fromInt(res.loseVotes)

debugList: List Character -> Html Msg
debugList charList =     
    ul [] (List.map (\l -> li [] [text l.name]) charList) 

-- HTTP
getCharacters : Cmd Msg
getCharacters =
    Http.get{ 
        url = "https://smashcountdown.azurewebsites.net/characters"
        , expect = Http.expectJson GotCharacters characterListDecoder 
    }

castVote : Character -> Character -> Cmd Msg
castVote winChar loseChar =

    let --
        body = Http.jsonBody (voteEncoder winChar loseChar)
    in

    Http.post {
        url = Url.crossOrigin "https://chars.azurewebsites.net" ["api","Vote"] []
        , body = body
        , expect = Http.expectJson GotResult resultDecoder}



-- DECODERS

characterDecoder : Decoder Character
characterDecoder = 
     map2 Character
        (field "id" int)
        (field "name" string)

characterListDecoder : Decoder (List Character)
characterListDecoder =
    list characterDecoder


resultDecoder: Decoder MatchResult
resultDecoder =
    map2 MatchResult
        (field "winVotes" int)
        (field "loseVotes" int)


resultListDecoder : Decoder (List MatchResult)
resultListDecoder =
    list resultDecoder

voteEncoder : Character -> Character -> Encode.Value
voteEncoder winChar loseChar = 
    Encode.object 
        [ ("winner", Encode.string (urlName winChar))
        , ("loser", Encode.string (urlName loseChar))
        ]