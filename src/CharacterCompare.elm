import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Url.Builder as Url
import Styles exposing (..)



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
  { charOne: Character
  , charTwo: Character
  , availableChars : List String
  , lastPickedChar : Character
  }

type alias Character = 
    { url: String
    , name: String}

init : () -> (Model, Cmd Msg)
init _ =
    let
        char1 = Character "https://www.smashbros.com/assets_v2/img/fighter/thumb_v/mario.png" "Mario"
        char2 = Character "https://www.smashbros.com/assets_v2/img/fighter/thumb_v/bowser_jr.png" "BowserJr"
    in
    
  ( Model 
  char1 
  char2
  ["mario", "luigi", "bowser", "bowser_jr", "pit", "dark_pit", "lucas", "ness"]
  char1
  , Cmd.none
  )

-- UPDATE


type Msg
  = CharacterPicked Character


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CharacterPicked char->
        ({model | lastPickedChar = char}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []  
    [
    div [class "gridContainer", style "display" "flex"]
        [ 
        div [class "gridElement"]
        [
            img [src (model.charOne.url), alt "char1" , onClick (CharacterPicked model.charOne)][]
        ]
        , div [class "gridElement"]
        [
            
            img [src (model.charTwo.url), alt "char2", onClick (CharacterPicked model.charTwo)][]
        ]
        ]
    , div []
    [text model.lastPickedChar.name]
    ]



-- HTTP

--"https://www.smashbros.com/assets_v2/img/fighter/thumb_v/mario.png"
toSmashUrl : String -> String
toSmashUrl char =
  Url.crossOrigin "https://www.smashbros.com" ["assets_v2","img","fighter","thumb_v",char ++ ".png"]
  []
