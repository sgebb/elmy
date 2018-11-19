import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder as Url



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
  { character : String
  , url : String
  , charIndex : Int
  , newChar : String
  , charPostedString : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" "" 1 "" "Hasnt posted yet"
  , Cmd.none
  )

urlFromChar : String -> String
urlFromChar characterString = 
    "https://www.smashbros.com/assets_v2/img/fighter/thumb_v/" ++ characterString ++ ".png" 
--https://www.smashbros.com/assets_v2/img/fighter/thumb_v/mario.png

-- UPDATE


type Msg
  = GetCharacter
  | NewChar (Result Http.Error String)
  | PostChar
  | CharPosted (Result Http.Error String)
  | Change String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetCharacter ->
      ( {model| charIndex = model.charIndex + 1}
      , getCharacter model.charIndex
      )

    NewChar result ->
      case result of
        Ok newChar ->
          ( { model | 
            character = newChar
            , url = urlFromChar newChar
           }
          , Cmd.none
          )

        Err _ ->
          ( model
          , getCharacter model.charIndex
          )
    PostChar ->
        ({model| newChar = ""}
        , postCharacter model.newChar)
    
    CharPosted result ->
        case result of
            Ok res ->
                ({model | charPostedString = ("ok: " ++ res )}, Cmd.none)
            Err _ ->
                ({model | charPostedString = "err"}, Cmd.none)
    
    Change new->
        ({model | newChar = new}, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text model.character ]
    , button [ onClick GetCharacter ] [ text "Hent neste" ]
    , br [] []
    , img [ src model.url ] []
    , div [class "Newchar"] 
        [ input [value model.newChar, onInput Change][]
        , button [onClick PostChar][text "post"]
        , h2 [] [text model.charPostedString]
        ]
    ]



-- HTTP


getCharacter : Int -> Cmd Msg
getCharacter charIndex =
  Http.send NewChar (Http.get (toPicUrl charIndex) picDecoder)

postCharacter : String -> Cmd Msg
postCharacter char = 
    Http.send CharPosted (Http.post charactersUrl (Http.jsonBody(charEncoder char)) charDecoder)

--(


toPicUrl : Int -> String
toPicUrl index =
  Url.crossOrigin charactersUrl  [String.fromInt(index)]
    [ 
    ]


picDecoder : Decode.Decoder String
picDecoder =
  Decode.field "name" Decode.string

charactersUrl :  String
charactersUrl =
    Url.crossOrigin "http://smashcountdown.azurewebsites.net" ["characters"] []

-- Encode user to construct POST request body (for Register and Log In)
-- Character encoded as a POST request body from name-string

charEncoder : String -> Encode.Value
charEncoder char = 
    Encode.object 
        [ ("name", Encode.string char)
        ]

charDecoder : Decode.Decoder String
charDecoder =
    Decode.field "name" Decode.string
