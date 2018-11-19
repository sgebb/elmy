import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
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
  { url : String
  , character : String
  , availableChars : List String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png" "mario"
  ["mario", "luigi", "bowser", "bowser_jr", "pit", "dark_pit", "lucas", "ness"]
  , Cmd.none
  )

-- UPDATE


type Msg
  = GetImage
  | NewImage (Result Http.Error String)
  | Change String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetImage ->
      ( {model | url = toSmashUrl model.character}
      , Cmd.none
      )

    NewImage result ->
      case result of
        Ok newUrl ->
          ( { model | url = newUrl }
          , Cmd.none
          )

        Err _ ->
          ( model
          , Cmd.none
          )
    
    Change newChar ->
      ({model | character = newChar},
      Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [value model.character, onInput Change] []
    , button [ onClick GetImage ] [ text ("Get  " ++ model.character) ]
    , br [] []
    , img [ src model.url ] []
    , br [] []
    , renderList model.availableChars
    ]

renderList : List String -> Html Msg
renderList  list =
  ul [] (List.map toLi list)

toLi : String -> Html Msg
toLi s =
  li [onDoubleClick GetImage, onClick (Change s)] [text s]

-- HTTP


--"https://www.smashbros.com/assets_v2/img/fighter/thumb_v/mario.png"
toSmashUrl : String -> String
toSmashUrl char =
  Url.crossOrigin "https://www.smashbros.com" ["assets_v2","img","fighter","thumb_v",char ++ ".png"]
  []

