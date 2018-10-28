import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing (Parser, (</>), (<?>), int, map, oneOf, s, string)
import Url.Parser.Query as Query
import Http exposing (..)



-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , route : Maybe Route
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url (routeFromUrl url), Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url, route = routeFromUrl url }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =

  { title = "URL Interceptor"
  , body =
      [ text "The current URL is: "
      , b [] [ text (Url.toString model.url) ]
      , ul []
          [ viewLink "/home"
          , viewLink "/profile"
          , viewLink "/reviews/the-century-of-the-self"
          , viewLink "/reviews/public-opinion"
          , viewLink "/reviews/shah-of-shahs"
          ]
      , viewMaybeRoute model.route
      ]
  }

viewMaybeRoute : Maybe Route -> Html Msg
viewMaybeRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            li [] [text "invalid"]
        Just rute ->
            viewRoute rute

viewRoute : Route -> Html Msg
viewRoute rute = 
    case rute of
        Reviews str ->
            h2 [][text str]
        Home ->
            viewHome
        Profile ->
            h2 [] [text "profile"]

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]


-- URL

type Route
  = Reviews String
  | Home
  | Profile


routeFromUrl : Url.Url -> (Maybe Route)
routeFromUrl url = 
    Url.Parser.parse route url


route : Parser (Route -> a) a
route =
  oneOf
    [ map Home (s "home" )
    , map Profile (s "profile" )
    , map Reviews ( s "reviews" </> string)
    ]

viewHome :  Html Msg
viewHome =
    div []
        [h2 [] [text "home"]]


-- HTTP https://pokeapi.co/api/v2/pokemon/13/

