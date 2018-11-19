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
  { 
      hendelsesListe : List Hendelse
  }

type alias Hendelse =
    {
        navn : String
        , fnr : String
        , feilmelding : String
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model [
      {navn = "En Person", fnr = "09068941222", feilmelding  = "Ingen feil"}
  ]
  , Cmd.none
  )

-- UPDATE


type Msg
  = None


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None ->
        (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [
        viewHendelsesliste model.hendelsesListe
    ]

viewHendelsesliste : List Hendelse -> Html Msg
viewHendelsesliste hendelsesliste =
    table [] (List.map toTr hendelsesliste )

toTr : Hendelse -> Html Msg
toTr hendelse = 
    tr [] (toTdList hendelse)

toTdList : Hendelse -> List (Html Msg)
toTdList hendelse= 
    [
        td [] [text hendelse.navn]
        , td [][text hendelse.fnr]
        , td [] [text hendelse.feilmelding]
    ]
-- HTTP
