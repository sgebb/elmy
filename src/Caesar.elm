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
      ciphertext : String
      , shift: Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" 0
  , Cmd.none
  )

alfSize : Int
alfSize =
    26

-- UPDATE


type Msg
  = None
  | ChangeCipher String
  | ChangeShift String

encodeChar : Char -> Int -> Char
encodeChar char shift =
        Char.fromCode (Char.toCode char  + shift)

decrypt : String -> Int -> String
decrypt cipher shift =
    String.concat (List.map (\x -> String.fromChar(encodeChar x shift))  (String.toList cipher))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None ->
        (model, Cmd.none)
    
    ChangeCipher nyCipher->
        ({model | ciphertext = nyCipher}, Cmd.none)

    ChangeShift nyShift->
        let 
            shiften = String.toInt(nyShift)
        in
            case shiften of
                Nothing ->
                    (model, Cmd.none)
                Just val ->
                    ({model | shift = val}, Cmd.none)
     

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [
        h1 [][text "Cipher"],
        input [placeholder "ciphertext", value model.ciphertext, onInput ChangeCipher] [],
        input [type_ "number", placeholder "shift", value (String.fromInt(model.shift)), onInput ChangeShift] [],
        textarea [placeholder "cleartext", value (decrypt model.ciphertext model.shift)][]
    ]