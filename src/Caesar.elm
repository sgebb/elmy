import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



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
      cleartext : String
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

encodeChar : Char -> Int -> Char
encodeChar char shift =
    charValue (modBy alfSize (intValue char + shift))

decodeChar : Char -> Int -> Char
decodeChar char shift =
    encodeChar char ((-1)*shift)
        
intValue : Char -> Int
intValue char =
    Char.toCode char - Char.toCode 'a'
    
charValue : Int -> Char
charValue int =
    Char.fromCode (int + Char.toCode 'a') 

encrypt : String -> Int -> String
encrypt clear shift =
    String.concat (List.map (\x -> String.fromChar(encodeChar x shift))  (String.toList clear))
   
decrypt : String -> Int -> String
decrypt cipher shift = 
    String.concat (List.map (\x -> String.fromChar(decodeChar x shift))  (String.toList cipher))

type Msg
  = None
  | ChangeClear String
  | ChangeShift String
  | ChangeCipher String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None ->
        (model, Cmd.none)
    
    ChangeClear nyClear->
        ({model | cleartext = nyClear}, Cmd.none)

    ChangeShift nyShift->
        let 
            shiften = String.toInt(nyShift)
        in
            case shiften of
                Nothing ->
                    (model, Cmd.none)
                Just val ->
                    ({model | shift = val}, Cmd.none)
    ChangeCipher nyCipher ->
        ({model | cleartext = decrypt nyCipher model.shift}, Cmd.none)
     

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
        input [placeholder "ciphertext", value model.cleartext, onInput ChangeClear] [],
        input [type_ "number", placeholder "shift", value (String.fromInt(model.shift)), onInput ChangeShift] [],
        input [placeholder "cleartext", value (encrypt model.cleartext model.shift), onInput ChangeCipher][]
    ]
