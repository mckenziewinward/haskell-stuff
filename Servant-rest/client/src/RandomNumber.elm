module RandomNumber exposing (Model)

import Random
import Browser
import Html exposing (..)
import Html.Events exposing (onClick)

type alias Model = Int

init : (Model, Cmd Msg)
init = ( 0, Cmd.none )

initialModel : Model
initialModel = 0

view : Model -> Html Msg
view model = 
    div []
        [ button [ onClick GenerateRandomNumber ] [ text "Generate Random Number"]
        , text (String.fromInt model)
        ]

type Msg
    = GenerateRandomNumber
    | NewRandomNumber Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomNumber ->
            ( model, Random.generate NewRandomNumber (Random.int 0 100) )
        NewRandomNumber number ->
            ( number, Cmd.none )

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }