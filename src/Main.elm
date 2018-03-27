module Main exposing (..)

import Html exposing (Html, div)


type alias Model =
    {}


initModel : Model
initModel =
    {}


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] []


init : ( Model, Cmd Msg )
init =
    let
        model =
            initModel
    in
        ( model, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> subscriptions)
        }
