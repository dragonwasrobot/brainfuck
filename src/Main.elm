module Main exposing (..)

import Html
import Model exposing (Model, initModel)
import Msg exposing (Msg(..))
import Update exposing (update)
import View exposing (view)


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
