module View exposing (view)

import Html exposing (Html, div, textarea, button, text)
import Html.Attributes exposing (rows, cols, id, type_, disabled)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model)
import Msg exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        vm =
            model.vm
    in
        div [ id "content" ]
            [ div [ id "text-areas" ]
                [ textarea [ id "codeInput", rows 25, cols 60, onInput SetCode ]
                    [ text model.programCode ]
                , textarea [ id "codeOutput", rows 25, cols 40 ]
                    [ text vm.output ]
                ]
            , div [ id "button-area" ]
                [ button [ type_ "submit", onClick Evaluate ]
                    [ text "Evaluate" ]
                ]
            ]
