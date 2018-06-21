module View exposing (view)

import Html exposing (Html, div, h1, textarea, button, text)
import Html.Attributes exposing (rows, cols, id, type_, disabled)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model)
import Msg exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [ id "content" ]
        [ viewHeader
        , viewInterpreter model
        , viewButtons model
        ]


viewHeader : Html msg
viewHeader =
    h1 [] [ text "Brainfuck interpreter" ]


viewInterpreter : Model -> Html Msg
viewInterpreter model =
    div [ id "text-areas" ]
        [ textarea
            [ id "codeInput"
            , rows 25
            , cols 60
            , onInput SetCode
            ]
            [ text model.programCode ]
        , textarea
            [ id "codeOutput"
            , rows 25
            , cols 40
            ]
            [ text model.vm.output ]
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    div [ id "button-area" ]
        [ button
            [ type_ "submit"
            , onClick Evaluate
            ]
            [ text "Evaluate" ]
        ]
