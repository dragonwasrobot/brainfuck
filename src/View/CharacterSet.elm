module View.CharacterSet exposing (view)

import Array
import Brainfuck.ASCII as ASCII exposing (ASCII)
import Html exposing (Html)
import Html.Attributes as Attr



-- ** VIEW


view : Html msg
view =
    let
        viewRow : ASCII -> Html msg
        viewRow ascii =
            Html.tr [ Attr.classList [ ( "bg-gray-100", modBy 2 ascii.dec == 1 ) ] ]
                [ Html.td [ Attr.class "py-1" ]
                    [ Html.text <| String.fromInt ascii.dec ]
                , Html.td [ Attr.class "py-1" ]
                    [ Html.text <| "0x" ++ ascii.hex ]
                , Html.td [ Attr.class "py-1 text-red-700" ]
                    [ Html.text <| ascii.char ]
                , Html.td [ Attr.class "py-1 text-left" ]
                    [ Html.text <| Maybe.withDefault "" ascii.description ]
                ]
    in
    Html.div
        [ Attr.id "character-set-section"
        , Attr.class "mt-4 min-h-164"
        ]
        [ Html.div
            [ Attr.id "subheader"
            , Attr.class "text-left font-semibold"
            ]
            [ Html.span [ Attr.class "mt-2" ] [ Html.text "CHARACTER SETS" ]
            ]
        , Html.hr [ Attr.class "mt-2" ] []
        , Html.table [ Attr.id "ascii-table", Attr.class "w-full table-auto" ]
            [ Html.thead [ Attr.class "font-semibold text-center" ]
                [ Html.tr []
                    [ Html.th [ Attr.class "py-2 border-t-0 border-l-0 border-b" ]
                        [ Html.text "DEC" ]
                    , Html.th [ Attr.class "py-2 border-t-0 border-l-0 border-b" ]
                        [ Html.text "HEX" ]
                    , Html.th [ Attr.class "py-2 border-t-0 border-l-0 border-b" ]
                        [ Html.text "CHAR" ]
                    , Html.th [ Attr.class "py-2 border-t-0 border-l-0 border-b text-left" ]
                        [ Html.text "NAME" ]
                    ]
                ]
            , Html.tbody [ Attr.class "text-center" ]
                (ASCII.table
                    |> Array.toList
                    |> List.map viewRow
                )
            ]
        ]
