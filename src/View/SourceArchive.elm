module View.SourceArchive exposing (ArchiveEntry, entries, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


type alias ArchiveEntry =
    { id : String
    , title : String
    , box : String
    , shelf : String
    }


entries : List ArchiveEntry
entries =
    [ ArchiveEntry "001" "HELLO-WORLD" "BX-07-14" "S-3B"
    , ArchiveEntry "002" "QUINE" "BX-09-22" "S-1A"
    , ArchiveEntry "003" "ROT-13" "BX-12-03" "S-4C"
    , ArchiveEntry "004" "B-SORT" "BX-03-18" "S-2D"
    , ArchiveEntry "005" "COLLATZ" "BX-15-07" "S-5A"
    , ArchiveEntry "006" "LIFE" "BX-11-10" "S-2A"
    , ArchiveEntry "007" "GOLDEN" "BX-04-05" "S-3C"
    , ArchiveEntry "008" "FACTORIAL" "BX-18-11" "S-1C"
    , ArchiveEntry "009" "FIBONACCI" "BX-06-02" "S-4A"
    , ArchiveEntry "010" "I-SORT" "BX-13-16" "S-5D"
    , ArchiveEntry "011" "SIERPINSKI" "BX-20-04" "S-2B"
    , ArchiveEntry "012" "RNG-4" "BX-02-12" "S-3A"
    , ArchiveEntry "013" "NUMWARP" "BX-07-19" "S-3D"
    , ArchiveEntry "014" "SQUARES" "BX-10-08" "S-4B"
    , ArchiveEntry "015" "THUE-MORSE" "BX-05-21" "S-2C"
    , ArchiveEntry "016" "EXPONENT" "BX-16-11" "S-5B"
    ]


view : (ArchiveEntry -> msg) -> Html msg
view selectSourceFile =
    let
        viewRow archiveEntry =
            let
                dots =
                    60
                        - String.length archiveEntry.id
                        - String.length archiveEntry.title
                        - String.length archiveEntry.box
                        - String.length archiveEntry.shelf
            in
            Html.div
                [ Attr.class "flex flex-row justify-between cursor-pointer"
                , Events.onClick (selectSourceFile archiveEntry)
                ]
                [ Html.span [] [ Html.text <| archiveEntry.id ++ " " ++ archiveEntry.title ]
                , Html.span [] [ Html.text <| " " ++ String.repeat dots "." ++ " " ]
                , Html.span [] [ Html.text (archiveEntry.box ++ " " ++ archiveEntry.shelf) ]
                ]
    in
    Html.div
        [ Attr.id "archives-section"
        , Attr.class "mt-4 min-h-164"
        ]
        [ Html.div
            [ Attr.id "subheader"
            , Attr.class "text-left font-semibold"
            ]
            [ Html.span [ Attr.class "mt-2" ] [ Html.text "SOURCE ARCHIVES" ] ]
        , Html.hr [ Attr.class "mt-2" ] []
        , Html.div [ Attr.id "list-header", Attr.class "mt-2 flex flex-row justify-between" ]
            [ Html.span [ Attr.class "font-semibold" ] [ Html.text "TAPE TITLE" ]
            , Html.span [ Attr.class "font-semibold" ] [ Html.text "STORAGE LOCATION" ]
            ]
        , Html.hr [ Attr.class "mt-2" ] []
        , Html.div [ Attr.id "list-body", Attr.class "mt-2 flex flex-col gap-y-1" ]
            (entries |> List.map viewRow)
        , Html.div [ Attr.class "mt-8 ml-2 text-sm hyphens-auto" ]
            [ Html.span []
                [ Html.span [ Attr.class "font-semibold mr-2" ] [ Html.text "BX-YY-ZZ" ]
                , Html.text "denotes the box containing the tape."
                , Html.br [] []
                , Html.span [ Attr.class "font-semibold mr-2" ] [ Html.text "S-XX" ]
                , Html.text "denotes the shelf storing the box."
                ]
            ]
        ]
