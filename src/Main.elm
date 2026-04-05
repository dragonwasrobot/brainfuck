module Main exposing (main)

import Array
import Brainfuck.ASCII as ASCII exposing (ASCII, Byte)
import Brainfuck.Evaluator as Evaluator exposing (EvaluationContext, State(..))
import Brainfuck.Parser as Parser
import Brainfuck.VirtualMachine as VirtualMachine exposing (VirtualMachine)
import Browser
import Char
import Hex
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http exposing (Response(..))
import List
import List.Extra as List
import Maybe.Extra as Maybe
import Process
import Result
import Result.Extra as Result
import String
import String.Extra as String
import Task
import View.CharacterSet as CharacterSet
import View.Interpreter as Interpreter
import View.ReferenceManual as ReferenceManual
import View.SourceArchive as SourceArchive exposing (ArchiveEntry)



-- ** Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { basePath : String }



-- ** Model


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initModel =
            { basePath = flags.basePath
            , interpreter = Interpreter.init
            , page = InterpreterPage
            }
    in
    case List.head SourceArchive.entries of
        Nothing ->
            ( initModel, Cmd.none )

        Just archiveEntry ->
            selectSourceFile archiveEntry initModel


type alias Model =
    { basePath : String
    , interpreter : Interpreter.Model
    , page : Page
    }


type Page
    = ReferenceManualPage
    | SourceArchivesPage
    | CharacterSetsPage
    | InterpreterPage



-- ** Update


type Msg
    = InterpreterMsg Interpreter.Msg
    | ChangePage Page
    | SelectSourceFile ArchiveEntry
    | SourceFileDownloaded (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InterpreterMsg interpreterMsg ->
            let
                ( newInterpreter, interpreterCmd ) =
                    Interpreter.update interpreterMsg model.interpreter

                newModel =
                    { model | interpreter = newInterpreter }
            in
            ( newModel, Cmd.map InterpreterMsg interpreterCmd )

        ChangePage page ->
            setPage page model

        SelectSourceFile archiveEntry ->
            selectSourceFile archiveEntry model

        SourceFileDownloaded result ->
            loadSourceFile result model


setPage : Page -> Model -> ( Model, Cmd Msg )
setPage page model =
    ( { model | page = page }, Cmd.none )


selectSourceFile : ArchiveEntry -> Model -> ( Model, Cmd Msg )
selectSourceFile archiveEntry model =
    let
        prefixPath =
            model.basePath ++ "/bf-programs/"

        filename =
            archiveEntry.id ++ "-" ++ String.toLower archiveEntry.title ++ ".bf"

        fetchProgram =
            Http.get
                { url = prefixPath ++ filename
                , expect = Http.expectString SourceFileDownloaded
                }
    in
    ( model, fetchProgram )


loadSourceFile : Result Http.Error String -> Model -> ( Model, Cmd Msg )
loadSourceFile result model =
    case result of
        Err error ->
            -- TODO: Show error
            ( model, Cmd.none )

        Ok code ->
            let
                oldInterpreter =
                    model.interpreter

                newInterpreter =
                    { oldInterpreter | inputCode = code }
            in
            ( { model | interpreter = newInterpreter, page = InterpreterPage }, Cmd.none )



-- ** View


view : Model -> Html Msg
view model =
    Html.div [ Attr.id "document-container", Attr.class "flex flex-row my-8" ]
        [ Html.div [ Attr.id "column-1", Attr.class "w-1/6" ] []
        , Html.div [ Attr.id "column-2", Attr.class "w-4/6" ] [ viewBody model ]
        , Html.div [ Attr.id "column-3", Attr.class "w-1/6" ] [ viewNavigation model ]
        ]


viewNavigation : Model -> Html Msg
viewNavigation model =
    let
        buttonStyling =
            "bg-white shadow-lg p-2 border border-slate-200 text-sm text-left"

        enabledStyling =
            "text-black cursor-pointer"

        disabledStyling =
            "text-slate-300 cursor-default"
    in
    Html.div
        [ Attr.id "navigation-container"
        , Attr.class "flex mx-auto font-mono tracking-wider"
        ]
        [ Html.div
            [ Attr.id "navigation"
            , Attr.class "flex flex-col gap-y-10"
            ]
            [ Html.button
                [ Attr.id "show-reference-manual"
                , Attr.type_ "submit"
                , Attr.class buttonStyling
                , Attr.classList
                    [ ( disabledStyling, model.page == ReferenceManualPage )
                    , ( enabledStyling, model.page /= ReferenceManualPage )
                    ]
                , Events.onClick (ChangePage ReferenceManualPage)
                ]
                [ Html.i [ Attr.class "mr-2 fa-solid fa-file-lines" ] []
                , Html.text "REFERENCE MANUAL"
                ]
            , Html.button
                [ Attr.id "interpreter"
                , Attr.type_ "submit"
                , Attr.class buttonStyling
                , Attr.classList
                    [ ( disabledStyling, model.page == InterpreterPage )
                    , ( enabledStyling, model.page /= InterpreterPage )
                    ]
                , Events.onClick (ChangePage InterpreterPage)
                ]
                [ Html.i [ Attr.class "fa-solid fa-play mr-2" ] []
                , Html.text "BF-4000 MACHINE"
                ]
            , Html.button
                [ Attr.id "show-source-archives"
                , Attr.type_ "submit"
                , Attr.class buttonStyling
                , Attr.classList
                    [ ( disabledStyling, model.page == SourceArchivesPage )
                    , ( enabledStyling, model.page /= SourceArchivesPage )
                    ]
                , Events.onClick (ChangePage SourceArchivesPage)
                ]
                [ Html.i [ Attr.class "mr-2 fa-solid fa-folder-open" ] []
                , Html.text "SOURCE ARCHIVES"
                ]
            , Html.button
                [ Attr.id "show-character-sets"
                , Attr.type_ "submit"
                , Attr.class buttonStyling
                , Attr.classList
                    [ ( disabledStyling, model.page == CharacterSetsPage )
                    , ( enabledStyling, model.page /= CharacterSetsPage )
                    ]
                , Events.onClick (ChangePage CharacterSetsPage)
                ]
                [ Html.i [ Attr.class "mr-2 fa-solid fa-paragraph" ] []
                , Html.text "CHARACTER SETS"
                ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    let
        viewPageHeader : String -> Html msg
        viewPageHeader sectionNumber =
            Html.div
                [ Attr.id "header-section"
                , Attr.class "w-full flex flex-row justify-between text-sm font-medium"
                ]
                [ Html.div [ Attr.class "text-left" ]
                    [ Html.text "DRAGON & ROBOT"
                    , Html.br [] []
                    , Html.text "BUSINESS MACHINES INC."
                    ]
                , Html.div [ Attr.class "text-right" ]
                    [ Html.text <| "SECTION " ++ sectionNumber
                    , Html.br [] []
                    , Html.text "SEP 1968"
                    ]
                ]

        viewPageFooter : Int -> Html msg
        viewPageFooter pageNumber =
            Html.div
                [ Attr.id "footer"
                , Attr.class "mt-4 mb-4 flex flex-row justify-between text-sm"
                ]
                [ Html.span [] [ Html.text "DENMARK" ]
                , Html.span [ Attr.class "font-semibold" ] [ Html.text "Tabulating the Future™" ]
                , Html.span [] [ Html.text <| "Page " ++ String.fromInt pageNumber ]
                ]

        viewPageWrapper : String -> Int -> Html msg -> Html msg
        viewPageWrapper sectionNumber pageNumber subView =
            Html.div
                [ Attr.id "body"
                , Attr.class "flex flex-col w-10/11 bg-white mx-5 mt-4 tracking-wider"
                ]
                [ viewPageHeader sectionNumber
                , subView
                , Html.hr [ Attr.class "mt-2" ] []
                , viewPageFooter pageNumber
                ]

        viewSelectedBody =
            case model.page of
                ReferenceManualPage ->
                    viewPageWrapper "2.1" 42 ReferenceManual.view

                SourceArchivesPage ->
                    viewPageWrapper "7.4" 211 (SourceArchive.view SelectSourceFile)

                CharacterSetsPage ->
                    viewPageWrapper "A.2" 374 CharacterSet.view

                InterpreterPage ->
                    viewPageWrapper "4.2" 127 (Html.map InterpreterMsg <| Interpreter.view model.interpreter)
    in
    Html.div
        [ Attr.id "paper-container"
        , Attr.class "flex min-h-192 w-3xl mx-auto bg-white shadow-lg border border-slate-200 font-mono"
        ]
        [ viewAside
        , viewSelectedBody
        ]


viewAside : Html msg
viewAside =
    let
        paperTapeHoles =
            "grow text-sm bg-[url(./circle.svg)] mt-4 bg-top bg-repeat-y bg-size-[1.25em]"

        paperTapeBorder =
            "w-[0.1em] border-dashed-[1.75em] border-l-0 border-t-0 border-b-0"
    in
    Html.aside
        [ Attr.id "paper-strip"
        , Attr.class "w-1/11 flex"
        ]
        [ Html.div [ Attr.id "holes", Attr.class paperTapeHoles ] []
        , Html.div [ Attr.id "ripped-border", Attr.class paperTapeBorder ] []
        ]



-- ** Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
