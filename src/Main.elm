module Main exposing (main)

import Brainfuck.Evaluator as Evaluator exposing (EvaluationContext, State(..))
import Brainfuck.Parser as Parser
import Brainfuck.VirtualMachine as VirtualMachine exposing (VirtualMachine)
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http exposing (Response(..))
import List
import List.Extra as List
import Process
import String
import Task exposing (Task)



-- ** Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- ** Model


init : () -> ( Model, Cmd Msg )
init flags =
    let
        initModel =
            { inputCode = ""
            , inputData = ""
            , form = Initial
            , page = InterpreterPage
            }
    in
    case List.head sourceArchiveData of
        Nothing ->
            ( initModel, Cmd.none )

        Just archiveEntry ->
            selectSourceFile archiveEntry initModel


type alias Code =
    String


type alias Model =
    { inputCode : Code
    , inputData : String
    , form : FormState
    , page : Page
    }


type FormState
    = Initial
    | Failed String
    | Loaded EvaluationContext


type Page
    = ReferenceManualPage
    | InterpreterPage
    | SourceArchivesPage



-- ** Msg


type Msg
    = StartEvaluation
    | StopEvaluation
    | ResumeEvaluation
    | EvaluateStep
    | ClearOutput
    | SetCode Code
    | SetInput String
    | ChangePage Page
    | SelectSourceFile ArchiveEntry
    | SourceFileDownloaded (Result Http.Error String)



-- ** Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartEvaluation ->
            startEvaluation model

        StopEvaluation ->
            stopEvaluation model

        ResumeEvaluation ->
            resumeEvaluation model

        EvaluateStep ->
            evaluateStep model

        ClearOutput ->
            clearOutput model

        SetCode code ->
            setCode code model

        SetInput input ->
            setInput input model

        ChangePage page ->
            setPage page model

        SelectSourceFile archiveEntry ->
            selectSourceFile archiveEntry model

        SourceFileDownloaded result ->
            loadSourceFile result model


startEvaluation : Model -> ( Model, Cmd Msg )
startEvaluation model =
    let
        parsedProgram =
            model.inputCode
                |> Parser.parse
    in
    case parsedProgram of
        Err _ ->
            -- TODO: Figure out how to convert err to something useful
            let
                error =
                    "Failed to parse program due to <deal with DeadEnd output>"
            in
            ( { model | form = Failed error }, Cmd.none )

        Ok program ->
            let
                newForm =
                    model.inputData
                        |> resetVirtualMachine
                        |> Evaluator.initContext program
                        |> Loaded
            in
            ( { model | form = newForm }, stepCmd )


stopEvaluation : Model -> ( Model, Cmd Msg )
stopEvaluation model =
    let
        pauseEval context =
            if context.state == Running then
                { context | state = Paused }

            else
                context
    in
    case model.form of
        Initial ->
            ( model, Cmd.none )

        Failed _ ->
            ( model, Cmd.none )

        Loaded context ->
            ( { model | form = Loaded <| pauseEval context }, Cmd.none )


resumeEvaluation : Model -> ( Model, Cmd Msg )
resumeEvaluation model =
    let
        resumeEval context =
            if context.state == Paused then
                ( { context | state = Running }, stepCmd )

            else
                ( context, Cmd.none )
    in
    case model.form of
        Initial ->
            ( model, Cmd.none )

        Failed _ ->
            ( model, Cmd.none )

        Loaded context ->
            let
                ( newContext, cmd ) =
                    resumeEval context
            in
            ( { model | form = Loaded newContext }, cmd )


evaluateStep : Model -> ( Model, Cmd Msg )
evaluateStep model =
    case model.form of
        Initial ->
            ( model, Cmd.none )

        Failed _ ->
            ( model, Cmd.none )

        Loaded context ->
            let
                newContext =
                    Evaluator.evaluateStep context

                newCmd =
                    if newContext.state == Running then
                        stepCmd

                    else
                        Cmd.none
            in
            ( { model | form = Loaded newContext }, newCmd )


stepCmd : Cmd Msg
stepCmd =
    -- NOTE: Faster but blocking (can lead to freeze): Task.perform identity (Task.succeed EvaluateStep)
    Task.perform (\_ -> EvaluateStep) (Process.sleep 1)


clearOutput : Model -> ( Model, Cmd Msg )
clearOutput model =
    case model.form of
        Initial ->
            -- Nothing to clear
            ( model, Cmd.none )

        Failed _ ->
            -- Nothing to clear
            ( model, Cmd.none )

        Loaded context ->
            let
                newContext =
                    { context | vm = resetVirtualMachine model.inputData }
            in
            ( { model | form = Loaded newContext }, Cmd.none )


resetVirtualMachine : String -> VirtualMachine
resetVirtualMachine data =
    let
        eofByte =
            0

        inputs =
            data |> String.toList |> List.map Char.toCode
    in
    VirtualMachine.init <| inputs ++ [ eofByte ]


setCode : Code -> Model -> ( Model, Cmd Msg )
setCode code model =
    ( { model | inputCode = code }, Cmd.none )


setInput : String -> Model -> ( Model, Cmd Msg )
setInput input model =
    ( { model | inputData = input }, Cmd.none )


setPage : Page -> Model -> ( Model, Cmd Msg )
setPage page model =
    ( { model | page = page }, Cmd.none )


selectSourceFile : ArchiveEntry -> Model -> ( Model, Cmd Msg )
selectSourceFile archiveEntry model =
    let
        -- prod: /brainfuck/bf-programs/
        -- dev: /bf-programs/
        prefixPath =
            "/bf-programs/"

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
            ( { model | inputCode = code, page = InterpreterPage }, Cmd.none )



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
                [ Html.i [ Attr.class "fa-solid fa-tape mr-2" ] []
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
                [ Html.i [ Attr.class "mr-2 fa-solid fa-box-archive" ] []
                , Html.text "SOURCE ARCHIVES"
                ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    let
        viewSelectedBody =
            case model.page of
                InterpreterPage ->
                    viewInterpreter model

                ReferenceManualPage ->
                    viewReferenceManual

                SourceArchivesPage ->
                    viewSourceArchives model
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
            "text-sm bg-[url(./circle.svg)] bg-center bg-repeat-y bg-size-[1.25em]"

        paperTapeBorder =
            "w-1/11 border-[0.125em] border-dashed-60 border-l-0 border-t-0 border-b-0 border-black"
    in
    Html.aside
        [ Attr.id "paper-strip"
        , Attr.class <| paperTapeBorder ++ " " ++ paperTapeHoles
        ]
        []


viewReferenceManual : Html Msg
viewReferenceManual =
    let
        viewIntroduction =
            Html.div [ Attr.id "introduction" ]
                [ Html.div
                    [ Attr.id "subheader", Attr.class "text-left font-semibold" ]
                    [ Html.span [ Attr.class "mt-2" ] [ Html.text "BF-4000 MACHINE LANGUAGE" ] ]
                , Html.hr [ Attr.class "mt-1" ] []
                , Html.hr [ Attr.class "mt-0.5" ] []
                , Html.div [ Attr.class "mt-4 text-sm hyphens-auto" ]
                    [ Html.span []
                        [ Html.text "The BF-4000 instruction set provides a compact general-purpose machine language intended for efficient execution on small to medium mainframes including time-shared installations. By employing a rigorously orthogonal set of eight primitive instructions, BF-4000 attains a degree of simplicity that promotes provable behavior and economical interpreter design. The instruction repertoire maps directly to a linear working store (\"tape\") addressed by a movable indicator (\"pointer\"), enabling deterministic execution without recourse to elaborate control units." ]
                    , Html.br [] []
                    , Html.br [] []
                    , Html.span [] [ Html.text "Prominent features include:" ]
                    , Html.ul [ Attr.class "mt-2 ml-4 list-disc" ]
                        [ Html.li [ Attr.class "mt-2" ]
                            [ Html.span [ Attr.class "font-semibold" ] [ Html.text "Minimal yet complete operator set." ]
                            , Html.span [ Attr.class "ml-2" ] [ Html.text "Eight single-character instructions provide arithmetic on the working cell, bidirectional pointer motion, byte-stream input/output compatible with TeleType® devices, and structured iteration via matched delimiters." ]
                            ]
                        , Html.li [ Attr.class "mt-2" ]
                            [ Html.span [ Attr.class "font-semibold" ] [ Html.text "High transportability." ]
                            , Html.span [ Attr.class "ml-2" ] [ Html.text "Cell width, tape extent, and end-of-file conventions are all configurable at the time of program execution, permitting straightforward accommodation to diverse cores, drums, or disc subsystems while retaining program form." ]
                            ]
                        , Html.li [ Attr.class "mt-2" ]
                            [ Html.span [ Attr.class "font-semibold" ] [ Html.text "Deterministic control transfer." ]
                            , Html.span [ Attr.class "ml-2" ] [ Html.text "Loop entry/exit semantics are fixed at translation time by bracket pairing, obviating run-time ambiguity and simplifying diagnostic procedures." ]
                            ]
                        , Html.li [ Attr.class "mt-2" ]
                            [ Html.span [ Attr.class "font-semibold" ] [ Html.text "Economy of resources." ]
                            , Html.span [ Attr.class "ml-2" ] [ Html.text "No symbol tables, stacks, or hidden temporaries are required beyond the working store and the input/output streams, rendering BF-4000 suitable for dynamic programming, numerical computation, and environments of constrained memory." ]
                            ]
                        ]
                    , Html.br [] []
                    , Html.span [] [ Html.text "The following concise specification enumerates the syntax and semantics of the BF-4000 machine language." ]
                    ]
                , Html.hr [ Attr.class "my-4" ] []
                ]

        viewSourceFormatDescription =
            Html.div [ Attr.class "mt-4" ]
                [ Html.h3 [ Attr.class "mt-4" ] [ Html.text "1. Source Format" ]
                , Html.div [ Attr.class "mt-2 text-sm hyphens-auto" ] [ Html.span [] [ Html.text "The following holds for any BF-4000 source program:" ] ]
                , Html.ul [ Attr.class "mt-2 ml-4 list-disc text-sm hyphens-auto" ]
                    [ Html.li [ Attr.class "mt-1" ] [ Html.text "A program is a finite sequence of characters." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "The significant characters (tokens) are: > < + - . , [ ]." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "All other characters are ignored (treated as comments)." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "An interpreter must diagnose unmatched [ or ] as a translation error." ]
                    ]
                ]

        viewGrammarDescription =
            Html.div [ Attr.class "mt-4" ]
                [ Html.h3 [] [ Html.text "2. Grammar (BNF)" ]
                , Html.div [ Attr.class "mt-2 text-sm hyphens-auto" ] [ Html.span [] [ Html.text "The syntax of the BF-4000 machine language is defined according to the following Backus-Naur Form (BNF) grammar:" ] ]
                , Html.pre [ Attr.class "mx-8 my-4 py-4 border border-l-0 border-r-0 text-sm leading-6" ] [ Html.text "  <program> ::= <instr>*\n  <instr> ::= \">\" | \"<\" | \"+\" | \"-\" | \".\" | \",\" | <loop>\n  <loop> ::= \"[\" <instr>* \"]\"" ]
                , Html.div [ Attr.class "text-sm hyphens-auto" ] [ Html.span [] [ Html.text "Bracket pairing is determined by static nesting at translation time." ] ]
                ]

        viewInstructionSemanticsDescription =
            Html.div [ Attr.class "mt-4" ]
                [ Html.h3 [] [ Html.text "3. Instruction Semantics" ]
                , Html.div [ Attr.class "mt-2 text-sm hyphens-auto" ]
                    [ Html.span [] [ Html.text "Table 2.3 below lists the symbols and semantics of the eight instructions of the BF-4000 machine language." ] ]
                , Html.div [ Attr.class "mx-8 text-sm hyphens-auto" ]
                    [ Html.table [ Attr.class "mx-auto mt-4 w-full table-auto" ]
                        [ Html.thead []
                            [ Html.tr []
                                [ Html.th [ Attr.class "p-1 border border-l-0 border-r-0" ] [ Html.span [ Attr.class "font-semibold" ] [ Html.text "INSTRUCTION" ] ]
                                , Html.th [ Attr.class "p-1 border border-l-0 border-r-0" ] [ Html.span [ Attr.class "font-semibold" ] [ Html.text "SYMBOL" ] ]
                                , Html.th [ Attr.class "p-1 border border-l-0 border-r-0" ] [ Html.span [ Attr.class "font-semibold" ] [ Html.text "SEMANTICS" ] ]
                                ]
                            ]
                        , Html.tbody
                            []
                            [ Html.tr []
                                [ Html.td [ Attr.class "p-1 border border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "Increment pointer" ] ]
                                , Html.td [ Attr.class "p-1 border border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text ">" ] ]
                                , Html.td [ Attr.class "p-1 border border-b-0 border-l-0 border-r-0" ] [ Html.span [] [ Html.text "DP ← DP + 1." ] ]
                                ]
                            , Html.tr []
                                [ Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "Decrement pointer" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "<" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0" ] [ Html.span [] [ Html.text "DP ← DP - 1." ] ]
                                ]
                            , Html.tr []
                                [ Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "Increment cell" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "+" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0" ] [ Html.span [] [ Html.text "cell[DP] ← cell[DP] + 1." ] ]
                                ]
                            , Html.tr []
                                [ Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "Decrement cell" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "-" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0" ] [ Html.span [] [ Html.text "cell[DP] ← cell[DP] − 1." ] ]
                                ]
                            , Html.tr []
                                [ Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "Print value" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "." ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0" ] [ Html.span [] [ Html.text "emit cell[DP] to output as one byte." ] ]
                                ]
                            , Html.tr []
                                [ Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "Read value" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "," ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0" ] [ Html.span [] [ Html.text "read one byte from input into cell[DP]." ] ]
                                ]
                            , Html.tr []
                                [ Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "Loop start" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "[" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-b-0 border-l-0 border-r-0" ]
                                    [ Html.span []
                                        [ Html.text "if cell[DP] = 0, jump to command"
                                        , Html.br [] []
                                        , Html.text "after matching ], else proceed."
                                        ]
                                    ]
                                ]
                            , Html.tr []
                                [ Html.td [ Attr.class "p-1 border border-t-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "Loop end" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-l-0 border-r-0 text-center" ] [ Html.span [] [ Html.text "]" ] ]
                                , Html.td [ Attr.class "p-1 border border-t-0 border-l-0 border-r-0" ]
                                    [ Html.span []
                                        [ Html.text "if cell[DP] ≠ 0, jump to command "
                                        , Html.br [] []
                                        , Html.text "after the matching [, else proceed."
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , Html.div [ Attr.class "mx-4 mt-1 text-center text-sm" ]
                    [ Html.span [ Attr.class "font-semibold" ] [ Html.text "Table 2.3" ] ]
                ]

        viewAbstractMachineDescription =
            Html.div [ Attr.class "mt-4" ]
                [ Html.h3 [] [ Html.text "4. Abstract Machine" ]
                , Html.div [ Attr.class "mt-2 text-sm hyphens-auto" ] [ Html.span [] [ Html.text "The BF-4000 machine architecture is defined as:" ] ]
                , Html.ul [ Attr.class "mt-2 ml-4 list-disc text-sm hyphens-auto" ]
                    [ Html.li [ Attr.class "mt-1" ] [ Html.text "A tape representing a one-dimensional array of cells." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "A data pointer (DP) designates one cell on the tape." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "Two byte streams: one for input and one for output." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "The tape length is 30,000 cells." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "Every cell on the tape has a width of 1 byte." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "If a data pointer gets out of tape range, an error code is printed." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "If a cell value exceeds its legal range, an error code is printed." ]
                    , Html.li [ Attr.class "mt-1" ] [ Html.text "Encountering EOF during input read does not change cell value." ]
                    ]
                ]

        viewLanguageSpec =
            Html.div [ Attr.id "language-spec" ]
                [ Html.h2 [ Attr.class "font-semibold" ] [ Html.text "LANGUAGE SPECIFICATION" ]
                , viewSourceFormatDescription
                , viewGrammarDescription
                , viewInstructionSemanticsDescription
                , viewAbstractMachineDescription
                , Html.div [ Attr.class "mt-6 mb-2 text-sm hyphens-auto" ]
                    [ Html.span []
                        [ Html.text "Consult Section 7.4 Source Archives for a selection of common programs written in the BF-4000 language." ]
                    ]
                ]
    in
    Html.div
        [ Attr.id "body"
        , Attr.class "flex flex-col w-10/11 bg-white mx-5 mt-4 tracking-wider"
        ]
        [ Html.div
            [ Attr.id "header-section"
            , Attr.class "w-full flex flex-row justify-between text-sm font-medium"
            ]
            [ Html.div [ Attr.class "text-left" ]
                [ Html.text "DRAGON & ROBOT"
                , Html.br [] []
                , Html.text "BUSINESS MACHINES INC."
                ]
            , Html.div [ Attr.class "text-right" ]
                [ Html.text "SECTION 2.1"
                , Html.br [] []
                , Html.text "SEP 1968"
                ]
            ]
        , Html.div
            [ Attr.id "archives-section"
            , Attr.class "mt-4 min-h-164"
            ]
            [ viewIntroduction
            , viewLanguageSpec
            ]
        , Html.hr [ Attr.class "mt-2" ] []
        , viewFooter 42
        ]


type alias ArchiveEntry =
    { id : String
    , title : String
    , box : String
    , shelf : String
    }


sourceArchiveData : List ArchiveEntry
sourceArchiveData =
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


viewSourceArchives : Model -> Html Msg
viewSourceArchives model =
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
                , Events.onClick (SelectSourceFile archiveEntry)
                ]
                [ Html.span [] [ Html.text <| archiveEntry.id ++ " " ++ archiveEntry.title ]
                , Html.span [] [ Html.text <| " " ++ String.repeat dots "." ++ " " ]
                , Html.span [] [ Html.text (archiveEntry.box ++ " " ++ archiveEntry.shelf) ]
                ]
    in
    Html.div
        [ Attr.id "body"
        , Attr.class "flex flex-col w-10/11 bg-white mx-5 mt-4 tracking-wider"
        ]
        [ Html.div
            [ Attr.id "header-section"
            , Attr.class "w-full flex flex-row justify-between text-sm font-medium"
            ]
            [ Html.div [ Attr.class "text-left" ]
                [ Html.text "DRAGON & ROBOT"
                , Html.br [] []
                , Html.text "BUSINESS MACHINES INC."
                ]
            , Html.div [ Attr.class "text-right" ]
                [ Html.text "SECTION 7.4"
                , Html.br [] []
                , Html.text "SEP 1968"
                ]
            ]
        , Html.div
            [ Attr.id "archives-section"
            , Attr.class "mt-4 min-h-164"
            ]
            [ Html.div
                [ Attr.id "subheader"
                , Attr.class "text-left font-semibold"
                ]
                [ Html.span [ Attr.class "mt-2" ] [ Html.text "SOURCE ARCHIVES" ]
                ]
            , Html.hr [ Attr.class "mt-2" ] []
            , Html.div [ Attr.id "list-header", Attr.class "mt-2 flex flex-row justify-between" ]
                [ Html.span [ Attr.class "font-semibold" ] [ Html.text "TAPE TITLE" ]
                , Html.span [ Attr.class "font-semibold" ] [ Html.text "STORAGE LOCATION" ]
                ]
            , Html.hr [ Attr.class "mt-2" ] []
            , Html.div [ Attr.id "list-body", Attr.class "mt-2 flex flex-col gap-y-1" ]
                (sourceArchiveData |> List.map viewRow)
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
        , Html.hr [ Attr.class "mt-2" ] []
        , viewFooter 211
        ]


viewInterpreter : Model -> Html Msg
viewInterpreter model =
    Html.div
        [ Attr.id "body"
        , Attr.class "flex flex-col w-10/11 bg-white mx-5 mt-4 tracking-wider"
        ]
        [ viewInterpreterHeader
        , viewInterpreterTitle
        , viewInterpreterForm model
        ]


viewInterpreterHeader : Html msg
viewInterpreterHeader =
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
            [ Html.text "SECTION 4.2"
            , Html.br [] []
            , Html.text "SEP 1968"
            ]
        ]


viewInterpreterTitle : Html msg
viewInterpreterTitle =
    Html.div
        [ Attr.id "title-section"
        , Attr.class "mx-auto"
        ]
        [ Html.div
            [ Attr.id "title-box"
            , Attr.class "mt-4 px-6 py-2 text-sm text-center border border-[0.125em]"
            ]
            [ Html.text "DATA SHEET"
            , Html.br [] []
            , Html.text "BF-4000 MACHINE"
            ]
        ]


viewInterpreterForm : Model -> Html Msg
viewInterpreterForm model =
    let
        viewSubHeader =
            Html.div [ Attr.id "subheader" ]
                [ Html.span [] [ Html.text "PROGRAM EXECUTION" ] ]

        viewSource =
            Html.div
                [ Attr.id "request"
                , Attr.class "mt-2 flex flex-row"
                ]
                [ Html.span [ Attr.class "w-1/6" ] [ Html.text "SOURCE" ]
                , Html.textarea
                    [ Attr.id "code-source"
                    , Attr.class "w-5/6 border-none outline-none resize-none"
                    , Attr.rows 25
                    , Attr.placeholder "ENTER SOURCE"
                    , Events.onInput SetCode
                    ]
                    [ Html.text model.inputCode ]
                ]

        viewInput =
            Html.div
                [ Attr.id "input-field"
                , Attr.class "mt-2 flex flex-row"
                ]
                [ Html.span [ Attr.class "w-1/6" ] [ Html.text "INPUT" ]
                , Html.textarea
                    [ Attr.id "code-input"
                    , Attr.class "w-5/6 border-none outline-none resize-none"
                    , Attr.rows 5
                    , Attr.cols 60
                    , Attr.placeholder "ENTER DATA"
                    , Events.onInput SetInput
                    ]
                    [ Html.text model.inputData ]
                ]

        viewRunButton =
            let
                ( msg, icon, label ) =
                    case model.form of
                        Initial ->
                            ( StartEvaluation, "fa-play", "RUN PROGRAM" )

                        Failed _ ->
                            -- TODO: Present error in UI
                            ( StartEvaluation, "fa-play", "RUN PROGRAM" )

                        Loaded context ->
                            case context.state of
                                Running ->
                                    ( StopEvaluation, "fa-pause", "PAUSE PROGRAM" )

                                Paused ->
                                    ( ResumeEvaluation, "fa-play", "RESUME PROGRAM" )

                                Crashed _ ->
                                    ( StartEvaluation, "fa-play", "RUN PROGRAM" )

                                Finished ->
                                    ( StartEvaluation, "fa-play", "RUN PROGRAM" )
            in
            Html.button
                [ Attr.id "run-program"
                , Attr.type_ "submit"
                , Attr.class "mt-4 p-2 bg-white border text-sm text-left cursor-pointer"
                , Events.onClick msg
                ]
                [ Html.i [ Attr.class <| "mr-2 fa-solid " ++ icon ] []
                , Html.text label
                ]

        viewClearButton =
            Html.button
                [ Attr.id "clear-program"
                , Attr.type_ "submit"
                , Attr.class "ml-4 mt-4 p-2 bg-white border text-sm text-left cursor-pointer"
                , Events.onClick ClearOutput
                ]
                [ Html.i [ Attr.class "fa-solid fa-xmark mr-2" ] []
                , Html.text "CLEAR"
                ]

        viewResult =
            let
                chunkString str =
                    str
                        |> String.toList
                        |> List.greedyGroupsOf 59
                        |> List.intersperse [ '\n' ]
                        |> List.concat
                        |> String.fromList

                stateToLabel state =
                    case state of
                        Running ->
                            "Running"

                        Paused ->
                            "Paused"

                        Finished ->
                            "Finished"

                        Crashed reason ->
                            "Crash: " ++ reason

                status =
                    case model.form of
                        Initial ->
                            "Initial"

                        Failed reason ->
                            "Failed: " ++ reason

                        Loaded context ->
                            context.state |> stateToLabel

                response =
                    case model.form of
                        Initial ->
                            ""

                        Failed reason ->
                            ""

                        Loaded context ->
                            context.vm.output
            in
            Html.div []
                [ Html.div
                    [ Attr.id "response-type"
                    , Attr.class "flex flex-row"
                    ]
                    [ Html.span [ Attr.class "w-1/6" ] [ Html.text "RESPONSE" ]
                    , Html.span [ Attr.class "w-5/6" ] [ Html.text "TEXT" ]
                    ]
                , Html.div
                    [ Attr.id "response-status"
                    , Attr.class "flex flex-row"
                    ]
                    [ Html.span [ Attr.class "w-1/6" ] [ Html.text "STATUS" ]
                    , Html.span [ Attr.class "w-5/6" ] [ Html.text status ]
                    ]
                , Html.div
                    [ Attr.id "response-body"
                    , Attr.class "flex flex-row"
                    ]
                    [ Html.span [ Attr.class "w-1/6" ] [ Html.text "DATA" ]
                    , Html.pre
                        [ Attr.id "code-output"
                        , Attr.class "w-5/6 border-none outline-none resize-none overflow-x-scroll"
                        ]
                        [ Html.text response ]
                    ]
                ]

        viewDescription =
            Html.div [ Attr.id "function-description" ]
                [ Html.text "Once the PROMPT text has been entered, hit CTRL plus ENTER to send the program from the terminal to the BF-4000 mainframe for execution. Once completed, the STATUS field will display the message SUCCESS along with the output of the program in the DATA field."
                , Html.br [] []
                , Html.br [] []
                , Html.text "If the STATUS field displays ERROR then the DATA field includes a description of the error encountered during execution."
                , Html.br [] []
                , Html.br [] []
                , Html.text "A reference to the BF-4000 machine language can be found on pages 138-149."
                ]
    in
    Html.div
        [ Attr.id "interpreter-section"
        , Attr.class "mt-4 text-sm"
        ]
        [ viewSubHeader
        , Html.hr [ Attr.class "mt-1.5" ] []
        , Html.hr [ Attr.class "mt-0.25" ] []
        , viewSource
        , Html.hr [ Attr.class "mt-2" ] []
        , viewInput
        , viewRunButton
        , viewClearButton
        , Html.hr [ Attr.class "my-2" ] []
        , viewResult
        , Html.hr [ Attr.class "my-2" ] []
        , viewDescription
        , Html.hr [ Attr.class "my-2" ] []
        , viewFooter 127
        ]


viewFooter : Int -> Html msg
viewFooter pageNumber =
    Html.div
        [ Attr.id "footer"
        , Attr.class "mt-4 mb-4 flex flex-row justify-between"
        ]
        [ Html.span [] [ Html.text "DENMARK" ]
        , Html.span [ Attr.class "font-semibold" ] [ Html.text "Tabulating the Future™" ]
        , Html.span [] [ Html.text <| "Page " ++ String.fromInt pageNumber ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
