module View.Interpreter exposing (Model, Msg, init, update, view)

import Brainfuck.ASCII as ASCII exposing (ASCII, Byte)
import Brainfuck.Evaluator as Evaluator exposing (EvaluationContext, State(..))
import Brainfuck.Optimizer as Optimizer
import Brainfuck.Parser as Parser
import Brainfuck.VirtualMachine as VirtualMachine exposing (VirtualMachine)
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



-- ** MODEL


init : Model
init =
    { inputCode = ""
    , inputMode = TextMode
    , inputData = []
    , optimizeCode = False
    , form = Initial
    }


type alias Model =
    { inputCode : Code
    , inputMode : InputMode
    , inputData : List Byte
    , optimizeCode : Bool
    , form : FormState
    }


type alias Code =
    String


type InputMode
    = ByteMode String
    | TextMode


type FormState
    = Initial
    | Failed String
    | Loaded EvaluationContext



-- ** UPDATE


type Msg
    = StartEvaluation
    | StopEvaluation
    | ResumeEvaluation
    | EvaluateStep
    | ChangeMode
    | ToggleOptimization
    | ResetState
    | SetCode Code
    | SetInput String


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

        ChangeMode ->
            changeMode model

        ToggleOptimization ->
            toggleOptimization model

        ResetState ->
            resetState model

        SetCode code ->
            setCode code model

        SetInput input ->
            setInput input model


startEvaluation : Model -> ( Model, Cmd Msg )
startEvaluation model =
    let
        parsedProgram =
            if model.optimizeCode then
                model.inputCode
                    |> Parser.parse
                    |> Result.map Optimizer.optimize

            else
                model.inputCode
                    |> Parser.parse
    in
    case parsedProgram of
        Err error ->
            let
                message =
                    "Failed to parse program due to: " ++ error
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


changeMode : Model -> ( Model, Cmd Msg )
changeMode model =
    case model.inputMode of
        TextMode ->
            ( { model | inputMode = ByteMode "", inputData = [] }, Cmd.none )

        ByteMode _ ->
            ( { model | inputMode = TextMode, inputData = [] }, Cmd.none )


toggleOptimization : Model -> ( Model, Cmd Msg )
toggleOptimization model =
    if model.optimizeCode then
        ( { model | optimizeCode = False }, Cmd.none )

    else
        ( { model | optimizeCode = True }, Cmd.none )


resetState : Model -> ( Model, Cmd Msg )
resetState model =
    ( { model | form = Initial }, Cmd.none )


resetVirtualMachine : List Byte -> VirtualMachine
resetVirtualMachine data =
    let
        eofByte =
            0
    in
    VirtualMachine.init <| data ++ [ eofByte ]


setCode : Code -> Model -> ( Model, Cmd Msg )
setCode code model =
    ( { model | inputCode = code }, Cmd.none )


padHex : String -> String
padHex =
    String.padLeft 2 '0'


setInput : String -> Model -> ( Model, Cmd Msg )
setInput input model =
    let
        isOddLength : String -> Bool
        isOddLength str =
            modBy 2 (String.length str) == 1

        isValidChar : String -> Bool
        isValidChar str =
            str
                |> String.toList
                |> List.head
                |> Maybe.unwrap False Char.isHexDigit

        toByte : String -> Maybe Int
        toByte =
            String.toLower >> Hex.fromString >> Result.toMaybe

        trimHexPrefix : String -> String
        trimHexPrefix str =
            str
                |> String.replace "0x" ""
                |> String.replace " " ""

        numberString =
            trimHexPrefix input

        newInputData =
            case model.inputMode of
                TextMode ->
                    input
                        |> String.toList
                        |> List.map Char.toCode

                ByteMode _ ->
                    if isOddLength numberString then
                        numberString
                            |> String.dropRight 1
                            |> String.break 2
                            |> List.map toByte
                            |> Maybe.values

                    else
                        numberString
                            |> String.break 2
                            |> List.map toByte
                            |> Maybe.values

        newInputMode =
            case model.inputMode of
                TextMode ->
                    TextMode

                ByteMode _ ->
                    let
                        lastChar =
                            String.right 1 numberString
                    in
                    if isOddLength numberString && isValidChar lastChar then
                        ByteMode lastChar

                    else
                        ByteMode ""
    in
    ( { model | inputData = newInputData, inputMode = newInputMode }, Cmd.none )



-- ** VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.id "interpreter-section"
        , Attr.class "mt-4 text-sm"
        ]
        [ viewInterpreterTitle
        , viewInterpreterForm model
        ]


viewInterpreterTitle : Html msg
viewInterpreterTitle =
    Html.div
        [ Attr.id "title-section"
        , Attr.class "mx-auto px-6 py-2 w-fit text-sm text-center border border-[0.125em]"
        ]
        [ Html.text "DATA SHEET"
        , Html.br [] []
        , Html.text "BF-4000 MACHINE"
        ]


viewInterpreterForm : Model -> Html Msg
viewInterpreterForm model =
    let
        viewSubHeader =
            Html.div [ Attr.id "subheader" ]
                [ Html.span [] [ Html.text "PROGRAM EXECUTION" ] ]
    in
    Html.div
        [ Attr.id "interpreter-form"
        , Attr.class "mt-4 text-sm"
        ]
        [ viewSubHeader
        , Html.hr [ Attr.class "mt-1.5" ] []
        , Html.hr [ Attr.class "mt-0.25" ] []
        , viewSourceArea model
        , Html.hr [ Attr.class "my-2" ] []
        , viewInputArea model
        , Html.hr [ Attr.class "my-2" ] []
        , viewExecutionArea model
        , Html.hr [ Attr.class "my-2" ] []
        , viewDescriptionArea
        ]


viewSourceArea : Model -> Html Msg
viewSourceArea model =
    Html.div
        [ Attr.id "source-area"
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


viewInputArea : Model -> Html Msg
viewInputArea model =
    let
        modeLabel =
            case model.inputMode of
                ByteMode _ ->
                    "BYTE"

                TextMode ->
                    "TEXT"

        inputText =
            case model.inputMode of
                TextMode ->
                    model.inputData
                        |> List.map
                            (\byte ->
                                case ASCII.lookup byte of
                                    Nothing ->
                                        " 0x" ++ String.toUpper (Hex.toString byte) ++ " "

                                    Just ascii ->
                                        case ascii.char of
                                            "LF" ->
                                                "\n"

                                            "SPACE" ->
                                                " "

                                            char ->
                                                char
                            )
                        |> String.join ""

                ByteMode newChar ->
                    let
                        hexString =
                            model.inputData
                                |> List.map (\byte -> "0x" ++ (padHex <| String.toUpper <| Hex.toString <| byte))
                                |> String.join " "
                    in
                    if String.length newChar > 0 then
                        hexString ++ (" 0x" ++ String.toUpper newChar)

                    else
                        hexString

        viewInputField =
            Html.div
                [ Attr.id "input-field"
                , Attr.class "mt-1 flex flex-row"
                ]
                [ Html.span [ Attr.class "w-1/6" ] [ Html.text "INPUT" ]
                , Html.textarea
                    [ Attr.id "data-input"
                    , Attr.class "w-5/6 border-none outline-none resize-none"
                    , Attr.rows 5
                    , Attr.cols 60
                    , Attr.placeholder "ENTER DATA"
                    , Attr.value <| String.trimLeft inputText
                    , Events.onInput SetInput
                    ]
                    []
                ]

        viewModeLabel =
            Html.div
                [ Attr.id "response-type"
                , Attr.class "flex flex-row"
                ]
                [ Html.span [ Attr.class "w-1/6" ] [ Html.text "MODE" ]
                , Html.span [ Attr.class "w-5/6" ] [ Html.text modeLabel ]
                ]

        viewModeButton =
            Html.button
                [ Attr.id "change-mode"
                , Attr.type_ "submit"
                , Attr.class "mt-4 p-2 bg-white border text-sm text-left cursor-pointer"
                , Events.onClick ChangeMode
                ]
                [ Html.i [ Attr.class "fa-solid fa-terminal mr-2" ] []
                , Html.text "CHANGE MODE"
                ]

        viewOptimizationButton =
            let
                ( icon, label ) =
                    if model.optimizeCode then
                        ( "fa-battery-half", "DISABLE OPTIMIZATION" )

                    else
                        ( "fa-battery-full", "ENABLE OPTIMIZATION" )
            in
            Html.button
                [ Attr.id "toggle-optimization"
                , Attr.type_ "submit"
                , Attr.class "ml-4 mt-4 p-2 bg-white border text-sm text-left cursor-pointer"
                , Events.onClick ToggleOptimization
                ]
                [ Html.i [ Attr.class <| "mr-2 fa-solid " ++ icon ] []
                , Html.text label
                ]
    in
    Html.div [ Attr.id "input-area" ]
        [ viewModeLabel
        , viewInputField
        , viewModeButton
        , viewOptimizationButton
        ]


viewExecutionArea : Model -> Html Msg
viewExecutionArea model =
    let
        viewRunButton =
            let
                ( msg, icon, label ) =
                    case model.form of
                        Initial ->
                            ( StartEvaluation, "fa-play", "RUN PROGRAM" )

                        Failed _ ->
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

        viewResetButton =
            Html.button
                [ Attr.id "reset-program"
                , Attr.type_ "submit"
                , Attr.class "ml-4 mt-4 p-2 bg-white border text-sm text-left cursor-pointer"
                , Events.onClick ResetState
                ]
                [ Html.i [ Attr.class "fa-solid fa-xmark mr-2" ] []
                , Html.text "RESET"
                ]

        viewResult =
            let
                stateToLabel state =
                    case state of
                        Running ->
                            "RUNNING"

                        Paused ->
                            "PAUSED"

                        Finished ->
                            "FINISHED"

                        Crashed reason ->
                            "CRASH: " ++ reason

                status =
                    case model.form of
                        Initial ->
                            "INITIAL"

                        Failed reason ->
                            "FAILED: " ++ reason

                        Loaded context ->
                            context.state |> stateToLabel

                response =
                    case model.form of
                        Initial ->
                            ""

                        Failed reason ->
                            ""

                        Loaded context ->
                            let
                                toPrintableChar byte =
                                    case ASCII.lookup byte of
                                        Nothing ->
                                            "0x" ++ (byte |> Hex.toString |> String.toUpper |> padHex)

                                        Just ascii ->
                                            case ascii.char of
                                                "LF" ->
                                                    "\n"

                                                "SPACE" ->
                                                    " "

                                                char ->
                                                    char

                                toPrintableHex byte =
                                    "0x" ++ (byte |> Hex.toString |> String.toUpper |> padHex)
                            in
                            case model.inputMode of
                                TextMode ->
                                    context.vm.output
                                        |> List.map toPrintableChar
                                        |> String.join ""

                                ByteMode _ ->
                                    context.vm.output
                                        |> List.map toPrintableHex
                                        |> String.join " "
            in
            Html.div []
                [ Html.div
                    [ Attr.id "response-status"
                    , Attr.class "flex flex-row"
                    ]
                    [ Html.span [ Attr.class "w-1/6" ] [ Html.text "STATUS" ]
                    , Html.span [ Attr.class "w-5/6" ] [ Html.text status ]
                    ]
                , Html.div
                    [ Attr.id "response-body"
                    , Attr.class "mt-1 flex flex-row"
                    ]
                    [ Html.span [ Attr.class "w-1/6" ] [ Html.text "OUTPUT" ]
                    , Html.textarea
                        [ Attr.id "code-output"
                        , Attr.disabled True
                        , Attr.rows 5
                        , Attr.cols 60
                        , Attr.class "w-5/6 border-none outline-none resize-none whitespace-normal"
                        ]
                        [ Html.text response ]
                    ]
                ]
    in
    Html.div [ Attr.id "execution-area" ]
        [ viewResult
        , viewRunButton
        , viewResetButton
        ]


viewDescriptionArea : Html msg
viewDescriptionArea =
    Html.div [ Attr.id "function-description" ]
        [ Html.text "Once the SOURCE text has been entered, hit CTRL plus ENTER to send the program from the terminal to the BF-4000 mainframe for execution. Once completed, the STATUS field will display the message SUCCESS along with the output of the program in the OUTPUT field."
        , Html.br [] []
        , Html.br [] []
        , Html.text "If the STATUS field displays ERROR then the OUTPUT field includes a description of the error encountered during execution."
        , Html.br [] []
        , Html.br [] []
        , Html.text "A reference to the BF-4000 machine language can be found on pages 138-149."
        ]
