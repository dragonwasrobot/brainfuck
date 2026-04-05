module View.ReferenceManual exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr



-- ** VIEW


view : Html msg
view =
    Html.div
        [ Attr.id "archives-section"
        , Attr.class "mt-4 min-h-164"
        ]
        [ viewIntroduction
        , viewLanguageSpec
        ]


viewIntroduction : Html msg
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


viewLanguageSpec : Html msg
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


viewSourceFormatDescription : Html msg
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


viewGrammarDescription : Html msg
viewGrammarDescription =
    Html.div [ Attr.class "mt-4" ]
        [ Html.h3 [] [ Html.text "2. Grammar (BNF)" ]
        , Html.div [ Attr.class "mt-2 text-sm hyphens-auto" ] [ Html.span [] [ Html.text "The syntax of the BF-4000 machine language is defined according to the following Backus-Naur Form (BNF) grammar:" ] ]
        , Html.pre [ Attr.class "mx-8 my-4 py-4 border border-l-0 border-r-0 text-sm leading-6" ] [ Html.text "  <program> ::= <instr>*\n  <instr> ::= \">\" | \"<\" | \"+\" | \"-\" | \".\" | \",\" | <loop>\n  <loop> ::= \"[\" <instr>* \"]\"" ]
        , Html.div [ Attr.class "text-sm hyphens-auto" ] [ Html.span [] [ Html.text "Bracket pairing is determined by static nesting at translation time." ] ]
        ]


viewInstructionSemanticsDescription : Html msg
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


viewAbstractMachineDescription : Html msg
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
