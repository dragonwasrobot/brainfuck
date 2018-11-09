module Parser exposing (AbstractSyntaxTree(..), Block, Command, parse)

import Lexer exposing (Position, Symbol(..), Token)


{-| The parser takes a sequence of tokens and constructs a parse tree, an
abstract syntax tree.


## Grammar

The grammar of brainfuck is very simple (we ignore comments):

    <program> ::= { <block> | <command> }
    <block>   ::= '[' { <block> | <command> } ']'
    <command> ::= '>' | '<' | '+' | '-' | '.' | ','

-}
type AbstractSyntaxTree
    = Leaf Command
    | Node Block


type alias Command =
    { value : Symbol
    , position : Position
    }


type alias Block =
    { children : List AbstractSyntaxTree
    , position : Position
    }


{-| The Parser construct an abstract syntax tree based on the list of tokens.
-}
parse : List Token -> Result String AbstractSyntaxTree
parse tokens =
    let
        root =
            Node
                { position = { row = 0, column = 0 }
                , children = []
                }
    in
    root
        |> parseTokens tokens
        |> Result.andThen
            (\( ast, remainingTokens ) ->
                if List.isEmpty remainingTokens then
                    Ok ast

                else
                    Err "Finished parsing too early!"
            )


{-| `parseTokens` takes a list of tokens and returns an abstract syntax
tree of the program.
-}
parseTokens :
    List Token
    -> AbstractSyntaxTree
    -> Result String ( AbstractSyntaxTree, List Token )
parseTokens tokensList node =
    case tokensList of
        [] ->
            Ok ( node, [] )

        token :: tokens ->
            case token.value of
                StartBlock ->
                    parseStartBlock token tokens node

                EndBlock ->
                    parseEndBlock tokens node

                _ ->
                    parseCommand token tokens node


parseStartBlock :
    Token
    -> List Token
    -> AbstractSyntaxTree
    -> Result String ( AbstractSyntaxTree, List Token )
parseStartBlock token tokens node =
    case node of
        Leaf _ ->
            Err "Unexpected command!"

        Node block ->
            let
                childBlock =
                    Node
                        { children = []
                        , position = token.position
                        }
            in
            childBlock
                |> parseTokens tokens
                |> Result.andThen
                    (\( parsedChildBlock, remainingTokens ) ->
                        let
                            newChildren =
                                block.children ++ [ parsedChildBlock ]

                            newNode =
                                Node { block | children = newChildren }
                        in
                        parseTokens remainingTokens newNode
                    )


parseEndBlock :
    List Token
    -> AbstractSyntaxTree
    -> Result String ( AbstractSyntaxTree, List Token )
parseEndBlock tokens node =
    Ok ( node, tokens )


parseCommand :
    Token
    -> List Token
    -> AbstractSyntaxTree
    -> Result String ( AbstractSyntaxTree, List Token )
parseCommand token tokens node =
    case node of
        Leaf _ ->
            Err "Enclosing block was command!"

        Node block ->
            let
                command =
                    Leaf
                        { value = token.value
                        , position = token.position
                        }

                newChildren =
                    block.children ++ [ command ]

                newNode =
                    Node { block | children = newChildren }
            in
            parseTokens tokens newNode
