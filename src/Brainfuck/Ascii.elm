module Brainfuck.Ascii exposing (toChar)

{-| Get the ASCII char of the int `c`
-}


toChar : Int -> Maybe Char
toChar c =
    case c of
        0x0A ->
            Just '\n'

        0x20 ->
            Just ' '

        0x21 ->
            Just '!'

        0x22 ->
            Just '"'

        0x23 ->
            Just '#'

        0x24 ->
            Just '$'

        0x25 ->
            Just '%'

        0x26 ->
            Just '&'

        0x27 ->
            Just '"'

        0x28 ->
            Just '('

        0x29 ->
            Just ')'

        0x2A ->
            Just '*'

        0x2B ->
            Just '+'

        0x2C ->
            Just ','

        0x2D ->
            Just '-'

        0x2E ->
            Just '.'

        0x2F ->
            Just '/'

        0x30 ->
            Just '0'

        0x31 ->
            Just '1'

        0x32 ->
            Just '2'

        0x33 ->
            Just '3'

        0x34 ->
            Just '4'

        0x35 ->
            Just '5'

        0x36 ->
            Just '6'

        0x37 ->
            Just '7'

        0x38 ->
            Just '8'

        0x39 ->
            Just '9'

        0x3A ->
            Just ':'

        0x3B ->
            Just ';'

        0x3C ->
            Just '<'

        0x3D ->
            Just '='

        0x3E ->
            Just '>'

        0x3F ->
            Just '?'

        0x40 ->
            Just '@'

        0x41 ->
            Just 'A'

        0x42 ->
            Just 'B'

        0x43 ->
            Just 'C'

        0x44 ->
            Just 'D'

        0x45 ->
            Just 'E'

        0x46 ->
            Just 'F'

        0x47 ->
            Just 'G'

        0x48 ->
            Just 'H'

        0x49 ->
            Just 'I'

        0x4A ->
            Just 'J'

        0x4B ->
            Just 'K'

        0x4C ->
            Just 'L'

        0x4D ->
            Just 'M'

        0x4E ->
            Just 'N'

        0x4F ->
            Just 'O'

        0x50 ->
            Just 'P'

        0x51 ->
            Just 'Q'

        0x52 ->
            Just 'R'

        0x53 ->
            Just 'S'

        0x54 ->
            Just 'T'

        0x55 ->
            Just 'U'

        0x56 ->
            Just 'V'

        0x57 ->
            Just 'W'

        0x58 ->
            Just 'X'

        0x59 ->
            Just 'Y'

        0x5A ->
            Just 'Z'

        0x5B ->
            Just '['

        0x5C ->
            Just '\\'

        0x5D ->
            Just ']'

        0x5E ->
            Just '^'

        0x5F ->
            Just '_'

        0x60 ->
            Just '`'

        0x61 ->
            Just 'a'

        0x62 ->
            Just 'b'

        0x63 ->
            Just 'c'

        0x64 ->
            Just 'd'

        0x65 ->
            Just 'e'

        0x66 ->
            Just 'f'

        0x67 ->
            Just 'g'

        0x68 ->
            Just 'h'

        0x69 ->
            Just 'i'

        0x6A ->
            Just 'j'

        0x6B ->
            Just 'k'

        0x6C ->
            Just 'l'

        0x6D ->
            Just 'm'

        0x6E ->
            Just 'n'

        0x6F ->
            Just 'o'

        0x70 ->
            Just 'p'

        0x71 ->
            Just 'q'

        0x72 ->
            Just 'r'

        0x73 ->
            Just 's'

        0x74 ->
            Just 't'

        0x75 ->
            Just 'u'

        0x76 ->
            Just 'v'

        0x77 ->
            Just 'w'

        0x78 ->
            Just 'x'

        0x79 ->
            Just 'y'

        0x7A ->
            Just 'z'

        0x7B ->
            Just '{'

        0x7C ->
            Just '|'

        0x7D ->
            Just '}'

        0x7E ->
            Just '~'

        _ ->
            Nothing
