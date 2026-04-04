module Brainfuck.ASCII exposing (ASCII, Byte, lookup, table)

import Array exposing (Array)


type alias Byte =
    Int


type alias ASCII =
    { dec : Byte
    , hex : String
    , char : String
    , description : Maybe String
    }


lookup : Byte -> Maybe ASCII
lookup byteValue =
    Array.get byteValue table


table : Array ASCII
table =
    Array.fromList
        [ { dec = 0, hex = "00", char = "NUL", description = Just "null" }
        , { dec = 1, hex = "01", char = "SOH", description = Just "start of heading" }
        , { dec = 2, hex = "02", char = "STX", description = Just "start of text" }
        , { dec = 3, hex = "03", char = "ETX", description = Just "end of text" }
        , { dec = 4, hex = "04", char = "EOT", description = Just "end of transmission" }
        , { dec = 5, hex = "05", char = "ENQ", description = Just "enquiry" }
        , { dec = 6, hex = "06", char = "ACK", description = Just "acknowledge" }
        , { dec = 7, hex = "07", char = "BEL", description = Just "bell" }
        , { dec = 8, hex = "08", char = "BS", description = Just "backspace" }
        , { dec = 9, hex = "09", char = "HT", description = Just "horizontal tab" }
        , { dec = 10, hex = "0A", char = "LF", description = Just "line feed" }
        , { dec = 11, hex = "0B", char = "VT", description = Just "vertical tab" }
        , { dec = 12, hex = "0C", char = "FF", description = Just "form feed" }
        , { dec = 13, hex = "0D", char = "CR", description = Just "carriage return" }
        , { dec = 14, hex = "0E", char = "SO", description = Just "shift out" }
        , { dec = 15, hex = "0F", char = "SI", description = Just "shift in" }
        , { dec = 16, hex = "10", char = "DLE", description = Just "data link escape" }
        , { dec = 17, hex = "11", char = "DC1", description = Just "device control 1" }
        , { dec = 18, hex = "12", char = "DC2", description = Just "device control 2" }
        , { dec = 19, hex = "13", char = "DC3", description = Just "device control 3" }
        , { dec = 20, hex = "14", char = "DC4", description = Just "device control 4" }
        , { dec = 21, hex = "15", char = "NAK", description = Just "negative acknowledge" }
        , { dec = 22, hex = "16", char = "SYN", description = Just "synchronous idle" }
        , { dec = 23, hex = "17", char = "ETB", description = Just "end of transmission block" }
        , { dec = 24, hex = "18", char = "CAN", description = Just "cancel" }
        , { dec = 25, hex = "19", char = "EM", description = Just "end of medium" }
        , { dec = 26, hex = "1A", char = "SUB", description = Just "substitute" }
        , { dec = 27, hex = "1B", char = "ESC", description = Just "escape" }
        , { dec = 28, hex = "1C", char = "FS", description = Just "file separator" }
        , { dec = 29, hex = "1D", char = "GS", description = Just "group separator" }
        , { dec = 30, hex = "1E", char = "RS", description = Just "record separator" }
        , { dec = 31, hex = "1F", char = "US", description = Just "unit separator" }
        , { dec = 32, hex = "20", char = "SPACE", description = Just "space" }
        , { dec = 33, hex = "21", char = "!", description = Just "exclamation mark" }
        , { dec = 34, hex = "22", char = "\"", description = Just "quotation mark" }
        , { dec = 35, hex = "23", char = "#", description = Just "number sign" }
        , { dec = 36, hex = "24", char = "$", description = Just "dollar sign" }
        , { dec = 37, hex = "25", char = "%", description = Just "percent sign" }
        , { dec = 38, hex = "26", char = "&", description = Just "ampersand" }
        , { dec = 39, hex = "27", char = "'", description = Just "apostrophe" }
        , { dec = 40, hex = "28", char = "(", description = Just "left parenthesis" }
        , { dec = 41, hex = "29", char = ")", description = Just "right parenthesis" }
        , { dec = 42, hex = "2A", char = "*", description = Just "asterisk" }
        , { dec = 43, hex = "2B", char = "+", description = Just "plus sign" }
        , { dec = 44, hex = "2C", char = ",", description = Just "comma" }
        , { dec = 45, hex = "2D", char = "-", description = Just "hyphen" }
        , { dec = 46, hex = "2E", char = ".", description = Just "full stop" }
        , { dec = 47, hex = "2F", char = "/", description = Just "forward slash" }
        , { dec = 48, hex = "30", char = "0", description = Just "digit zero" }
        , { dec = 49, hex = "31", char = "1", description = Just "digit one" }
        , { dec = 50, hex = "32", char = "2", description = Just "digit two" }
        , { dec = 51, hex = "33", char = "3", description = Just "digit three" }
        , { dec = 52, hex = "34", char = "4", description = Just "digit four" }
        , { dec = 53, hex = "35", char = "5", description = Just "digit five" }
        , { dec = 54, hex = "36", char = "6", description = Just "digit six" }
        , { dec = 55, hex = "37", char = "7", description = Just "digit seven" }
        , { dec = 56, hex = "38", char = "8", description = Just "digit eight" }
        , { dec = 57, hex = "39", char = "9", description = Just "digit nine" }
        , { dec = 58, hex = "3A", char = ":", description = Just "colon" }
        , { dec = 59, hex = "3B", char = ";", description = Just "semicolon" }
        , { dec = 60, hex = "3C", char = "<", description = Just "less-than sign" }
        , { dec = 61, hex = "3D", char = "=", description = Just "equals sign" }
        , { dec = 62, hex = "3E", char = ">", description = Just "greater-than sign" }
        , { dec = 63, hex = "3F", char = "?", description = Just "question mark" }
        , { dec = 64, hex = "40", char = "@", description = Just "at sign" }
        , { dec = 65, hex = "41", char = "A", description = Just "capital letter A" }
        , { dec = 66, hex = "42", char = "B", description = Just "capital letter B" }
        , { dec = 67, hex = "43", char = "C", description = Just "capital letter C" }
        , { dec = 68, hex = "44", char = "D", description = Just "capital letter D" }
        , { dec = 69, hex = "45", char = "E", description = Just "capital letter E" }
        , { dec = 70, hex = "46", char = "F", description = Just "capital letter F" }
        , { dec = 71, hex = "47", char = "G", description = Just "capital letter G" }
        , { dec = 72, hex = "48", char = "H", description = Just "capital letter H" }
        , { dec = 73, hex = "49", char = "I", description = Just "capital letter I" }
        , { dec = 74, hex = "4A", char = "J", description = Just "capital letter J" }
        , { dec = 75, hex = "4B", char = "K", description = Just "capital letter K" }
        , { dec = 76, hex = "4C", char = "L", description = Just "capital letter L" }
        , { dec = 77, hex = "4D", char = "M", description = Just "capital letter M" }
        , { dec = 78, hex = "4E", char = "N", description = Just "capital letter N" }
        , { dec = 79, hex = "4F", char = "O", description = Just "capital letter O" }
        , { dec = 80, hex = "50", char = "P", description = Just "capital letter P" }
        , { dec = 81, hex = "51", char = "Q", description = Just "capital letter Q" }
        , { dec = 82, hex = "52", char = "R", description = Just "capital letter R" }
        , { dec = 83, hex = "53", char = "S", description = Just "capital letter S" }
        , { dec = 84, hex = "54", char = "T", description = Just "capital letter T" }
        , { dec = 85, hex = "55", char = "U", description = Just "capital letter U" }
        , { dec = 86, hex = "56", char = "V", description = Just "capital letter V" }
        , { dec = 87, hex = "57", char = "W", description = Just "capital letter W" }
        , { dec = 88, hex = "58", char = "X", description = Just "capital letter X" }
        , { dec = 89, hex = "59", char = "Y", description = Just "capital letter Y" }
        , { dec = 90, hex = "5A", char = "Z", description = Just "capital letter Z" }
        , { dec = 91, hex = "5B", char = "[", description = Just "left square bracket" }
        , { dec = 92, hex = "5C", char = "\\", description = Just "backslash" }
        , { dec = 93, hex = "5D", char = "]", description = Just "right square bracket" }
        , { dec = 94, hex = "5E", char = "^", description = Just "caret accent" }
        , { dec = 95, hex = "5F", char = "_", description = Just "underscore" }
        , { dec = 96, hex = "60", char = "`", description = Just "backtick" }
        , { dec = 97, hex = "61", char = "a", description = Just "small letter a" }
        , { dec = 98, hex = "62", char = "b", description = Just "small letter b" }
        , { dec = 99, hex = "63", char = "c", description = Just "small letter c" }
        , { dec = 100, hex = "64", char = "d", description = Just "small letter d" }
        , { dec = 101, hex = "65", char = "e", description = Just "small letter e" }
        , { dec = 102, hex = "66", char = "f", description = Just "small letter f" }
        , { dec = 103, hex = "67", char = "g", description = Just "small letter g" }
        , { dec = 104, hex = "68", char = "h", description = Just "small letter h" }
        , { dec = 105, hex = "69", char = "i", description = Just "small letter i" }
        , { dec = 106, hex = "6A", char = "j", description = Just "small letter j" }
        , { dec = 107, hex = "6B", char = "k", description = Just "small letter k" }
        , { dec = 108, hex = "6C", char = "l", description = Just "small letter l" }
        , { dec = 109, hex = "6D", char = "m", description = Just "small letter m" }
        , { dec = 110, hex = "6E", char = "n", description = Just "small letter n" }
        , { dec = 111, hex = "6F", char = "o", description = Just "small letter o" }
        , { dec = 112, hex = "70", char = "p", description = Just "small letter p" }
        , { dec = 113, hex = "71", char = "q", description = Just "small letter q" }
        , { dec = 114, hex = "72", char = "r", description = Just "small letter r" }
        , { dec = 115, hex = "73", char = "s", description = Just "small letter s" }
        , { dec = 116, hex = "74", char = "t", description = Just "small letter t" }
        , { dec = 117, hex = "75", char = "u", description = Just "small letter u" }
        , { dec = 118, hex = "76", char = "v", description = Just "small letter v" }
        , { dec = 119, hex = "77", char = "w", description = Just "small letter w" }
        , { dec = 120, hex = "78", char = "x", description = Just "small letter x" }
        , { dec = 121, hex = "79", char = "y", description = Just "small letter y" }
        , { dec = 122, hex = "7A", char = "z", description = Just "small letter z" }
        , { dec = 123, hex = "7B", char = "{", description = Just "left curly bracket" }
        , { dec = 124, hex = "7C", char = "|", description = Just "vertical bar" }
        , { dec = 125, hex = "7D", char = "}", description = Just "right curly bracket" }
        , { dec = 126, hex = "7E", char = "~", description = Just "tilde" }
        , { dec = 127, hex = "7F", char = "DEL", description = Just "delete" }
        ]
