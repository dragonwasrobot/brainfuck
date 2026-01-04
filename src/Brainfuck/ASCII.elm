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
    -- TODO: Include description of all chars
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
        , { dec = 32, hex = "20", char = "SPACE", description = Nothing }
        , { dec = 33, hex = "21", char = "!", description = Nothing }
        , { dec = 34, hex = "22", char = "\"", description = Nothing }
        , { dec = 35, hex = "23", char = "#", description = Nothing }
        , { dec = 36, hex = "24", char = "$", description = Nothing }
        , { dec = 37, hex = "25", char = "%", description = Nothing }
        , { dec = 38, hex = "26", char = "&", description = Nothing }
        , { dec = 39, hex = "27", char = "'", description = Nothing }
        , { dec = 40, hex = "28", char = "(", description = Nothing }
        , { dec = 41, hex = "29", char = ")", description = Nothing }
        , { dec = 42, hex = "2A", char = "*", description = Nothing }
        , { dec = 43, hex = "2B", char = "+", description = Nothing }
        , { dec = 44, hex = "2C", char = ",", description = Nothing }
        , { dec = 45, hex = "2D", char = "-", description = Nothing }
        , { dec = 46, hex = "2E", char = ".", description = Nothing }
        , { dec = 47, hex = "2F", char = "/", description = Nothing }
        , { dec = 48, hex = "30", char = "0", description = Nothing }
        , { dec = 49, hex = "31", char = "1", description = Nothing }
        , { dec = 50, hex = "32", char = "2", description = Nothing }
        , { dec = 51, hex = "33", char = "3", description = Nothing }
        , { dec = 52, hex = "34", char = "4", description = Nothing }
        , { dec = 53, hex = "35", char = "5", description = Nothing }
        , { dec = 54, hex = "36", char = "6", description = Nothing }
        , { dec = 55, hex = "37", char = "7", description = Nothing }
        , { dec = 56, hex = "38", char = "8", description = Nothing }
        , { dec = 57, hex = "39", char = "9", description = Nothing }
        , { dec = 58, hex = "3A", char = ":", description = Nothing }
        , { dec = 59, hex = "3B", char = ";", description = Nothing }
        , { dec = 60, hex = "3C", char = "<", description = Nothing }
        , { dec = 61, hex = "3D", char = "=", description = Nothing }
        , { dec = 62, hex = "3E", char = ">", description = Nothing }
        , { dec = 63, hex = "3F", char = "?", description = Nothing }
        , { dec = 64, hex = "40", char = "@", description = Nothing }
        , { dec = 65, hex = "41", char = "A", description = Nothing }
        , { dec = 66, hex = "42", char = "B", description = Nothing }
        , { dec = 67, hex = "43", char = "C", description = Nothing }
        , { dec = 68, hex = "44", char = "D", description = Nothing }
        , { dec = 69, hex = "45", char = "E", description = Nothing }
        , { dec = 70, hex = "46", char = "F", description = Nothing }
        , { dec = 71, hex = "47", char = "G", description = Nothing }
        , { dec = 72, hex = "48", char = "H", description = Nothing }
        , { dec = 73, hex = "49", char = "I", description = Nothing }
        , { dec = 74, hex = "4A", char = "J", description = Nothing }
        , { dec = 75, hex = "4B", char = "K", description = Nothing }
        , { dec = 76, hex = "4C", char = "L", description = Nothing }
        , { dec = 77, hex = "4D", char = "M", description = Nothing }
        , { dec = 78, hex = "4E", char = "N", description = Nothing }
        , { dec = 79, hex = "4F", char = "O", description = Nothing }
        , { dec = 80, hex = "50", char = "P", description = Nothing }
        , { dec = 81, hex = "51", char = "Q", description = Nothing }
        , { dec = 82, hex = "52", char = "R", description = Nothing }
        , { dec = 83, hex = "53", char = "S", description = Nothing }
        , { dec = 84, hex = "54", char = "T", description = Nothing }
        , { dec = 85, hex = "55", char = "U", description = Nothing }
        , { dec = 86, hex = "56", char = "V", description = Nothing }
        , { dec = 87, hex = "57", char = "W", description = Nothing }
        , { dec = 88, hex = "58", char = "X", description = Nothing }
        , { dec = 89, hex = "59", char = "Y", description = Nothing }
        , { dec = 90, hex = "5A", char = "Z", description = Nothing }
        , { dec = 91, hex = "5B", char = "[", description = Nothing }
        , { dec = 92, hex = "5C", char = "\\", description = Nothing }
        , { dec = 93, hex = "5D", char = "]", description = Nothing }
        , { dec = 94, hex = "5E", char = "^", description = Nothing }
        , { dec = 95, hex = "5F", char = "_", description = Nothing }
        , { dec = 96, hex = "60", char = "`", description = Nothing }
        , { dec = 97, hex = "61", char = "a", description = Nothing }
        , { dec = 98, hex = "62", char = "b", description = Nothing }
        , { dec = 99, hex = "63", char = "c", description = Nothing }
        , { dec = 100, hex = "64", char = "d", description = Nothing }
        , { dec = 101, hex = "65", char = "e", description = Nothing }
        , { dec = 102, hex = "66", char = "f", description = Nothing }
        , { dec = 103, hex = "67", char = "g", description = Nothing }
        , { dec = 104, hex = "68", char = "h", description = Nothing }
        , { dec = 105, hex = "69", char = "i", description = Nothing }
        , { dec = 106, hex = "6A", char = "j", description = Nothing }
        , { dec = 107, hex = "6B", char = "k", description = Nothing }
        , { dec = 108, hex = "6C", char = "l", description = Nothing }
        , { dec = 109, hex = "6D", char = "m", description = Nothing }
        , { dec = 110, hex = "6E", char = "n", description = Nothing }
        , { dec = 111, hex = "6F", char = "o", description = Nothing }
        , { dec = 112, hex = "70", char = "p", description = Nothing }
        , { dec = 113, hex = "71", char = "q", description = Nothing }
        , { dec = 114, hex = "72", char = "r", description = Nothing }
        , { dec = 115, hex = "73", char = "s", description = Nothing }
        , { dec = 116, hex = "74", char = "t", description = Nothing }
        , { dec = 117, hex = "75", char = "u", description = Nothing }
        , { dec = 118, hex = "76", char = "v", description = Nothing }
        , { dec = 119, hex = "77", char = "w", description = Nothing }
        , { dec = 120, hex = "78", char = "x", description = Nothing }
        , { dec = 121, hex = "79", char = "y", description = Nothing }
        , { dec = 122, hex = "7A", char = "z", description = Nothing }
        , { dec = 123, hex = "7B", char = "{", description = Nothing }
        , { dec = 124, hex = "7C", char = "|", description = Nothing }
        , { dec = 125, hex = "7D", char = "}", description = Nothing }
        , { dec = 126, hex = "7E", char = "~", description = Nothing }
        , { dec = 127, hex = "7F", char = "DEL", description = Nothing }
        ]
