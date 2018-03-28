port module Ports exposing (outputByte, evaluate)


port outputByte : Int -> Cmd msg


port evaluate : (String -> msg) -> Sub msg
