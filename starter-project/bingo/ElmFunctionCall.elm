module BingoFunctionCall exposing (..)

import Html exposing (..)

-- main =
--     Html.text (String.repeat 3 (String.toUpper "bikash"))

-- PIPING
main =
    "bikash " 
        |> String.toUpper
        |> String.repeat 3
        |> String.pad 100 '*'
        |> Html.text