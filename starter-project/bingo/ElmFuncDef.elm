module ElmFuncDef exposing (..)

import Html exposing (..)

--Top level declarations are separated by 2 spaces ( vertically)
playerInfo name gameNumber =
    name ++ " - Game # " ++ gameNumber

playerInfoText name gameNumber = 
    playerInfo name gameNumber
        |> String.toUpper
        |> Html.text

main = 
    playerInfoText "Vaishnav" "3"

