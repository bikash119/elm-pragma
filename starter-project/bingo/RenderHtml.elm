module RenderHtml exposing (..)

import Html exposing (text, h2)
import Html.Attributes exposing (..)

playerInfo name gameNumber = 
    name ++ " - Game # " ++ (toString gameNumber)


playerInfoText name gameNumber =
    playerInfo name gameNumber 
        |> String.toUpper
        |> text


-- main = 
-- --    Html.h2 [ attributes ] [ child node]
--     h2 [id "info", class "classy"]
--         [ playerInfoText "mike" 3]

viewPlayer name gameNumber = 
    h2 [id "info", class "classy"]
         [ playerInfoText name gameNumber]


main = 
    viewPlayer "Vaishnav" 4