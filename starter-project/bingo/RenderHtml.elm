module RenderHtml exposing (..)

import Html exposing (text, h2,header,h1,a,footer,text,div)
import Html.Attributes exposing (..)

playerInfo name gameNumber = 
    name ++ " - Game # " ++ (toString gameNumber)





-- main = 
-- --    Html.h2 [ attributes ] [ child node]
--     h2 [id "info", class "classy"]
--         [ playerInfoText "mike" 3]

viewPlayer name gameNumber = 
    let
        playerInfoText  =
            playerInfo name gameNumber 
                |> String.toUpper
                |> text
    in
    
        h2 [id "info", class "classy"]
             [ playerInfoText ]

--viewHeader, viewFooter, view are definitions as they do not take any value. You can't have a function with zero arguments in Elm
viewHeader title= 
    header []
            [ h1 [] [ text title]]


viewFooter =
    footer []
            [ a [ href "http://elm-lang.org" ]
                [ text "Powered by me"]
            ]


view = 
    div [ class "content"]
        [ viewHeader "Buzzword Bingo"
        ,viewPlayer "Vaishnav" 4
        ,viewFooter 
        ]
        
main = 
    view