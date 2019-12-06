module RenderHtml exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(onClick)
import List exposing (sortBy )
import Random
import Http

--MODEL


type alias Entry = 
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }

type alias Model = 
    { name : String
    , gameNumber : Int
    , entries : List Entry 
    }

initialModel : Model
initialModel = 
    {
        name = "mike"
    ,   gameNumber = 1
    ,   entries = []
    }

--update

type Msg = NewGame | Mark Int | Sort | NewRandom Int | NewEntries (Result Http.Error String)

allEntriesMarked : List Entry -> Bool
allEntriesMarked entries =
    let
        marked e = e.marked
    in
        List.all marked entries


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        NewGame ->
            ({ model |  gameNumber = model.gameNumber + 1}, getEntries )
        Mark id ->
            let 
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e

            in
                ({ model | entries = List.map markEntry model.entries}, Cmd.none)
        Sort ->
            let
                sortByPoints e =
                    e.points
            in
                ( { model | entries = List.sortBy  sortByPoints model.entries }, Cmd.none )
                
        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber } , Cmd.none)
        
        NewEntries (Ok jsonString) ->
            let
                _ = Debug.log "it worked!" jsonString
            in
                (model, Cmd.none )
        NewEntries (Err error) ->
            let
                _ = Debug.log "oops!" error
            in
                (model, Cmd.none)



--COMMAND

generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate  (\num -> NewRandom num)   (Random.int 1 100)

entriesUrl : String
entriesUrl =
    "http://localhost:3000/random-entries"


getEntries : Cmd Msg
getEntries =
--    Http.send NewEntries (Http.getString entriesUrl)
    entriesUrl
        |> Http.getString
        |> Http.send NewEntries

--VIEW

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

viewEntryItem : Entry -> Html.Html Msg
viewEntryItem entry =
    li [ classList [ ("marked" , entry.marked) ] ,onClick (Mark entry.id) ]
        [ span [ class "phrase" ] [text entry.phrase]
        , span [ class "points" ] [text (toString entry.points)]
        ]

totalPoints : List Entry -> Int
totalPoints entries =
    let
        squares x = x*x
    in
        entries
            |> List.filter .marked
            |> List.map .points
            |> List.foldl (+) 0 


viewScore : Int -> Html.Html msg
viewScore totalPoints =
    div [ class "score" ] 
        [
            span [ class "label" ] [ text "Score"]
        ,   span [ class "value" ] [ text (toString totalPoints) ]
        ]

viewEntries : List Entry -> Html.Html Msg
viewEntries entries =
     entries 
        |> List.map viewEntryItem
        |> ul []

view : Model -> Html.Html Msg
view model = 
    div [ class "content"]
        [ viewHeader "Buzzword Bingo"
        , viewPlayer model.name model.gameNumber
        , viewEntries model.entries
        , viewScore (totalPoints model.entries)
        , div [ class "button-group"]
              [ button [ onClick NewGame ] [text "NewGame"] ]
        , div [ class "button-group"]
              [ button [ onClick Sort ] [ text "Sort" ] ]
        , div [ class "debug"] [ text (toString model) ]
        , viewFooter 
        ]
        
-- main = 
--     update NewGame initialModel
--         |> view

main : Program Never Model Msg
main =
    Html.program
    {
        init = ( initialModel, getEntries )
    ,   view = view
    ,   update = update
    ,   subscriptions = (\_ -> Sub.none)
    }