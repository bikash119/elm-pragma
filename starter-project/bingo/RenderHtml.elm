module RenderHtml exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(onClick)
import List exposing (sortBy )
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode

--MODEL

type alias Score = 
    { id : Int
    , name : String
    , score : Int
    }

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
    , alertMessage: Maybe String
    }

initialModel : Model
initialModel = 
    {
        name = "mike"
    ,   gameNumber = 1
    ,   entries = []
    ,   alertMessage = Nothing
    }

--update

type Msg 
    = NewGame 
    | Mark Int 
    | Sort 
    | NewRandom Int 
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)

allEntriesMarked : List Entry -> Bool
allEntriesMarked entries =
    let
        marked e = e.marked
    in
        List.all marked entries


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        ShareScore ->
            (model, postScore model)
        NewScore ( Ok score) ->
            let
                message = 
                    "Your score of"
                        ++ (toString score.score)
                        ++ " was successfully shared"
            in
                ( { model | alertMessage = Just message} , Cmd.none)
        NewScore ( Err error) ->
            let
                message = 
                    "Error posting your "
                        ++ (toString error)
            in
                ( { model | alertMessage = Just message} , Cmd.none)
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
        
        NewEntries (Ok randomEntries) ->
                ({model | entries = randomEntries}, Cmd.none )
        NewEntries (Err error) ->
            let
                errorMessage = 
                    case error of
                        Http.NetworkError ->
                            "Is the server running"
                        Http.BadStatus response ->
                            (toString response.status)
                        Http.BadPayload message _->
                            "Decoding failed" ++ message
                        _ ->
                            (toString error)
            in
                ( {model | alertMessage = Just errorMessage }, Cmd.none)
        closeAlert ->
            ( { model | alertMessage = Nothing}, Cmd.none )

--DECODERS / ENCODERS

entryDecoder : Decoder Entry
entryDecoder = 
    Decode.map4 Entry 
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)
scoreDecoder = 
    Decode.map3 Score 
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)

encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [   ("name", Encode.string model.name)
        ,   ("score" , Encode.int (totalPoints model.entries))
        ]

--COMMAND

generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate  (\num -> NewRandom num)   (Random.int 1 100)

entriesUrl : String
entriesUrl =
    "http://localhost:3000/random-entries"


postScore : Model -> Cmd Msg
postScore model =
    let
        url = "http://localhost:3000/scores"

        body = 
            encodeScore model
                |> Http.jsonBody
 
        request =
            Http.post url body scoreDecoder
    in
        Http.send NewScore request

getEntries : Cmd Msg
getEntries =
--    Http.send NewEntries (Http.getString entriesUrl)
    (Decode.list entryDecoder)
        |> Http.get entriesUrl
        |> Http.send NewEntries

hasZeroScore : Model -> Bool
hasZeroScore model = 
    -- case totalPoints model.entries of
    --     0 ->
    --         True
    --     _ ->
    --         False
    totalPoints model.entries == 0

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
        , viewAlertMessage model.alertMessage
        , viewEntries model.entries
        , viewScore (totalPoints model.entries)
        , div [ class "button-group"]
              [ 
                  button [ onClick NewGame ] [text "NewGame"]
                , button [ onClick Sort ] [ text "Sort" ]
                , button [ onClick ShareScore, disabled (hasZeroScore model) ] [ text "Share Score"]
               ]
        , div [ class "debug"] [ text (toString model) ]
        , viewFooter 
        ]
viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage = 
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [   span [ class "close", onClick CloseAlert ] [ text "X"]
                ,   text message 
                ]
        Nothing ->
            text ""
        
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