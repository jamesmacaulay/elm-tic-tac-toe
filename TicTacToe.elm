module TicTacToe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = newGame
        , view = view
        , update = update
        }


type Symbol
    = X
    | O


type alias Model =
    Dict Int Symbol


type PlayState
    = CurrentPlayer Symbol
    | Winner Symbol


newGame : Model
newGame =
    Dict.empty


viewSpace : ( Int, Maybe Symbol ) -> Html Msg
viewSpace ( index, space ) =
    td
        [ style
            [ ( "background-color", "#eef" )
            , ( "text-align", "center" )
            , ( "font-size", "72px" )
            , ( "font-family", "Helvetica" )
            , ( "width", "100px" )
            , ( "height", "100px" )
            ]
        , onClick (Mark index)
        ]
        [ space
            |> Maybe.map toString
            |> Maybe.withDefault ""
            |> text
        ]


viewRow : List ( Int, Maybe Symbol ) -> Html Msg
viewRow indexedSpaces =
    let
        tableCells =
            List.map viewSpace indexedSpaces
    in
        tr [] tableCells


splitBy : Int -> List a -> List (List a)
splitBy n xs =
    if List.length xs <= n then
        [ xs ]
    else
        List.take n xs :: splitBy n (List.drop n xs)


indexRange : List Int
indexRange =
    List.range 0 8


viewBoard : Model -> Html Msg
viewBoard model =
    let
        tableRows =
            indexRange
                |> List.map (flip Dict.get model)
                |> List.map2 (,) indexRange
                |> splitBy 3
                |> List.map viewRow
    in
        table [] tableRows


viewPlayState : PlayState -> Html Msg
viewPlayState playState =
    let
        message =
            case playState of
                CurrentPlayer symbol ->
                    "Current player: " ++ toString symbol

                Winner symbol ->
                    toString symbol ++ " wins!"
    in
        p [] [ text message ]


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "0 20px" )
            , ( "font-family", "Helvetica" )
            ]
        ]
        [ h1 [] [ text "Tic-Tac-Toe" ]
        , viewPlayState (playState model)
        , viewBoard model
        , button [ onClick Reset ] [ text "Reset" ]
        ]


type Msg
    = Mark Int
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mark i ->
            mark i model

        Reset ->
            newGame


winningLines : List (List Int)
winningLines =
    [ [ 0, 1, 2 ]
    , [ 3, 4, 5 ]
    , [ 6, 7, 8 ]
    , [ 0, 3, 6 ]
    , [ 1, 4, 7 ]
    , [ 2, 5, 8 ]
    , [ 0, 4, 8 ]
    , [ 2, 4, 6 ]
    ]


lineWinner : Model -> List Int -> Maybe Symbol
lineWinner model line =
    case List.filterMap (flip Dict.get model) line of
        [ X, X, X ] ->
            Just X

        [ O, O, O ] ->
            Just O

        _ ->
            Nothing


boardWinner : Model -> Maybe Symbol
boardWinner model =
    winningLines
        |> List.filterMap (lineWinner model)
        |> List.head


playState : Model -> PlayState
playState model =
    let
        currentPlayer =
            if Dict.size model % 2 == 0 then
                X
            else
                O
    in
        boardWinner model
            |> Maybe.map Winner
            |> Maybe.withDefault (CurrentPlayer currentPlayer)


mark : Int -> Model -> Model
mark i model =
    case playState model of
        Winner _ ->
            model

        CurrentPlayer symbol ->
            if Dict.member i model then
                model
            else
                Dict.insert i symbol model
