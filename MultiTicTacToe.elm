module MultiTicTacToe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Board exposing (Board)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = newGame
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Board


type Symbol
    = X
    | O


type PlayState
    = CurrentPlayer Symbol
    | Winner Symbol
    | Draw


newGame : Model
newGame =
    Board.empty



-- UPDATE


type Msg
    = Mark Int
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mark i ->
            Board.mark i model

        Reset ->
            newGame



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "0 20px" )
            , ( "font-family", "Helvetica" )
            ]
        ]
        [ h1 [] [ text "Tic-Tac-Toe" ]
        , viewPlayState (Board.playState model)
        , viewBoard model
        , button [ onClick Reset ] [ text "Reset" ]
        ]


viewPlayState : Board.PlayState -> Html Msg
viewPlayState playState =
    let
        message =
            case playState of
                Board.CurrentPlayer symbol ->
                    "Current player: " ++ toString symbol

                Board.Winner symbol ->
                    toString symbol ++ " wins!"

                Board.Draw ->
                    "Draw"
    in
        p [] [ text message ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        tableRows =
            model
                |> Board.toTable
                |> List.map viewRow
    in
        table [] tableRows


viewRow : List ( Int, Maybe Board.Symbol ) -> Html Msg
viewRow indexedSpaces =
    let
        tableCells =
            List.map viewSpace indexedSpaces
    in
        tr [] tableCells


viewSpace : ( Int, Maybe Board.Symbol ) -> Html Msg
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



-- UTIL
