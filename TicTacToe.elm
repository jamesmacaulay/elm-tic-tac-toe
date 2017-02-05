module TicTacToe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)


main =
    Html.beginnerProgram
        { model = newGame
        , view = view
        , update = update
        }


type Symbol
    = X
    | O


alternate : Symbol -> Symbol
alternate symbol =
    case symbol of
        X ->
            O

        O ->
            X


type alias Board =
    Array (Maybe Symbol)


type PlayState
    = CurrentPlayer Symbol
    | Winner Symbol


type alias Model =
    { board : Board
    , playState : PlayState
    }


newGame : Model
newGame =
    { board = Array.repeat 9 Nothing
    , playState = CurrentPlayer X
    }


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


viewBoard : Board -> Html Msg
viewBoard board =
    let
        indexedSpaces : List ( Int, Maybe Symbol )
        indexedSpaces =
            Array.toIndexedList board

        preparedBoard : List (List ( Int, Maybe Symbol ))
        preparedBoard =
            splitBy 3 indexedSpaces

        tableRows =
            List.map viewRow preparedBoard
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
        , viewPlayState model.playState
        , viewBoard model.board
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


lineWinner : Board -> List Int -> Maybe Symbol
lineWinner board line =
    case List.filterMap (flip Array.get board) line of
        [ Just X, Just X, Just X ] ->
            Just X

        [ Just O, Just O, Just O ] ->
            Just O

        _ ->
            Nothing


boardWinner : Board -> Maybe Symbol
boardWinner board =
    winningLines
        |> List.filterMap (lineWinner board)
        |> List.head


mark : Int -> Model -> Model
mark i ({ board, playState } as model) =
    case playState of
        Winner _ ->
            model

        CurrentPlayer symbol ->
            case Array.get i board of
                Just Nothing ->
                    let
                        board2 =
                            Array.set i (Just symbol) board

                        playState2 =
                            case boardWinner board2 of
                                Just winner ->
                                    Winner winner

                                Nothing ->
                                    CurrentPlayer (alternate symbol)
                    in
                        { model
                            | board = board2
                            , playState = playState2
                        }

                _ ->
                    model
