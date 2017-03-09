module MultiTicTacToe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Board exposing (Board)
import Array.Hamt as Array exposing (Array)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = singleEmptyBoard
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Array Board


singleEmptyBoard : Model
singleEmptyBoard =
    Array.fromList [ Board.empty ]



-- UPDATE


type Msg
    = Mark Int Int
    | Reset Int
    | AddBoard


update : Msg -> Model -> Model
update msg boards =
    case msg of
        Mark boardIndex spaceIndex ->
            case Array.get boardIndex boards of
                Just board ->
                    Array.set boardIndex (Board.mark spaceIndex board) boards

                Nothing ->
                    boards

        Reset boardIndex ->
            case Array.get boardIndex boards of
                Just board ->
                    Array.set boardIndex Board.empty boards

                Nothing ->
                    boards

        AddBoard ->
            Array.push Board.empty boards



-- VIEW


view : Model -> Html Msg
view boards =
    div
        [ style
            [ ( "margin", "0 20px" )
            , ( "font-family", "Helvetica" )
            ]
        ]
        ([ h1 [] [ text "Tic-Tac-Toe" ]
         , button [ onClick AddBoard ] [ text "Add Board" ]
         ]
            ++ (boards |> Array.indexedMap viewBoard |> Array.toList)
        )


viewBoard : Int -> Board -> Html Msg
viewBoard boardNumber =
    Board.view
        { mark = Mark boardNumber
        , reset = Reset boardNumber
        }
