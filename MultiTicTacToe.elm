module MultiTicTacToe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Board exposing (Board)
import Dict exposing (Dict)
import Tuple


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = singleEmptyBoard
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Dict Int Board


nextBoardIndex : Model -> Int
nextBoardIndex model =
    List.foldr Basics.max -1 (Dict.keys model) + 1


addEmptyBoard : Model -> Model
addEmptyBoard model =
    Dict.insert (nextBoardIndex model) Board.empty model


singleEmptyBoard : Model
singleEmptyBoard =
    addEmptyBoard Dict.empty


toOrderedList : Model -> List ( Int, Board )
toOrderedList model =
    model
        |> Dict.toList
        |> List.sortBy Tuple.first



-- UPDATE


type Msg
    = Mark Int Int
    | Reset Int
    | AddBoard
    | RemoveBoard Int


update : Msg -> Model -> Model
update msg boards =
    case msg of
        Mark boardIndex spaceIndex ->
            Dict.update boardIndex (Maybe.map (Board.mark spaceIndex)) boards

        Reset boardIndex ->
            Dict.update boardIndex (Maybe.map (always Board.empty)) boards

        AddBoard ->
            addEmptyBoard boards

        RemoveBoard boardIndex ->
            Dict.remove boardIndex boards



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
            ++ (toOrderedList boards |> List.map viewBoard)
        )


viewBoard : ( Int, Board ) -> Html Msg
viewBoard ( boardIndex, board ) =
    board
        |> Board.view
            { mark = Mark boardIndex
            , reset = Reset boardIndex
            , remove = RemoveBoard boardIndex
            }
