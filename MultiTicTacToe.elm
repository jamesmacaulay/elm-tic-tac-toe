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
    Dict Int Board


type Symbol
    = X
    | O


type PlayState
    = CurrentPlayer Symbol
    | Winner Symbol
    | Draw


noBoards : Model
noBoards =
    Dict.empty


nextIndex : Model -> Int
nextIndex model =
    Dict.keys model
        |> List.foldr Basics.max -1
        |> (+) 1


addEmptyBoard : Model -> Model
addEmptyBoard model =
    Dict.insert (nextIndex model) Board.empty model


newGame : Model
newGame =
    addEmptyBoard Dict.empty



-- UPDATE


type Msg
    = Mark Int Int
    | ResetBoard Int
    | AddBoard
    | RemoveBoard Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mark boardIndex spaceIndex ->
            Dict.update boardIndex (Maybe.map (Board.mark spaceIndex)) model

        ResetBoard boardIndex ->
            Dict.insert boardIndex Board.empty model

        AddBoard ->
            addEmptyBoard model

        RemoveBoard boardIndex ->
            Dict.remove boardIndex model



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "0 20px" )
            , ( "font-family", "Helvetica" )
            ]
        ]
        ([ h1 [] [ text "Tic-Tac-Toe" ]
         , button [ onClick AddBoard ] [ text "Add Board" ]
         ]
            ++ (model |> Dict.toList |> List.map viewBoard)
        )


viewBoard : ( Int, Board ) -> Html Msg
viewBoard ( boardIndex, board ) =
    board
        |> Board.view
            { mark = Mark boardIndex
            , reset = ResetBoard boardIndex
            , remove = RemoveBoard boardIndex
            }
