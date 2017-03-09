module MultiTicTacToe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Board exposing (Board)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = Board.empty
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Board



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
            Board.empty



-- VIEW


view : Model -> Html Msg
view board =
    div
        [ style
            [ ( "margin", "0 20px" )
            , ( "font-family", "Helvetica" )
            ]
        ]
        [ h1 [] [ text "Tic-Tac-Toe" ]
        , viewBoard board
        ]


viewBoard : Board -> Html Msg
viewBoard =
    Board.view
        { mark = Mark
        , reset = Reset
        }
