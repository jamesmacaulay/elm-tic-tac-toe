module Board
    exposing
        ( Model
        , Symbol(..)
        , PlayState(..)
        , newGame
        , playState
        )

import Dict exposing (Dict)


type alias Model =
    Dict Int Symbol


type Symbol
    = X
    | O


type PlayState
    = CurrentPlayer Symbol
    | Winner Symbol
    | Draw


newGame : Model
newGame =
    Dict.empty


playState : Model -> PlayState
playState model =
    case boardWinner model of
        Just symbol ->
            Winner symbol

        Nothing ->
            if isFullBoard model then
                Draw
            else
                CurrentPlayer
                    (if moveCountIsEven model then
                        X
                     else
                        O
                    )


isFullBoard : Model -> Bool
isFullBoard model =
    Dict.size model == 9


moveCountIsEven : Model -> Bool
moveCountIsEven model =
    Dict.size model % 2 == 0


boardWinner : Model -> Maybe Symbol
boardWinner model =
    winningLines
        |> List.filterMap (lineWinner model)
        |> List.head


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
    case List.filterMap (\index -> Dict.get index model) line of
        [ X, X, X ] ->
            Just X

        [ O, O, O ] ->
            Just O

        _ ->
            Nothing
