module Board
    exposing
        ( Board
        , Symbol(..)
        , PlayState(..)
        , empty
        , playState
        , mark
        , toTable
        )

import Dict exposing (Dict)


type Board
    = Board (Dict Int Symbol)


type Symbol
    = X
    | O


type PlayState
    = CurrentPlayer Symbol
    | Winner Symbol
    | Draw


empty : Board
empty =
    Board Dict.empty


playState : Board -> PlayState
playState board =
    case boardWinner board of
        Just symbol ->
            Winner symbol

        Nothing ->
            if isFullBoard board then
                Draw
            else
                CurrentPlayer
                    (if moveCountIsEven board then
                        X
                     else
                        O
                    )


isFullBoard : Board -> Bool
isFullBoard (Board marks) =
    Dict.size marks == 9


moveCountIsEven : Board -> Bool
moveCountIsEven (Board marks) =
    Dict.size marks % 2 == 0


boardWinner : Board -> Maybe Symbol
boardWinner board =
    winningLines
        |> List.filterMap (lineWinner board)
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


lineWinner : Board -> List Int -> Maybe Symbol
lineWinner (Board marks) line =
    case List.filterMap (\index -> Dict.get index marks) line of
        [ X, X, X ] ->
            Just X

        [ O, O, O ] ->
            Just O

        _ ->
            Nothing


mark : Int -> Board -> Board
mark i ((Board marks) as board) =
    case playState board of
        CurrentPlayer symbol ->
            if Dict.member i marks then
                board
            else
                Board (Dict.insert i symbol marks)

        _ ->
            board


toTable : Board -> List (List ( Int, Maybe Symbol ))
toTable (Board marks) =
    List.range 0 8
        |> List.map (\index -> ( index, Dict.get index marks ))
        |> splitBy 3


splitBy : Int -> List a -> List (List a)
splitBy n xs =
    if List.length xs <= n then
        [ xs ]
    else
        List.take n xs :: splitBy n (List.drop n xs)
