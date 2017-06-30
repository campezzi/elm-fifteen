module Board
    exposing
        ( Board
        , Row
        , Column
        , Coord
        , Tile
        , emptyBoard
        , randomBoard
        , size
        , isFinished
        , findAdjacentHole
        , moveTile
        )

import Dict exposing (Dict, fromList, insert, get, remove, values)
import List exposing (concatMap, filter, map, map2, range, foldr)
import Random exposing (Generator, andThen)
import Random.List exposing (shuffle)


type alias Board =
    Dict Coord Tile


type alias Row =
    Int


type alias Column =
    Int


type alias Coord =
    ( Row, Column )


type alias Tile =
    Int


size : Int
size =
    4


coords : List Coord
coords =
    let
        rowCoords row =
            map (coord row) (range 1 size)

        coord row column =
            ( row, column )
    in
        concatMap rowCoords (range 1 size)


sequentialTiles : List (Maybe Tile)
sequentialTiles =
    [ Nothing ] ++ map Just (range 1 15)


emptyBoard : Board
emptyBoard =
    fromList []


makeBoard : List (Maybe Tile) -> Board
makeBoard tiles =
    let
        fillBoard ( coord, tile ) board =
            case tile of
                Nothing ->
                    board

                Just x ->
                    insert coord x board
    in
        map2 (,) coords tiles |> foldr fillBoard (fromList [])


randomBoard : Generator Board
randomBoard =
    Random.map makeBoard (shuffle sequentialTiles)


solvableBoard : Board
solvableBoard =
    fromList
        [ ( ( 1, 1 ), 13 )
        , ( ( 1, 2 ), 2 )
        , ( ( 1, 3 ), 10 )
        , ( ( 1, 4 ), 3 )
        , ( ( 2, 1 ), 1 )
        , ( ( 2, 2 ), 12 )
        , ( ( 2, 3 ), 8 )
        , ( ( 2, 4 ), 4 )
        , ( ( 3, 1 ), 5 )
        , ( ( 3, 3 ), 9 )
        , ( ( 3, 4 ), 6 )
        , ( ( 4, 1 ), 15 )
        , ( ( 4, 2 ), 14 )
        , ( ( 4, 3 ), 11 )
        , ( ( 4, 4 ), 7 )
        ]


almostSolvedBoard : Board
almostSolvedBoard =
    fromList
        [ ( ( 1, 1 ), 1 )
        , ( ( 1, 2 ), 2 )
        , ( ( 1, 3 ), 3 )
        , ( ( 1, 4 ), 4 )
        , ( ( 2, 1 ), 5 )
        , ( ( 2, 2 ), 6 )
        , ( ( 2, 3 ), 7 )
        , ( ( 2, 4 ), 8 )
        , ( ( 3, 1 ), 9 )
        , ( ( 3, 2 ), 10 )
        , ( ( 3, 3 ), 11 )
        , ( ( 3, 4 ), 12 )
        , ( ( 4, 1 ), 13 )
        , ( ( 4, 2 ), 14 )
        , ( ( 4, 4 ), 15 )
        ]


isFinished : Board -> Bool
isFinished board =
    let
        totalTiles =
            size ^ 2 - 1

        bottomRightCoord =
            ( size, size )
    in
        if values board == (range 1 totalTiles) && get bottomRightCoord board == Nothing then
            True
        else
            False


adjacentTo : Coord -> List Coord
adjacentTo ( row, column ) =
    filter withinBoard
        [ ( row, column + 1 )
        , ( row + 1, column )
        , ( row, column - 1 )
        , ( row - 1, column )
        ]


withinBoard : Coord -> Bool
withinBoard ( row, column ) =
    if (row > 0) && (row <= size) && (column > 0) && (column <= size) then
        True
    else
        False


findAdjacentHole : Board -> Coord -> Maybe Coord
findAdjacentHole board ( row, column ) =
    let
        findHoleCoord coord acc =
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case get coord board of
                        Nothing ->
                            Just coord

                        _ ->
                            Nothing
    in
        adjacentTo ( row, column ) |> foldr findHoleCoord Nothing


moveTile : Tile -> Coord -> Coord -> Board -> Board
moveTile tile tileCoord holeCoord board =
    board |> insert holeCoord tile |> remove tileCoord
