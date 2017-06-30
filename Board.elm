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
        , isSolvable
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


isSolvable : Board -> Bool
isSolvable board =
    let
        countInversions tiles acc =
            case tiles of
                [] ->
                    acc

                _ :: [] ->
                    acc

                a :: b :: rest ->
                    acc |> incrementIfInversion a b |> countInversions (b :: rest)

        incrementIfInversion a b acc =
            if a > b then
                acc + 1
            else
                acc

        inversions =
            countInversions (values board) 0

        ( r, _ ) =
            findHole board

        holeRow =
            size - r + 1
    in
        if isOdd size then
            (isEven inversions)
        else
            ((isEven holeRow && isOdd inversions) || (isOdd holeRow && isEven inversions))


isEven : Int -> Bool
isEven number =
    number % 2 == 0


isOdd : Int -> Bool
isOdd =
    not << isEven


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


findHole : Board -> Coord
findHole board =
    let
        go coord acc =
            if get coord board == Nothing then
                coord
            else
                acc
    in
        coords |> foldr go ( 1, 1 )


moveTile : Tile -> Coord -> Coord -> Board -> Board
moveTile tile tileCoord holeCoord board =
    board |> insert holeCoord tile |> remove tileCoord
