module Board
    exposing
        ( Board
        , Row
        , Column
        , Coord
        , Tile
        , Direction(..)
        , emptyBoard
        , randomBoard
        , size
        , isSolvable
        , isFinished
        , findAdjacentHole
        , moveTile
        , slideTo
        )

import Dict exposing (Dict, fromList, insert, get, remove, values)
import List exposing (concatMap, filter, map, map2, range, foldr)
import Random exposing (Generator)
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


type Direction
    = Left
    | Up
    | Right
    | Down


size : Int
size =
    4


emptyBoard : Board
emptyBoard =
    fromList []


randomBoard : Generator Board
randomBoard =
    Random.map makeBoard (shuffle orderedTiles)


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


orderedTiles : List (Maybe Tile)
orderedTiles =
    [ Nothing ] ++ map Just (range 1 (size ^ 2 - 1))


coords : List Coord
coords =
    let
        rowCoords row =
            map (coord row) (range 1 size)

        coord row column =
            ( row, column )
    in
        concatMap rowCoords (range 1 size)


isSolvable : Board -> Bool
isSolvable board =
    let
        inversions =
            countInversions (values board) 0

        countInversions tiles acc =
            case tiles of
                [] ->
                    acc

                _ :: [] ->
                    acc

                a :: b :: rest ->
                    acc |> incIfInversion a b |> countInversions (b :: rest)

        incIfInversion a b acc =
            if a > b then
                acc + 1
            else
                acc

        ( r, _ ) =
            findHole board

        holeRow =
            size - r

        isEven number =
            number % 2 == 0

        isOdd =
            not << isEven
    in
        if isOdd size then
            (isEven inversions)
        else
            ((isOdd holeRow && isOdd inversions) || (isEven holeRow && isEven inversions))


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


shift : Coord -> Direction -> Coord
shift ( row, column ) direction =
    case direction of
        Left ->
            ( row, column - 1 )

        Up ->
            ( row - 1, column )

        Right ->
            ( row, column + 1 )

        Down ->
            ( row + 1, column )


adjacentTo : Coord -> List Coord
adjacentTo coord =
    map (shift coord) [ Left, Up, Right, Down ] |> filter withinBoard


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


slideTo : Direction -> Board -> Board
slideTo direction board =
    let
        holeCoord =
            board |> findHole

        tileCoord =
            shift holeCoord (opposite direction)

        possibleTile =
            board |> get tileCoord
    in
        case possibleTile of
            Just tile ->
                board |> moveTile tile tileCoord holeCoord

            Nothing ->
                board


opposite : Direction -> Direction
opposite direction =
    case direction of
        Left ->
            Right

        Up ->
            Down

        Right ->
            Left

        Down ->
            Up
