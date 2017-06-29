module Main exposing (..)

import List exposing (range, map, concatMap, append, filter, foldr)
import Dict exposing (Dict, fromList, get, insert, remove, values)
import Html exposing (Html, div, text, h3, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Styles


main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }


size : Int
size =
    4



-- MODEL


type alias Model =
    { board : Board
    , status : GameStatus
    }


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


type GameStatus
    = Playing
    | Finished


init : ( Model, Cmd Msg )
init =
    ( { board =
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
      , status = Playing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = TileClicked Tile Coord
    | Replay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Replay ->
            init

        TileClicked tile coords ->
            case findAdjacentHole model coords of
                Nothing ->
                    ( model, Cmd.none )

                Just holeCoords ->
                    let
                        updatedBoard =
                            model.board |> insert holeCoords tile |> remove coords
                    in
                        ( { board = updatedBoard, status = verify updatedBoard }, Cmd.none )


findAdjacentHole : Model -> Coord -> Maybe Coord
findAdjacentHole model ( row, column ) =
    let
        adjacentCoords =
            [ ( row, column + 1 )
            , ( row + 1, column )
            , ( row, column - 1 )
            , ( row - 1, column )
            ]

        withinBoard ( r, c ) =
            if (r > 0) && (r <= size) && (c > 0) && (c <= size) then
                True
            else
                False

        holeCoord coord acc =
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case get coord model.board of
                        Nothing ->
                            Just coord

                        _ ->
                            Nothing
    in
        adjacentCoords |> filter withinBoard |> foldr holeCoord Nothing


verify : Board -> GameStatus
verify board =
    if values board == (range 1 15) && get ( 4, 4 ) board == Nothing then
        Finished
    else
        Playing



-- VIEW


view : Model -> Html Msg
view model =
    div [ style Styles.container ]
        ([ (renderBoard model.board) ] ++ (renderFinishedScreen model.status))


renderBoard : Board -> Html Msg
renderBoard board =
    div [] (concatMap (renderRow board) (range 1 size))


renderRow : Board -> Row -> List (Html Msg)
renderRow board row =
    append
        (map (renderTile board row) (range 1 size))
        [ div [ style Styles.lineBreak ] [] ]


renderTile : Board -> Row -> Column -> Html Msg
renderTile board row column =
    case get ( row, column ) board of
        Nothing ->
            div [ style Styles.hole ] []

        Just tile ->
            div
                [ style Styles.tile
                , onClick (TileClicked tile ( row, column ))
                ]
                [ text (toString tile) ]


renderFinishedScreen : GameStatus -> List (Html Msg)
renderFinishedScreen status =
    case status of
        Playing ->
            []

        Finished ->
            [ div [ style Styles.victoryOverlay ]
                [ h3 [ style Styles.victoryTitle ] [ text "YOU WIN!" ]
                , button [ style Styles.replayButton, onClick Replay ] [ text "Play Again" ]
                ]
            ]
