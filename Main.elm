module Main exposing (..)

import List exposing (range, map, concatMap, append, filter, foldr)
import Dict exposing (fromList, get, insert, remove, values)
import Random exposing (generate)
import Html exposing (Html, div, text, h3, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Board exposing (..)
import Styles


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { board : Board
    , status : GameStatus
    }


type GameStatus
    = Playing
    | Finished


init : ( Model, Cmd Msg )
init =
    ( { board = emptyBoard
      , status = Playing
      }
    , generate BoardGenerated randomBoard
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = BoardGenerated Board
    | TileClicked Tile Coord
    | Replay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardGenerated board ->
            if isSolvable board && not (isFinished board) then
                ( { board = board, status = Playing }, Cmd.none )
            else
                init

        Replay ->
            init

        TileClicked tile coords ->
            case findAdjacentHole model.board coords of
                Nothing ->
                    ( model, Cmd.none )

                Just holeCoords ->
                    let
                        updatedBoard =
                            model.board |> moveTile tile coords holeCoords
                    in
                        ( { board = updatedBoard, status = verify updatedBoard }, Cmd.none )


verify : Board -> GameStatus
verify board =
    if isFinished board then
        Finished
    else
        Playing



-- VIEW


view : Model -> Html Msg
view model =
    div [ style (Styles.container size) ]
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
