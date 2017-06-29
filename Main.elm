module Main exposing (..)

import List exposing (range, map, concatMap, append, filter, foldr)
import Dict exposing (Dict, fromList, get, insert, remove)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
verify model =
    Playing



-- VIEW


view : Model -> Html Msg
view model =
    div [] (renderBoard model)


renderBoard : Model -> List (Html Msg)
renderBoard model =
    [ div [ style boardStyles ] (concatMap (renderRow model) (range 1 size)) ]


renderRow : Model -> Row -> List (Html Msg)
renderRow model row =
    append
        (map (renderTile model row) (range 1 size))
        [ div [ style lineBreakStyles ] [] ]


renderTile : Model -> Row -> Column -> Html Msg
renderTile model row column =
    case get ( row, column ) model.board of
        Nothing ->
            div [ style holeStyles ] []

        Just tile ->
            div
                [ style tileStyles
                , onClick (TileClicked tile ( row, column ))
                ]
                [ text (toString tile) ]



-- STYLES


type alias Styles =
    List ( String, String )


boardStyles : Styles
boardStyles =
    [ ( "width", "60vh" )
    , ( "margin", "30px auto" )
    ]


tileStyles : Styles
tileStyles =
    [ ( "float", "left" )
    , ( "width", "15vh" )
    , ( "height", "15vh" )
    , ( "box-sizing", "border-box" )
    , ( "cursor", "pointer" )
    , ( "line-height", "15vh" )
    , ( "text-align", "center" )
    , ( "font-size", "7vh" )
    , ( "font-weight", "bold" )
    , ( "border", "1px solid black" )
    ]


holeStyles : Styles
holeStyles =
    append
        tileStyles
        [ ( "background-color", "#CECECE" )
        , ( "cursor", "not-allowed" )
        ]


lineBreakStyles : Styles
lineBreakStyles =
    [ ( "clear", "both" ) ]
