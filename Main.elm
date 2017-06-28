module Main exposing (..)

import List exposing (range, map, concatMap, append)
import Dict exposing (Dict, fromList, get)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }


rows : Int
rows =
    2


columns : Int
columns =
    2



-- MODEL


type alias Model =
    Dict Coord Int


type alias Row =
    Int


type alias Column =
    Int


type alias Coord =
    ( Row, Column )


init : ( Model, Cmd Msg )
init =
    ( fromList
        [ ( ( 1, 1 ), 1 )
        , ( ( 1, 2 ), 3 )
        , ( ( 2, 2 ), 2 )
        ]
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = PieceClicked Coord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    init



-- VIEW


view : Model -> Html Msg
view model =
    div [] (renderBoard model)


renderBoard : Model -> List (Html Msg)
renderBoard model =
    [ div [ style boardStyles ] (concatMap (renderRow model) (range 1 rows)) ]


renderRow : Model -> Row -> List (Html Msg)
renderRow model row =
    append
        (map (renderTile model row) (range 1 columns))
        [ div [ style lineBreakStyles ] [] ]


renderTile : Model -> Row -> Column -> Html Msg
renderTile model row column =
    case get ( row, column ) model of
        Nothing ->
            div [ style holeStyles ] []

        Just number ->
            div [ style tileStyles ] [ text (toString number) ]



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
    , ( "width", "30vh" )
    , ( "height", "30vh" )
    , ( "box-sizing", "border-box" )
    , ( "line-height", "30vh" )
    , ( "text-align", "center" )
    , ( "font-size", "4em" )
    , ( "font-weight", "bold" )
    , ( "border", "1px solid black" )
    ]


holeStyles : Styles
holeStyles =
    ( "background-color", "#CECECE" ) :: tileStyles


lineBreakStyles : Styles
lineBreakStyles =
    [ ( "clear", "both" ) ]
