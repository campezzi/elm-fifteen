module Styles exposing (..)

import List exposing (append)


type alias Styles =
    List ( String, String )


board : Styles
board =
    [ ( "width", "60vh" )
    , ( "margin", "30px auto" )
    ]


tile : Styles
tile =
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


hole : Styles
hole =
    append
        tile
        [ ( "background-color", "#CECECE" )
        , ( "cursor", "not-allowed" )
        ]


lineBreak : Styles
lineBreak =
    [ ( "clear", "both" ) ]
