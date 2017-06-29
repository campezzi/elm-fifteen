module Styles exposing (..)

import List exposing (append)


type alias Styles =
    List ( String, String )


container : Styles
container =
    [ ( "position", "relative " )
    , ( "width", "60vh" )
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


victoryOverlay : Styles
victoryOverlay =
    [ ( "position", "absolute" )
    , ( "z-index", "999" )
    , ( "background-color", "rgba(255,255,255,0.95)" )
    , ( "top", "0" )
    , ( "bottom", "0" )
    , ( "left", "0" )
    , ( "right", "0" )
    , ( "display", "flex" )
    , ( "flex-direction", "column" )
    , ( "justify-content", "center" )
    , ( "align-items", "center" )
    ]


victoryTitle : Styles
victoryTitle =
    [ ( "font-size", "10vh" )
    , ( "margin", "0" )
    ]


replayButton : Styles
replayButton =
    [ ( "font-size", "20px" )
    , ( "margin", "0" )
    , ( "padding", "10px" )
    ]
