{- Implementation based on code snippets from https://www.redblobgames.com/grids/hexagons/ -}


module Hexagon exposing (Hexagon, toSVG)

import Board exposing (Cell)
import Html exposing (Html)
import List
import Model exposing (Msg(..))
import Svg exposing (image, polygon, svg, text_)
import Svg.Attributes exposing (cursor, dominantBaseline, fill, fontFamily, fontSize, fontVariant, height, opacity, points, stroke, textAnchor, viewBox, width, x, xlinkHref, y)
import Svg.Events exposing (onClick)


type alias Hexagon =
    { center : ( Float, Float )
    , radius : Float
    , width : Float
    , height : Float
    }


calculateCorners : Hexagon -> List ( Float, Float )
calculateCorners hexagon =
    let
        radius =
            hexagon.radius

        ( x, y ) =
            ( hexagon.radius, hexagon.radius )

        calculatePoint corner =
            let
                angle_deg =
                    60 * corner - 30

                angle_rad =
                    pi / 180 * angle_deg
            in
            ( x + radius * cos angle_rad, y + radius * sin angle_rad )
    in
    List.map calculatePoint (List.range 0 5 |> List.map toFloat)


createHexagon : Int -> Int -> Float -> Hexagon
createHexagon cubeX cubeZ size =
    let
        column =
            cubeZ

        row =
            cubeX + ((cubeZ - modBy 2 cubeZ) // 2)

        width =
            sqrt 3 * size

        height =
            size * 2

        offsetX =
            toFloat (modBy 2 column) * (width / 2)

        -- Every even row is offset by half a hexagon
        x =
            (width * toFloat row) + offsetX

        y =
            size * 1.5 * toFloat column
    in
    Hexagon ( x, y ) size width height


toSVG : (Cell -> Msg) -> (Cell -> Bool) -> Cell -> Html Msg
toSVG clickAction isClickable cell =
    let
        size =
            70

        hexagon =
            createHexagon cell.position.x cell.position.z size

        fillColor =
            cell.occupant.color

        {- For some reason, the points are offset by 9.xx -}
        viewbox =
            String.fromFloat (70 - hexagon.width / 2) ++ " 0 " ++ String.fromFloat hexagon.width ++ " " ++ String.fromFloat hexagon.height

        clickAttrs =
            if isClickable cell then
                [ cursor "pointer", onClick (clickAction cell) ]

            else
                []
    in
    svg (clickAttrs ++ [ viewBox viewbox, width (String.fromFloat hexagon.width), height (String.fromFloat hexagon.height), x (String.fromFloat (Tuple.first hexagon.center)), y (String.fromFloat (Tuple.second hexagon.center)) ])
        [ polygon [ fill fillColor, stroke "black", points (String.join " " (calculateCorners hexagon |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y))) ] []
        , image [ xlinkHref ("./assets/countries/" ++ cell.country), opacity "33%", width (String.fromInt size), height (String.fromInt size), x (String.fromFloat (size / 2)), y (String.fromFloat (size / 2)) ] []
        , text_ [ x "43%", y "55%", dominantBaseline "middle", fontSize "70px", fontVariant "bold" ] [ Svg.text (String.fromInt cell.armyStrength) ]
        , text_ [ x "50%", y "20%", dominantBaseline "middle" ] [ Svg.text (String.fromInt cell.position.x ++ "," ++ String.fromInt cell.position.y ++ "," ++ String.fromInt cell.position.z) ]
        ]
