{- Implementation based on code snippets from https://www.redblobgames.com/grids/hexagons/ -}


module Utils.CubeCoordinates exposing (..)


type alias CubeCoordinates =
    { x : Int
    , y : Int
    , z : Int
    }


axialToCube : ( Int, Int ) -> CubeCoordinates
axialToCube ( row, col ) =
    CubeCoordinates (col - ((row - modBy 2 row) // 2)) (-(col - ((row - modBy 2 row) // 2)) - row) row


cubeToIndex : CubeCoordinates -> Int
cubeToIndex coords =
    let
        row =
            coords.z

        col =
            coords.x + ((coords.z - modBy 2 coords.z) // 2)
    in
    row + (col * 6)
