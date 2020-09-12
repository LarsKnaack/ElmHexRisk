module Utils.Search exposing (..)

import Array exposing (Array)
import Board exposing (Cell)
import Utils.CubeCoordinates exposing (CubeCoordinates, cubeToIndex)


neighbourDirections : List CubeCoordinates
neighbourDirections =
    [ CubeCoordinates 1 -1 0
    , CubeCoordinates 1 0 -1
    , CubeCoordinates 0 1 -1
    , CubeCoordinates -1 1 0
    , CubeCoordinates -1 0 1
    , CubeCoordinates 0 -1 1
    ]


isConnected : Cell -> Cell -> Array Cell -> Bool
isConnected start end cells =
    breadthFirst start end [] [] cells


breadthFirst : Cell -> Cell -> List Cell -> List Cell -> Array Cell -> Bool
breadthFirst start end visited toVisit cells =
    let
        neighbours =
            validNeighbours start cells
    in
    if List.member end neighbours then
        Debug.log "connected" True

    else if List.member start visited then
        case toVisit of
            [] ->
                Debug.log "connected" False

            nextVisit :: restToVisit ->
                breadthFirst nextVisit end visited restToVisit cells

    else
        case neighbours of
            [] ->
                Debug.log "connected" False

            neighbour :: rest ->
                breadthFirst neighbour end (start :: visited) (toVisit ++ rest) cells


validNeighbours : Cell -> Array Cell -> List Cell
validNeighbours start cells =
    let
        allNeighbours =
            List.map (\dir -> Array.get (cubeToIndex (CubeCoordinates (start.position.x + dir.x) (start.position.y + dir.y) (start.position.z + dir.z))) cells) neighbourDirections
    in
    List.foldl
        (\cell acc ->
            case cell of
                Nothing ->
                    acc

                Just c ->
                    if c.occupant == start.occupant then
                        c :: acc

                    else
                        acc
        )
        []
        allNeighbours
