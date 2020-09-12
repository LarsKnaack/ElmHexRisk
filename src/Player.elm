module Player exposing (Player, getPlayer, randomPlayer, toString, togglePlayer)

import Array exposing (Array)
import Random


type alias Player =
    { id : Int
    , color : String
    , armiesAvailable : Int
    }


initialPlayers : Array Player
initialPlayers =
    Array.fromList
        [ Player 0 "#A9E5BB" (15 - 9)
        , {- 62d083 -} Player 1 "#F7B32B" (15 - 9)
        , {- c58607 -} Player 2 "#FDF8C4" 0
        , Player 3 "#FDF8C4" 0
        , Player 4 "#FDF8C4" 0
        , Player 5 "#FDF8C4" 0
        ]


randomPlayer : Int -> Player
randomPlayer seed =
    let
        ( index, _ ) =
            Random.step (Random.int 0 1) (Random.initialSeed seed)
    in
    withDefault (Array.get index initialPlayers)


getPlayer : Int -> Player
getPlayer index =
    withDefault (Array.get index initialPlayers)


togglePlayer : Player -> Player
togglePlayer player =
    case player.id of
        0 ->
            withDefault (Array.get 1 initialPlayers)

        _ ->
            withDefault (Array.get 0 initialPlayers)


toString : Player -> String
toString player =
    "SPIELER " ++ String.fromInt player.id


withDefault : Maybe Player -> Player
withDefault mp =
    case mp of
        Nothing ->
            Player -1 "white" -1

        Just p ->
            p
