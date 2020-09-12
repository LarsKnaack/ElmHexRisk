module Datatypes.Model exposing (..)

import Datatypes.Board exposing (Cell)
import Game exposing (Game)


type Model
    = Tutorial
    | GenerateSeed
    | Playing Game


type Msg
    = Play
    | GeneratedRandomSeed Int
    | RolledDice ( List Int, List Int )
    | Click Cell
    | CancelAttack
    | CancelMovement
    | SetAttackingArmies Cell Cell Int
    | RollAttack Int Int
    | EndAttack
    | EndTurn
    | Restart
