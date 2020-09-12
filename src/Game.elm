module Game exposing (..)

import Board as Board exposing (Board, Cell)
import Player exposing (Player)
import Utils.Search as SearchAlgorithm


type alias Game =
    { state : ( Player, Phase )
    , board : Board
    }


type Phase
    = Preparation Int
    | Reinforce Int
    | SelectingAttacker
    | SelectingDefender Cell
    | Attack Cell Cell
    | Defend Cell Cell Int
    | TakeTroops
    | MoveTroops Cell
    | GameOver Player


phaseToString : Phase -> String
phaseToString phase =
    case phase of
        Preparation _ ->
            "ARMEEN PLATZIEREN"

        Reinforce _ ->
            "VERSTÄRKUNG PLAZIEREN"

        SelectingAttacker ->
            "ANGREIFER WÄHLEN"

        SelectingDefender _ ->
            "VERTEIDIGER WÄHLEN"

        Attack _ _ ->
            ""

        Defend _ _ _ ->
            ""

        TakeTroops ->
            "ARMEEN VERSCHIEBEN"

        MoveTroops _ ->
            "ZIELORT WÄHLEN"

        GameOver winningPlayer ->
            "SPIELER " ++ String.fromInt winningPlayer.id ++ " hat gewonnen"


initialize : Int -> Game
initialize seed =
    Game ( Player.randomPlayer seed, Preparation 0 ) (Board.createRandomBoard seed 6 7)


registerClick : Cell -> Game -> Game
registerClick cell game =
    let
        ( currentPlayer, phase ) =
            game.state
    in
    case phase of
        Preparation armiesPlaced ->
            if Board.getNumberOfArmies currentPlayer game.board == 14 && Board.getNumberOfArmies (Player.togglePlayer currentPlayer) game.board == 15 then
                { game | board = game.board |> Board.reinforceArmy cell }
                    |> (\g -> { g | state = ( Player.togglePlayer currentPlayer, SelectingAttacker ) })

            else if armiesPlaced == 2 then
                { game | state = ( Player.togglePlayer currentPlayer, Preparation 0 ), board = game.board |> Board.reinforceArmy cell }

            else
                { game | state = ( currentPlayer, Preparation (armiesPlaced + 1) ), board = game.board |> Board.reinforceArmy cell }

        Reinforce available ->
            { game | board = game.board |> Board.reinforceArmy cell }
                |> (\g ->
                        { g
                            | state =
                                if available == 1 then
                                    ( currentPlayer, SelectingAttacker )

                                else
                                    ( currentPlayer, Reinforce (available - 1) )
                        }
                   )

        SelectingAttacker ->
            if cell.occupant == currentPlayer && cell.armyStrength > 1 then
                { game | state = ( currentPlayer, SelectingDefender cell ) }

            else
                game

        SelectingDefender attacker ->
            if Board.validateDefender attacker cell then
                { game | state = ( currentPlayer, Attack attacker cell ) }

            else
                game

        Attack _ _ ->
            game

        Defend _ _ _ ->
            game

        TakeTroops ->
            if cell.armyStrength > 1 then
                { game | state = ( currentPlayer, MoveTroops cell ) }

            else
                game

        MoveTroops start ->
            if SearchAlgorithm.isConnected start cell game.board.cells then
                { game | state = ( currentPlayer, TakeTroops ), board = game.board |> Board.reinforceArmy cell |> Board.removeArmy start }

            else
                game

        GameOver _ ->
            game


registerRolls : List Int -> List Int -> Game -> Game
registerRolls attackers defenders game =
    let
        ( currentPlayer, phase ) =
            game.state

        player =
            Player.togglePlayer currentPlayer

        checkForLiberation attacker defender =
            if defender.armyStrength == 0 then
                {- Set Land occupied -}
                { game | state = ( player, SelectingAttacker ), board = game.board |> Board.occupy defender player |> Board.removeArmy attacker }

            else
                { game | state = ( player, SelectingAttacker ) }

        checkForGameOver updatedGame =
            if Board.getNumberOfCountries currentPlayer updatedGame.board == 0 then
                { updatedGame | state = ( player, GameOver player ) }

            else if Board.getNumberOfCountries player updatedGame.board == 0 then
                { updatedGame | state = ( currentPlayer, GameOver currentPlayer ) }

            else
                updatedGame
    in
    case phase of
        Defend attacker defender unused ->
            case attackers of
                attackRoll :: restAttackers ->
                    case defenders of
                        defendRoll :: restDefenders ->
                            if attackRoll > defendRoll then
                                { game | state = ( currentPlayer, Defend attacker { defender | armyStrength = defender.armyStrength - 1 } unused ), board = game.board |> Board.removeArmy defender } |> registerRolls restAttackers restDefenders

                            else
                                { game | state = ( currentPlayer, Defend { attacker | armyStrength = attacker.armyStrength - 1 } defender unused ), board = game.board |> Board.removeArmy attacker } |> registerRolls restAttackers restDefenders

                        [] ->
                            checkForLiberation attacker defender |> checkForGameOver

                [] ->
                    checkForLiberation attacker defender |> checkForGameOver

        _ ->
            game



{--
checkForLiberation : Cell -> Cell -> Game -> Game
checkForLiberation attacker defender game =
    let
        updatedAttacker = Dict.get attacker.position game.board.cells
        updatedDefender = Dict.get defender.position game.board.cells
        nextGame = game
    in
        case updatedAttacker of
            Nothing -> game
            Just a ->
                case updatedDefender of
                    Nothing -> game
                    Just d ->
                        if d.armyStrength <= 0 then
                            {game | state = Phase.next a game.state, board = game.board |> Board.occupy defender (Tuple.first game.state)}
                        else
                            {game | state = Phase.next a game.state}
--}
