module Main exposing (..)

import Array
import Browser
import Datatypes.Board as Board exposing (Board, Cell)
import Datatypes.Hexagon as Hexagon exposing (..)
import Datatypes.Model exposing (Model(..), Msg(..))
import Datatypes.Player as Player exposing (Player)
import Dict
import Game exposing (Game, Phase(..))
import Html exposing (Html, button, div, header, img, text)
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Svg exposing (svg)
import Svg.Attributes exposing (height, width)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , subscriptions =
            \_ ->
                Sub.none
        , view = view
        , update = update
        }


initialModel : ( Model, Cmd Msg )
initialModel =
    (Tutorial, Cmd.none)


intGenerator : Int -> Int -> Generator Int
intGenerator min max =
    Random.int min max


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedRandomSeed seed ->
            ( Playing (Game.initialize seed), Cmd.none )
        Play ->
            (GenerateSeed, Random.generate GeneratedRandomSeed (intGenerator Random.minInt Random.maxInt))
        Restart ->
            (Tutorial, Cmd.none)
        _ ->
            case model of
                Playing game ->
                    let
                        ( currentPlayer, phase ) =
                            game.state

                        nextPlayer =
                            Player.togglePlayer currentPlayer
                    in
                    case msg of

                        GeneratedRandomSeed _ ->
                            ( model, Cmd.none )

                        Click cell ->
                            ( Playing (game |> Game.registerClick cell), Cmd.none )

                        SetAttackingArmies attacker defender strength ->
                            ( Playing { game | state = ( nextPlayer, Defend attacker defender strength ) }
                            , if defender.occupant == nextPlayer then
                                Cmd.none

                              else
                                Random.generate RolledDice (Random.pair (Random.list strength (intGenerator 1 6)) (Random.list (min 2 defender.armyStrength) (intGenerator 1 6)))
                            )

                        CancelAttack ->
                            ( Playing { game | state = ( currentPlayer, SelectingAttacker ) }, Cmd.none )

                        RollAttack attackers defenders ->
                            ( model, Random.generate RolledDice (Random.pair (Random.list attackers (intGenerator 1 6)) (Random.list defenders (intGenerator 1 6))) )

                        RolledDice ( attackers, defenders ) ->
                            let
                                descending a b =
                                    case compare a b of
                                        LT ->
                                            GT

                                        EQ ->
                                            EQ

                                        GT ->
                                            LT
                            in
                            ( Playing (game |> Game.registerRolls (List.sortWith descending attackers) (List.sortWith descending defenders)), Cmd.none )

                        EndAttack ->
                            ( Playing { game | state = ( currentPlayer, TakeTroops ) }, Cmd.none )
                        CancelMovement ->
                            ( Playing { game | state = ( currentPlayer, TakeTroops ) }, Cmd.none )
                        EndTurn ->
                            ( Playing { game | state = ( nextPlayer, Reinforce (Board.getReinforcements nextPlayer game.board) ) }, Cmd.none )
                        _ -> (model, Cmd.none)
                _ -> (model, Cmd.none)


view : Model -> Html Msg
view model =
    case model of
        Tutorial ->
            div
                [ style "width" "100vw"
                , style "height" "100vh"
                , style "display" "flex"
                , style "justifyContent" "center"
                , style "alignItems" "center"
                , style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "backgroundColor" "rgba(0,0,0,0.6)"
                , style "color" "white"
                ]
                [ div [ style "flex" "1 1 auto", style "display" "flex", style "flexDirection" "row" ]
                                      [ Html.nav (sideBar (Player.getPlayer -1)) [ div [] [], div [] [div [] [] ], div [] [] ]
                                      , Html.main_ [ style "flex" "1 1 auto", style "overflow" "auto" ] [tutorialView]
                                      , Html.aside (sideBar (Player.getPlayer -1)) []
                                      ]]
        GenerateSeed ->
            div
                [ style "width" "100vw"
                , style "height" "100vh"
                , style "display" "flex"
                , style "justifyContent" "center"
                , style "alignItems" "center"
                , style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "backgroundColor" "rgba(0,0,0,0.6)"
                , style "color" "white"
                ]
                [ text "Loading" ]

        Playing game ->
            let
                ( currentPlayer, phase ) =
                    game.state
            in
            div [ style "display" "flex", style "flexDirection" "column", style "height" "100vh", style "fontFamily" "Calibri" ]
                [ Html.header [ style "flex" "0 0 50px", style "textAlign" "center", style "lineHeight" "50px", style "fontSize" "50px" ] [ text "ELM HEXRISK" ]
                , div [ style "flex" "1 1 auto", style "display" "flex", style "flexDirection" "row" ]
                    [ Html.nav (sideBar currentPlayer) [ div [] [], div [] [div [] [ text (Game.phaseToString phase) ] ], div [] [] ]
                    , Html.main_ [ style "flex" "1 1 auto", style "overflow" "auto" ] [ boardView game ]
                    , Html.aside (sideBar currentPlayer) [ stateView phase ]
                    ]
                , Html.footer [ style "flex" "0 0 50px", style "textAlign" "center", style "lineHeight" "50px" ] [{- <div>Icons erstellt von <a href="https://www.flaticon.com/de/autoren/google" title="Google">Google</a> from <a href="https://www.flaticon.com/de/" title="Flaticon">www.flaticon.com</a></div> -}]
                ]


sideBar : Player -> List (Html.Attribute msg)
sideBar player =
    [ style "flex" "0 0 200px"
    , style "overflow" "hidden"
    , style "borderRadius" "80px"
    , style "backgroundColor" player.color
    , style "display" "flex"
    , style "flexDirection" "column"
    , style "alignItems" "center"
    , style "justifyContent" "center"
    ]

tutorialView : Html Msg
tutorialView =
    div [ style "display" "flex", style "flexDirection" "column", style "margin" "0px 100px", style "fontFamily" "Calibri" ]
    [ div [style "fontSize" "13pt", style "fontWeight" "bold", style "margin" "15px 0"][text "Willkommen zum Spiel HEXRISK."]
    , div [style "fontSize" "13pt", style "fontWeight" "bold", style "margin" "15px 0"][text "ALLGEMEIN"]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "Hier wird dir erklärt, wie du das Spiel bedienst. Eine ausführliche Anleitung der Risiko-Regeln findest du ", Html.a[style "color" "white",href "./assets/Risiko_de_Luxe-Spielanleitung.pdf"][text "hier"]]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "Bitte beachte, dass es in dieser Version keine Gebietskarten gibt und die neutralen Armeen passiv sind."]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "In der Mitte des Fensters befindet sich das Spielbrett. Dort kannst du einzelne Hexagone anklicken. Um zu sehen welche Hexagone du anklicken kannst, achte auf deinen Mauscursor."]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "Links und rechts neben dem Spielfeld befinden sich farblich eingefärbte Flächen. Die Farbe dieser Flächen gibt die Farbe des aktuellen Spielers an."]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "Die linke Fläche zeigt dir, in welcher Spielphase du dich befindest. Es gibt 3 Phasen:"]
    , div [style "fontSize" "13pt", style "fontWeight" "bold", style "margin" "15px 0"][text "VERSTÄRKUNG"]
    , div [style "marginLeft" "20px"][text "In dieser Phase kannst du deine Armeen per Klick auf ein Hexagon verstärken. Die Anzahl der verfügbaren Armeen siehst du auf der rechten Seite."]
    , div [style "fontSize" "13pt", style "fontWeight" "bold", style "margin" "15px 0"][text "ANGRIFF"]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "In dieser Phase kannst du einen Angriff ausführen. Wähle dazu per Klick ein Hexagon aus, von welchem du angreifen willst und anschließend ein benachbartes Hexagon welches du angreifen willst."]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "Du kannst das angreifende Land ändern, indem du auf den 'Zurück'-Pfeil in der Fläche rechts klickst."]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "Wenn beide Länder ausgewählt sind, kannst du die Anzahl der Armeen auswählen mit denen du angreifen willst. Anschließend wählt der Gegenspieler die Anzahl der verteidigenden Armeen."]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "Greifst du ein neutrales Land an, wird automatisch die maximale Anzahl an veteidigenden Armeen ausgewählt."]
    , div [style "marginLeft" "20px"][text "Du kannst deine Angriffsphase beenden, indem du auf den 'Beenden'-Button rechts klickst"]
    , div [style "fontSize" "13pt", style "fontWeight" "bold", style "margin" "15px 0"][text "VERSCHIEBEN"]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "In dieser Phase kannst du deine Armeen zwischen miteinander verbundenen Hexagonen verschieben. Wähle dazu zuerst per Klick den Ursprung und anschließend das Ziel. Du kannst so viele Armeen verschieben, wie du möchtest."]
    , div [style "marginLeft" "20px", style "marginBottom" "10px"][text "Um deinen Zug zu beenden klickst du auf den 'Zug Beenden'-Button rechts."]
    , div [style "display" "flex", style "justifyContent" "center"][button [onClick Play][text "Spiel starten"]]
    ]


boardView : Game -> Html Msg
boardView game =
    let
        ( currentPlayer, phase ) =
            game.state

        board =
            game.board

        boardHeight =
            if modBy 2 board.width == 0 then
                ((toFloat board.width / 2) * 140) + ((toFloat board.width / 2) * 70) + 35

            else
                toFloat board.width * 140

        boardWidth =
            (toFloat board.height * sqrt 3 * 70) + 71

        isClickable cell =
            case phase of
                SelectingDefender attacker ->
                    cell.occupant /= attacker.occupant
                SelectingAttacker ->
                    cell.occupant == currentPlayer && cell.armyStrength > 1
                GameOver _ -> False
                _ ->
                    cell.occupant == currentPlayer
    in
    {--}

    div [ style "display" "flex", style "justifyContent" "center" ]
        [ svg [ height (String.fromFloat boardHeight), width (String.fromFloat boardWidth) ]
            (Array.foldl (\cell acc -> Hexagon.toSVG Click isClickable cell :: acc) [] board.cells)
        ]
--}


stateView : Phase -> Html Msg
stateView phase =
    let
        countryFlag fileName =
            img [ src ("./assets/countries/" ++ fileName), width "70px", height "70px" ] []
    in
    div [ style "display" "flex", style "flexDirection" "column", style "justifyContent" "center", style "alignItems" "center" ]
        (case phase of
            Preparation a ->
                List.map (\_ -> div [ style "display" "flex", style "width" "100px", style "height" "100px", style "backgroundSize" "contain", style "backgroundImage" "url(\"./assets/infantry_1.png\")" ] []) (List.range 0 (2 - a))

            Reinforce a ->
                [ div [ style "display" "flex", style "flexWrap" "wrap" ] (List.map (\_ -> div [ style "display" "flex", style "width" "100px", style "height" "100px", style "backgroundSize" "contain", style "backgroundImage" "url(\"./assets/infantry_1.png\")" ] []) (List.range 1 a)) ]

            SelectingAttacker ->
                [ text "ANGRIFF BEENDEN", imageButton EndAttack "./assets/EndAttack.png" ]

            SelectingDefender attacker ->
                [ countryFlag attacker.country, imageButton CancelAttack "./assets/cancel.png" ]

            Attack attacker defender ->
                div [ style "display" "flex" ] [ countryFlag attacker.country, img [ src "./assets/attack.png", width "50px" ] [], countryFlag defender.country ] :: List.map (\i -> imageButton (SetAttackingArmies attacker defender i) ("./assets/infantry_" ++ String.fromInt i ++ ".png")) (List.range 1 (min 3 (attacker.armyStrength - 1)))

            Defend attacker defender strength ->
                div [ style "display" "flex" ] [ countryFlag attacker.country, img [ src "./assets/attack.png", width "50px" ] [], countryFlag defender.country ] :: List.map (\i -> imageButton (RollAttack strength i) ("./assets/infantry_" ++ String.fromInt i ++ ".png")) (List.range 1 (min 2 defender.armyStrength))
            TakeTroops ->
                [ text "ZUG BEENDEN", imageButton EndTurn "./assets/EndTurn.png" ]
            MoveTroops start ->
                [ countryFlag start.country, imageButton CancelMovement "./assets/cancel.png" ]
            GameOver winningPlayer ->
                [ text ("SPIELER " ++ String.fromInt winningPlayer.id ++ " hat gewonnen")
                , button[ onClick Restart
                                , style "width" "162px"
                                , style "height" "100px"
                                , style "backgroundColor" "white"
                                , style "cursor" "pointer"
                                , style "clipPath" "polygon(30px 5px, 5px 30px, 5px 70px, 30px 95px, 132px 95px, 157px 70px, 157px 30px, 132px 5px)"
                                ][text "Neu Starten"]]
        )

gameEndView : Player -> Html Msg
gameEndView winningPlayer =
    div[][]


imageButton : Msg -> String -> Html Msg
imageButton clickAction path =
    Html.button
        [ onClick clickAction
        , style "width" "162px"
        , style "height" "100px"
        , style "color" "white"
        , style "backgroundSize" "contain"
        , style "backgroundImage" ("url(\"" ++ path ++ "\")")
        , style "backgroundPosition" "center"
        , style "backgroundRepeat" "no-repeat"
        , style "backgroundColor" "white"
        , style "cursor" "pointer"
        , style "clipPath" "polygon(30px 5px, 5px 30px, 5px 70px, 30px 95px, 132px 95px, 157px 70px, 157px 30px, 132px 5px)"
        ]
        []
