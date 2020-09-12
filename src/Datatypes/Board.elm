module Datatypes.Board exposing (Board, Cell, createRandomBoard, getNumberOfArmies, getNumberOfCountries, getReinforcements, occupy, reinforceArmy, removeArmy, validateDefender)

import Array exposing (Array)
import Datatypes.CubeCoordinates exposing (CubeCoordinates, axialToCube, cubeToIndex)
import Datatypes.Helpers as Helpers
import Datatypes.Player as Player exposing (Player)
import Random


type alias Board =
    { height : Int
    , width : Int
    , cells : Array Cell
    }


type alias Cell =
    { position : CubeCoordinates
    , occupant : Player
    , armyStrength : Int
    , country : String
    }


createRandomBoard : Int -> Int -> Int -> Board
createRandomBoard seed columns rows =
    let
        indices =
            List.range 0 (rows - 1)
                |> List.concatMap (\row -> List.range 0 (columns - 1) |> List.map (\col -> ( col, row )))

        occupants =
            List.repeat 9 0 ++ List.repeat 9 1 ++ List.repeat 6 2 ++ List.repeat 6 3 ++ List.repeat 6 4 ++ List.repeat 6 5

        shuffled =
            Array.toList (Helpers.shuffleArray (Random.initialSeed seed) (Array.fromList occupants))

        shuffledCountries =
            Helpers.shuffleArray (Random.initialSeed seed) (Array.fromList countryFlags)

        mapOccupant axialPos occupant =
            Cell (axialToCube axialPos)
                (Player.getPlayer occupant)
                (if occupant > 1 then
                    4

                 else
                    1
                )
                (Maybe.withDefault "germany.png" (Array.get (cubeToIndex (axialToCube axialPos)) shuffledCountries))
    in
    Board rows columns (Array.fromList (List.map2 mapOccupant indices shuffled))


getNumberOfArmies : Player -> Board -> Int
getNumberOfArmies player board =
    Array.foldl
        (\cell acc ->
            if cell.occupant == player then
                acc + cell.armyStrength

            else
                acc
        )
        0
        board.cells


getReinforcements : Player -> Board -> Int
getReinforcements player board =
    max 3
        (Array.foldl
            (\cell acc ->
                if cell.occupant == player then
                    (acc + 1) // 3

                else
                    acc
            )
            0
            board.cells
        )


getNumberOfCountries : Player -> Board -> Int
getNumberOfCountries player board =
    Array.foldl
        (\cell acc ->
            if cell.occupant == player then
                acc + 1

            else
                acc
        )
        0
        board.cells


reinforceArmy : Cell -> Board -> Board
reinforceArmy cell board =
    { board
        | cells = Array.set (cubeToIndex cell.position) { cell | armyStrength = cell.armyStrength + 1 } board.cells
    }


removeArmy : Cell -> Board -> Board
removeArmy cell board =
    { board
        | cells = Array.set (cubeToIndex cell.position) { cell | armyStrength = cell.armyStrength - 1 } board.cells
    }


validateDefender : Cell -> Cell -> Bool
validateDefender attacker defender =
    let
        directions =
            [ CubeCoordinates 1 -1 0
            , CubeCoordinates 1 0 -1
            , CubeCoordinates 0 1 -1
            , CubeCoordinates -1 1 0
            , CubeCoordinates -1 0 1
            , CubeCoordinates 0 -1 1
            ]

        diff =
            CubeCoordinates (attacker.position.x - defender.position.x) (attacker.position.y - defender.position.y) (attacker.position.z - defender.position.z)
    in
    List.member diff directions && attacker.occupant /= defender.occupant


occupy : Cell -> Player -> Board -> Board
occupy cell player board =
    { board
        | cells = Array.set (cubeToIndex cell.position) { cell | armyStrength = 1, occupant = player } board.cells
    }


countryFlags : List String
countryFlags =
    [ "abkhazia.png"
    , "afghanistan.png"
    , "aland-islands.png"
    , "albania.png"
    , "algeria.png"
    , "american-samoa.png"
    , "andorra.png"
    , "angola.png"
    , "anguilla.png"
    , "antigua-and-barbuda.png"
    , "argentina.png"
    , "armenia.png"
    , "aruba.png"
    , "australia.png"
    , "austria.png"
    , "azerbaijan.png"
    , "azores-islands.png"
    , "bahamas.png"
    , "bahrain.png"
    , "balearic-islands.png"
    , "bangladesh.png"
    , "barbados.png"
    , "basque-country.png"
    , "belarus.png"
    , "belgium.png"
    , "belize.png"
    , "benin.png"
    , "bermuda.png"
    , "bhutan.png"
    , "bhutan-1.png"
    , "bolivia.png"
    , "bonaire.png"
    , "bosnia-and-herzegovina.png"
    , "botswana.png"
    , "brazil.png"
    , "british-columbia.png"
    , "british-indian-ocean-territory.png"
    , "british-virgin-islands.png"
    , "brunei.png"
    , "bulgaria.png"
    , "burkina-faso.png"
    , "burundi.png"
    , "cambodia.png"
    , "cameroon.png"
    , "canada.png"
    , "canary-islands.png"
    , "cape-verde.png"
    , "cayman-islands.png"
    , "central-african-republic.png"
    , "ceuta.png"
    , "chad.png"
    , "chile.png"
    , "china.png"
    , "christmas-island.png"
    , "cocos-island.png"
    , "colombia.png"
    , "comoros.png"
    , "cook-islands.png"
    , "corsica.png"
    , "costa-rica.png"
    , "croatia.png"
    , "cuba.png"
    , "curacao.png"
    , "cyprus.png"
    , "czech-republic.png"
    , "democratic-republic-of-congo.png"
    , "denmark.png"
    , "djibouti.png"
    , "dominica.png"
    , "dominican-republic.png"
    , "east-timor.png"
    , "ecuador.png"
    , "egypt.png"
    , "england.png"
    , "equatorial-guinea.png"
    , "eritrea.png"
    , "estonia.png"
    , "ethiopia.png"
    , "european-union.png"
    , "falkland-islands.png"
    , "faroe-islands.png"
    , "fiji.png"
    , "finland.png"
    , "france.png"
    , "french-polynesia.png"
    , "gabon.png"
    , "galapagos-islands.png"
    , "gambia.png"
    , "georgia.png"
    , "germany.png"
    , "ghana.png"
    , "gibraltar.png"
    , "greece.png"
    , "greenland.png"
    , "grenada.png"
    , "guam.png"
    , "guatemala.png"
    , "guernsey.png"
    , "guinea.png"
    , "guinea-bissau.png"
    , "guyana.png"
    , "haiti.png"
    , "hawaii.png"
    , "honduras.png"
    , "hong-kong.png"
    , "hungary.png"
    , "iceland.png"
    , "india.png"
    , "indonesia.png"
    , "iran.png"
    , "iraq.png"
    , "ireland.png"
    , "isle-of-man.png"
    , "israel.png"
    , "italy.png"
    , "ivory-coast.png"
    , "jamaica.png"
    , "japan.png"
    , "jersey.png"
    , "jordan.png"
    , "kazakhstan.png"
    , "kenya.png"
    , "kiribati.png"
    , "kosovo.png"
    , "kuwait.png"
    , "kyrgyzstan.png"
    , "laos.png"
    , "latvia.png"
    , "lebanon.png"
    , "lesotho.png"
    , "liberia.png"
    , "libya.png"
    , "liechtenstein.png"
    , "lithuania.png"
    , "luxembourg.png"
    , "macao.png"
    , "madagascar.png"
    , "madeira.png"
    , "malawi.png"
    , "malaysia.png"
    , "maldives.png"
    , "mali.png"
    , "malta.png"
    , "marshall-island.png"
    , "martinique.png"
    , "mauritania.png"
    , "mauritius.png"
    , "melilla.png"
    , "mexico.png"
    , "micronesia.png"
    , "moldova.png"
    , "monaco.png"
    , "mongolia.png"
    , "montenegro.png"
    , "montserrat.png"
    , "morocco.png"
    , "mozambique.png"
    , "myanmar.png"
    , "namibia.png"
    , "nato.png"
    , "nauru.png"
    , "nepal.png"
    , "netherlands.png"
    , "new-zealand.png"
    , "nicaragua.png"
    , "niger.png"
    , "nigeria.png"
    , "niue.png"
    , "norfolk-island.png"
    , "northen-cyprus.png"
    , "northern-marianas-islands.png"
    , "north-korea.png"
    , "norway.png"
    , "oman.png"
    , "orkney-islands.png"
    , "ossetia.png"
    , "pakistan.png"
    , "palau.png"
    , "palestine.png"
    , "panama.png"
    , "papua-new-guinea.png"
    , "paraguay.png"
    , "peru.png"
    , "philippines.png"
    , "pitcairn-islands.png"
    , "portugal.png"
    , "puerto-rico.png"
    , "qatar.png"
    , "rapa-nui.png"
    , "republic-of-macedonia.png"
    , "republic-of-poland.png"
    , "republic-of-the-congo.png"
    , "romania.png"
    , "russia.png"
    , "rwanda.png"
    , "saba-island.png"
    , "saint-kitts-and-nevis.png"
    , "salvador.png"
    , "samoa.png"
    , "san-marino.png"
    , "sao-tome-and-principe.png"
    , "sardinia.png"
    , "saudi-arabia.png"
    , "scotland.png"
    , "senegal.png"
    , "serbia.png"
    , "seychelles.png"
    , "sierra-leone.png"
    , "singapore.png"
    , "sint-eustatius.png"
    , "sint-maarten.png"
    , "slovakia.png"
    , "slovenia.png"
    , "solomon-islands.png"
    , "somalia.png"
    , "somaliland.png"
    , "south-africa.png"
    , "south-korea.png"
    , "south-sudan.png"
    , "spain.png"
    , "sri-lanka.png"
    , "st-barts.png"
    , "st-lucia.png"
    , "st-vincent-and-the-grenadines.png"
    , "sudan.png"
    , "suriname.png"
    , "swaziland.png"
    , "sweden.png"
    , "switzerland.png"
    , "syria.png"
    , "taiwan.png"
    , "tajikistan.png"
    , "tanzania.png"
    , "thailand.png"
    , "tibet.png"
    , "togo.png"
    , "tokelau.png"
    , "tonga.png"
    , "transnistria.png"
    , "trinidad-and-tobago.png"
    , "tunisia.png"
    , "turkey.png"
    , "turkmenistan.png"
    , "turks-and-caicos.png"
    , "tuvalu.png"
    , "uganda.png"
    , "ukraine.png"
    , "united-arab-emirates.png"
    , "united-kingdom.png"
    , "united-nations.png"
    , "united-states-of-america.png"
    , "uruguay.png"
    , "uzbekistn.png"
    , "vanuatu.png"
    , "vatican-city.png"
    , "venezuela.png"
    , "vietnam.png"
    , "virgin-islands.png"
    , "wales.png"
    , "western-sahara.png"
    , "yemen.png"
    , "zambia.png"
    , "zimbabwe.png"
    ]
