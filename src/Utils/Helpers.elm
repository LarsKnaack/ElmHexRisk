module Utils.Helpers exposing (..)

import Array exposing (Array)
import Random exposing (Seed)


shuffleArray : Seed -> Array a -> Array a
shuffleArray seed array =
    shuffleHelper seed array Array.empty


shuffleHelper : Seed -> Array a -> Array a -> Array a
shuffleHelper seed array result =
    if Array.isEmpty array then
        result

    else
        let
            indexGenerator =
                Random.int 0 (Array.length array - 1)

            ( index, nextSeed ) =
                Random.step indexGenerator seed

            value =
                Array.get index array

            newArray =
                Array.append (Array.slice 0 index array) (Array.slice (index + 1) (Array.length array) array)
        in
        case value of
            Just v ->
                shuffleHelper nextSeed newArray (Array.push v result)

            Nothing ->
                result
