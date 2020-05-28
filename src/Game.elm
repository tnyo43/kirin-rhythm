module Game exposing (..)

import Array exposing (Array)
import Binary exposing (fromHex, toBooleans)
import Playground exposing (..)


type alias Data =
    { bpm : Int
    , unit : NoteUnit
    , melody : Melody
    , elapsed : Int
    , clock : Number
    }


type NoteUnit
    = N4
    | N8
    | N16


type alias Melody =
    List (Array Bool)


initData : Data
initData =
    let
        melody =
            [ "a83019473b"
            , "100ced7d73"
            , "9184c0201a"
            , "fc24395481"
            ]
                |> List.map (fromHex >> toBooleans >> Array.fromList)
    in
    { bpm = 128
    , unit = N4
    , melody = melody
    , elapsed = 0
    , clock = 0
    }


isNoteAdd : Melody -> Int -> List Bool
isNoteAdd melody elapsed =
    List.map
        (\m ->
            Array.get elapsed m
                |> Maybe.withDefault False
        )
        melody


clock : Time -> Data -> ( Data, List Bool )
clock time data =
    let
        currentClock =
            spin 1 time

        tik =
            data.clock > currentClock

        currentElapsed =
            if tik then
                data.elapsed + 1

            else
                data.elapsed
    in
    ( { data
        | clock = currentClock
        , elapsed = currentElapsed
      }
    , if tik then
        isNoteAdd data.melody data.elapsed

      else
        List.repeat (List.length data.melody) False
    )
