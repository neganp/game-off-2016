module Progress exposing (ready, nextBureaugraphId)

import Array
import Dict
import Maybe

import Data
import Model

---------------------------------------------------------------------

{-| The next BureapgrahId that would be available to the
player. Doesn't tell you whether they should be allowed to have it
though.
-}
nextBureaugraphId : Model.Model -> Data.BureaugraphId
nextBureaugraphId model =
    let
        maxEverBureaugraphId = (Array.length model.bureaugraphs) - 1
    in
        clamp 0 maxEverBureaugraphId (model.activeBureaugraphId + 1)

---------------------------------------------------------------------

notNothing : Maybe a -> Bool
notNothing m =
    case m of
        Nothing -> False
        Just _ -> True

{-| Check whether all of the districts have been drawn (eveyone has
been allocated a vote).
-}
allDistrictsDrawn : Data.Bureaugraph -> Bool
allDistrictsDrawn bgraph =
    let
        alignments = Data.districtAlignments bgraph
    in
        (Dict.keys bgraph.districts)
            |> List.map (\id -> Dict.get id alignments)
            |> List.all notNothing


{-| Check whether a majority of distrits are aligned Red.
-}
redAligned : Data.Bureaugraph -> Bool
redAligned bgraph =
    let
        alignments = Data.districtAlignments bgraph
        redAligned = (Dict.values alignments)
                   |> List.filter (\a -> a == Data.Red)
                   |> List.length
        blueAligned = (Dict.values alignments)
                    |> List.filter (\a -> a == Data.Blue)
                    |> List.length
    in
        redAligned > blueAligned

{-| Checks whether the player is ready to progress to the next level. -}
ready : Model.Model -> Bool
ready model =
    let
        -- conditions
        atCurrentMaximum = (model.activeBureaugraphId == model.maxAvailableBureaugraphId)
        mBureaugraph = Array.get model.activeBureaugraphId model.bureaugraphs
        checkFunction f = (Maybe.withDefault
                               False
                               (Maybe.map f mBureaugraph))
    in
        (checkFunction redAligned) && (checkFunction allDistrictsDrawn)
