module Data exposing (..)

import Dict
import Maybe
import Set

---------------------------------------------------------------------
-- Data Types

type Alignment = Red | Blue
type alias Coord = (Int, Int)

type alias DistrictId = Int
type alias BureaugraphId = Int

type alias Demograph = Dict.Dict Coord Alignment

type alias District = { id: DistrictId
                      , hq: Coord
                      , assigned: Set.Set Coord
                      }

type alias Bureaugraph = { id: Int
                         , name: String
                         , fullSize: Int
                         , demograph : Demograph
                         , districts: Dict.Dict DistrictId District
                         }

---------------------------------------------------------------------
-- Natural Operations on Data

dCoord : Coord -> Int -> Int -> Coord
dCoord coord dx dy
    =
      let
          (x,y) = coord
      in
          (x+dx, y+dy)

---------------------------------------------------------------------
-- Internal Properites of Data

districtExtent : District -> Set.Set Coord
districtExtent district
    =  Set.insert district.hq district.assigned


districtSize : District -> Int
districtSize district
    = Set.size (districtExtent district)


isHQ : Coord -> Bureaugraph -> Bool
isHQ c bgraph =
    let
        checkHQ = \_ district status -> status || district.hq == c
    in
        Dict.foldl checkHQ False bgraph.districts


districtOf : Coord -> Bureaugraph -> Maybe DistrictId
districtOf coord bgraph
    =
      let getId id district maybeId =
              case maybeId of
                  Just id -> Just id
                  Nothing -> if Set.member coord (districtExtent district)
                             then Just id
                             else Nothing
      in
          Dict.foldl getId Nothing bgraph.districts



hasNeighbour : Coord -> Set.Set Coord -> Bool
hasNeighbour coord coords
    =
      let
          potentialNeighbours =
              List.map
                  (\ (dx,dy) -> dCoord coord dx dy)
                  [(1,0), (0,1), (-1,0), (0,-1)]
      in
          List.any
              (\c -> Set.member c coords)
              potentialNeighbours

{-| Check that a set of coordinates are all orthogonally adjacent to
at least one other coordinate in the set
-}
connected : Set.Set Coord -> Bool
connected coords
    = let
        singleton = Set.size coords <= 1
        multiConnected = coords
                       |> Set.toList
                       |> List.all (\c -> hasNeighbour c coords)
   in
       singleton || multiConnected

{-| Check that a set of districts are all connected after a potential
move.
-}
validDistricts : Dict.Dict DistrictId District -> Bool
validDistricts districtDict
    =  districtDict
    |> Dict.values
    |> List.map districtExtent
    |> List.all (\coords -> connected coords)


---------------------------------------------------------------------
-- Display-able Properties of Data

districtAlignments : Bureaugraph -> Dict.Dict DistrictId Alignment
districtAlignments bgraph
    = let
        getAlign : Coord -> Alignment
        getAlign c
            = Dict.get c bgraph.demograph
            |> Maybe.withDefault Red

        countAligns : List Alignment -> Alignment -> Int
        countAligns aligns align
            = aligns
            |> List.filter (\a -> a == align)
            |> List.length

        majority : List Alignment -> Alignment
        majority aligns
            = if (countAligns aligns Red) > (countAligns aligns Blue)
              then Red
              else Blue

        winner : District -> Alignment
        winner district
            = (districtExtent district)
            |> Set.toList
            |> List.map getAlign
            |> majority

        shouldCount : District -> Bool
        shouldCount district
            = (Set.size (districtExtent district)) == bgraph.fullSize
   in
       bgraph.districts
       |> Dict.filter (\_ district -> shouldCount district)
       |> Dict.map (\_ district -> winner district)
