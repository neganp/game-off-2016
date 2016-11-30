module DistrictSvg exposing (districtSvg)

import Dict
import Set

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Data exposing (..)
import Message
import MapUtil exposing (..)
import DebugSvg

---------------------------------------------------------------------

type Side = North | South | East | West

type alias Vec = { x1 : Int
                 , x2 : Int
                 , y1 : Int
                 , y2 : Int
                 }

---------------------------------------------------------------------
-- Draw the district boundary for a single side of a single cell.

borderVec : Coord -> Side -> Vec
borderVec (x,y) s =
    case s of
        North -> { x1 = x * cellDim
                 , x2 = (x + 1) * cellDim
                 , y1 = y * cellDim
                 , y2 = y * cellDim
                 }
        South -> { x1 = x * cellDim
                 , x2 = (x + 1) * cellDim
                 , y1 = (y + 1) * cellDim
                 , y2 = (y + 1) * cellDim
                 }
        East -> { x1 = (x + 1) * cellDim
                , x2 = (x + 1) * cellDim
                , y1 = y * cellDim
                , y2 = (y + 1) * cellDim
                }
        West -> { x1 = x * cellDim
                , x2 = x * cellDim
                , y1 = y * cellDim
                , y2 = (y + 1) * cellDim
                }

borderVecToSvg : Vec -> Svg Message.Msg
borderVecToSvg v =
    line [ strokeWidth <| toPx 2
         , stroke "black"
         , x1 <| toPx v.x1
         , x2 <| toPx v.x2
         , y1 <| toPx v.y1
         , y2 <| toPx v.y2
         , strokeLinecap "square"
         ]
        []

borderSvg : (Coord, Side) -> Svg Message.Msg
borderSvg (c,s) = borderVec c s
                |> borderVecToSvg

---------------------------------------------------------------------
-- Draw the boundaries for a single district

containsNeighbor : District -> Coord -> Side -> Bool
containsNeighbor district coord side =
    let
        (x,y) = coord
        neighborCoord = case side of
                            North -> (x,y-1)
                            South -> (x, y+1)
                            East -> (x+1,y)
                            West -> (x-1,y)
    in
        Set.member neighborCoord (Data.districtExtent district)

{-| Given a coordinate and a district, which of it's sides (North,
East, South, and West) give a list of sides to draw (i.e., sides where
the square's neighbour is not also in the district).
-}
sidesToDraw : District -> Coord -> List (Coord, Side)
sidesToDraw district coord
    =  [North, South, East, West]
    |> List.filter (\s -> not (containsNeighbor district coord s))
    |> List.map (\s -> (coord, s))

{-| Given a district, construct the list of (coordinate, side) pairs
that define its boundary.
-}
districtBorders : District -> List (Coord, Side)
districtBorders d = Set.toList (Data.districtExtent d)
                  |> List.map (\c -> sidesToDraw d c)
                  |> List.concat

districtBorderSvgs : District -> List (Svg Message.Msg)
districtBorderSvgs district
    =  districtBorders district
    |> List.map borderSvg

---------------------------------------------------------------------
-- Draw a district HQ

districtHqSvg : District -> List (Svg Message.Msg)
districtHqSvg district
    =
      let
          id = toString district.id
          (idx,idy) = district.hq
          drawX = (idx * cellDim) + (cellPadding * 2)
          drawY = (idy * cellDim) + (cellPadding * 4)
      in
          [text' [ x <| toPx drawX
                 , y <| toPx drawY
                 , fontSize "20pt"
                 ]
               [text id]
          ]

---------------------------------------------------------------------
-- Draw a whole district

districtSvg : District -> List (Svg Message.Msg)
districtSvg district
    =
      let
          hq = districtHqSvg district
          borders = districtBorderSvgs district
      in
          hq ++ borders

---------------------------------------------------------------------
-- Draw the whole map-UI for districts

districtsSvg : Dict.Dict DistrictId District -> List (Svg Message.Msg)
districtsSvg districts
    =  districts
    |> Dict.map (\_ district -> districtSvg district)
    |> Dict.values
    |> List.concat


---------------------------------------------------------------------
-- Debugging

{-

exampleDistricts = Dict.fromList
                   [ (1, {id = 1, hq = (0,0), assigned = Set.empty})
                   , (2, {id = 2, hq = (1,1), assigned = Set.empty})
                   , (3, {id = 3, hq = (2,2), assigned = Set.empty})
                   ]


main = DebugSvg.svgShower districtsSvg exampleDistricts
--}
