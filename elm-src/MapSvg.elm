module MapSvg exposing (mapSvg)

import Dict
import List

import Html exposing (div, Html)
import Html.App

import Svg
import Svg.Attributes exposing(x, y, width, height, fill)

import Data
import MapUtil exposing(..)
import DebugSvg
import Message

---------------------------------------------------------------------

attrColor alignment =
    case alignment of
        Data.Red -> "#ffbc00"
        Data.Blue -> "#0023A3"


cell : Data.Coord -> Data.Alignment -> Svg.Svg Message.Msg
cell (xx,yy) alignment =
    let
        xPx = cellDim * xx + cellPadding
        yPx = cellDim * yy + cellPadding
    in
        Svg.rect
            [ x <| toPx xPx
            , y <| toPx yPx
            , width <| toPx cellInnerDim
            , height <| toPx cellInnerDim
            , fill <| attrColor alignment
            ]
            []


mapSvg : Data.Demograph -> List (Svg.Svg Message.Msg)
mapSvg data
    = data
    |> Dict.map cell
    |> Dict.values

---------------------------------------------------------------------
-- Debugging

{-
exampleMap = Dict.fromList [ ((0,0), Data.Red)
                          , ((0,1), Data.Blue)
                          , ((1,0), Data.Red)
                          , ((1,1), Data.Blue)
                          ]

main = DebugSvg.svgShower mapSvg exampleMap
---}
