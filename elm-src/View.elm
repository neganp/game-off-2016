module View exposing (view)

import Array
import Dict

import Html exposing (div, button, text, p)
import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onMouseUp, onClick)
import Svg
import Svg.Attributes exposing(viewBox, preserveAspectRatio)

import Data
import DistrictSvg
import MapSvg
import MapControl
import Message
import Model
import Progress

---------------------------------------------------------------------
-- Utils

toggleButton : Message.Msg -> String -> Bool -> Html.Html Message.Msg
toggleButton msg label available =
    (button
         [ class (if available
                  then ""
                  else "unavailable")
         , onClick msg
         ]
         [text label])

---------------------------------------------------------------------
-- Controls

districtAlignControl : (Data.DistrictId,  Maybe Data.Alignment) -> Html.Html Message.Msg
districtAlignControl (id, alignment)=
    let
        colorClass = case alignment of
                         Just Data.Red -> "aligned-red"
                         Just Data.Blue -> "aligned-blue"
                         Nothing -> "aligned-none"
    in
        Html.li [ class "district-score"
                , class colorClass
                ]
            [ text (toString id)
            , button [onClick (Message.ResetDistrict id)] [text "X"]
            ]

scoreView : Maybe Data.Bureaugraph -> Html.Html Message.Msg
scoreView bgraph =
    case bgraph of
        Nothing -> div [] [text "Error: No active bureaugraph?"]
        Just bgraph ->
            let
                alignments = Data.districtAlignments bgraph
                districtIds = List.sort <| Dict.keys bgraph.districts
                districtAlignments =
                    List.map
                        (\id -> (id, Dict.get id alignments))
                        districtIds
            in
                Html.ul
                    [id "district-alignments"]
                    (List.map districtAlignControl districtAlignments)


stage : Int -> Int -> Html.Html Message.Msg
stage current total =
    let
        current = toString current
        total = toString total
        content = "Lvl: " ++ current ++ " / " ++ total
    in
        p []
          [text content]

controlsView : Model.Model -> Html.Html Message.Msg
controlsView model =
    let
        activeBureaugraph = Array.get model.activeBureaugraphId model.bureaugraphs
        canLegislate = Progress.ready model
        nextAvailable = model.activeBureaugraphId < model.maxAvailableBureaugraphId
        prevAvailable = model.activeBureaugraphId > 0
    in
        (div
         [id "controls"]
         [ (stage
                (model.activeBureaugraphId + 1)
                ((Array.length model.bureaugraphs)))
         , (toggleButton
                (Message.SetActiveBureaugraph (model.activeBureaugraphId - 1))
                "Prev"
                prevAvailable)
         , (toggleButton
                (Message.SetActiveBureaugraph (model.activeBureaugraphId + 1))
                "Next"
                nextAvailable)
         , scoreView activeBureaugraph
         , (toggleButton Message.Legislate "Legislate!" canLegislate)
         ]
        )

---------------------------------------------------------------------
-- Map

bureaugraphSvg : Maybe Data.Bureaugraph -> Html.Html Message.Msg
bureaugraphSvg mbgraph =
    case mbgraph of
        Nothing -> div [] [text "Error; no active bureagraph"]
        Just bgraph ->
            let
                demographSvg = MapSvg.mapSvg bgraph.demograph
                districtSvgs = bgraph.districts
                             |> Dict.values
                             |> List.map DistrictSvg.districtSvg
                             |> List.concat
            in
                div [ id "map" ]
                    [ button [onClick Message.ResetAll] [text "Reset"]
                    , p [] [text bgraph.name]
                    , Svg.svg
                        [ viewBox "-3 -3 804 804"
                        , preserveAspectRatio "xMidYMid meet"
                        ]
                        (demographSvg ++ districtSvgs ++ MapControl.controlRects)
                    ]

---------------------------------------------------------------------
-- Synthesis

view : Model.Model -> Html.Html Message.Msg
view model =
    div [ style [ ("margin", "1em")
                , ("padding", "1em")]
        , onMouseUp Message.StopDrawing
        , id "appcontainer"
        ]
        [ bureaugraphSvg (Array.get model.activeBureaugraphId model.bureaugraphs)
        , controlsView model
        ]
