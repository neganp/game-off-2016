module Main exposing (..)

import Array
import Dict
import Set

import Html.App

import Data exposing(..)
import Game
import Update
import View

---------------------------------------------------------------------

initModel = { activeBureaugraphId = 0
            , maxAvailableBureaugraphId = 0
            , bureaugraphs = Game.bureaugraphs
            , activeDistrictId = 0
            , drawing = False
            }

main =
    Html.App.beginnerProgram
        { model = initModel
        , view = View.view
        , update = Update.update
        }
