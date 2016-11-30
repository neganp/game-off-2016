module Message exposing (..)

import Data

type Msg = SetActiveBureaugraph Data.BureaugraphId
         | EnterCell Data.Coord
         | TapCell Data.Coord
         | StopDrawing
         | ResetAll
         | ResetDistrict Data.DistrictId
         | Legislate
