module Model exposing (Model)

import Array

import Data

---------------------------------------------------------------------

type alias  Model = { activeBureaugraphId: Data.BureaugraphId
                    , maxAvailableBureaugraphId: Data.BureaugraphId
                    , bureaugraphs: Array.Array Data.Bureaugraph
                    , activeDistrictId: Data.DistrictId
                    , drawing: Bool
                    }
