{-# LANGUAGE OverloadedStrings #-}

module Converter where

import qualified GHC.Prof.Types as P -- ghc-prof package
import Data.Tree
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Scientific

import Types

convertCc :: P.Profile -> Tree P.CostCentre -> CostCentreData
convertCc profile node =
  let cc = rootLabel node
  in CostCentreData {
        ccdProfile = convertProfile profile
      , ccdRecordId = P.costCentreNo cc
      , ccdRecord = ProfileRecord {
                        prCcId = P.costCentreNo cc
                      , prEntries = P.costCentreEntries cc
                      , prTicks = P.costCentreTicks cc
                      , prAlloc = P.costCentreBytes cc
                      , prTimeIndividual = Just $ toRealFloat $ P.costCentreIndTime cc
                      , prAllocIndividual = Just $ toRealFloat $ P.costCentreIndAlloc cc
                      , prTimeInherited = Just $ toRealFloat $ P.costCentreInhTime cc
                      , prAllocInherited = Just $ toRealFloat $ P.costCentreInhAlloc cc
                    }
      , ccdCostCentre = CostCentre {
                            ccLabel = P.costCentreName cc
                          , ccId = P.costCentreNo cc
                          , ccModule = P.costCentreModule cc
                          , ccSource = fromMaybe "<unknown>" $ P.costCentreSrc cc
                          , ccIsCaf = "CAF:" `T.isPrefixOf` P.costCentreName cc
                        }
      , ccdChildren = map (convertCc profile) (subForest node)
    }

convertProfile :: P.Profile -> Profile
convertProfile p = Profile {
      profileProgram = P.profileCommandLine p
    , profileTotalTime = 0
    , profileRtsArguments = []
    , profileInitCaps = 0
    , profileTickInterval = 0
    , profileTotalAlloc = P.totalAllocBytes $ P.profileTotalAlloc p
    , profileTotalTicks = P.totalTimeTicks $ P.profileTotalTime p
    , profileTree = undefined
    , profileTreeMap = M.empty
    , profileCostCentres = M.empty
  }

