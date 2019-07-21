{-# LANGUAGE OverloadedStrings #-}

module Converter where

import qualified GHC.Prof.Types as P -- ghc-prof package
import Data.Tree
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Scientific

import Types

convertCc :: P.Profile -> Tree P.CostCentre -> CostCentreData
convertCc profile node = go Nothing node
  where
    profile' = convertProfile profile
    go parent node =
      let cc = rootLabel node
          ccd = CostCentreData {
            ccdProfile = profile'
          , ccdParent = parent
          , ccdRecords = [
                ProfileRecord {
                            prCcId = IndividualId $ P.costCentreNo cc
                          , prEntries = P.costCentreEntries cc
                          , prTicks = P.costCentreTicks cc
                          , prAlloc = P.costCentreBytes cc
                          , prTimeIndividual = Just $ toRealFloat $ P.costCentreIndTime cc
                          , prAllocIndividual = Just $ toRealFloat $ P.costCentreIndAlloc cc
                          , prTimeInherited = Just $ toRealFloat $ P.costCentreInhTime cc
                          , prAllocInherited = Just $ toRealFloat $ P.costCentreInhAlloc cc
                        }
              ]
          , ccdCostCentre = CostCentre {
                                ccLabel = P.costCentreName cc
                              , ccId = P.costCentreNo cc
                              , ccModule = P.costCentreModule cc
                              , ccSource = fromMaybe "<unknown>" $ P.costCentreSrc cc
                              , ccIsCaf = "CAF:" `T.isPrefixOf` P.costCentreName cc
                            }
          , ccdChildren = map (go (Just ccd)) (subForest node)
        }
      in  ccd

convertProfile :: P.Profile -> Profile
convertProfile p = Profile {
      profileProgram = P.profileCommandLine p
    , profileTotalTime = 0
    , profileRtsArguments = []
    , profileInitCaps = 0
    , profileTickInterval = 0
    , profileTotalAlloc = P.totalAllocBytes $ P.profileTotalAlloc p
    , profileTotalTicks = P.totalTimeTicks $ P.profileTotalTime p
    , profileTree = error "profile tree was not read from .prof file"
    , profileTreeMap = IM.empty
    , profileCostCentres = IM.empty
  }

