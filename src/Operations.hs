{-# LANGUAGE OverloadedStrings #-}
module Operations where

import Control.Monad
import qualified Data.Text as T
import Data.Int
import Data.Tree
import qualified Data.Map as M

import Types

(//) :: (Integral a, Fractional b) => a -> a -> b
x // y = fromIntegral x / fromIntegral y

findRecord :: Profile -> Id -> Tree ProfileRecord
findRecord p id =
    case M.lookup id (profileTreeMap p) of
      Nothing -> error $ "record not found: " ++ show id
      Just tree -> tree

resolveRecord :: CostCentreData -> Tree ProfileRecord
resolveRecord ccd = findRecord (ccdProfile ccd) (ccdRecordId ccd)

resolved :: (Profile -> Tree ProfileRecord -> a) -> CostCentreData -> a
resolved fn ccd =
  let tree = resolveRecord ccd
  in  fn (ccdProfile ccd) tree

resolveProfile :: Profile -> CostCentreData
resolveProfile p = go (profileTree p)
  where
    go node =
      let root = CostCentreData {
                     ccdProfile = p
                   , ccdRecordId = id
                   , ccdRecord = rootLabel node
                   , ccdCostCentre = cc
                   , ccdChildren = children
                  }
          children = map go (subForest node)
          id = prCcId (rootLabel node)
          Just cc = M.lookup id (profileCostCentres p)
      in  root

withCostCentre :: (CostCentre -> a) -> CostCentreData -> a
withCostCentre fn ccd =
  fn (ccdCostCentre ccd)

filterTree :: (a -> Bool) -> Tree a -> Tree a
filterTree good node = Node (rootLabel node) $ go (subForest node)
  where
    go [] = []
    go (node : nodes)
      | good (rootLabel node) =
        let node' = Node (rootLabel node) $ go (subForest node)
        in  node' : go nodes
      | otherwise = go nodes

filterCcd :: (CostCentreData -> Bool) -> CostCentreData -> CostCentreData
filterCcd good node = node {ccdChildren = go (ccdChildren node)}
  where
    go [] = []
    go (node : nodes)
      | good node =
        let node' = node {ccdChildren = go (ccdChildren node)}
        in  node' : nodes
      | otherwise = go nodes

timeIndividual :: Profile -> CostCentreData -> Double
timeIndividual p node =
  100 * prTicks (ccdRecord node) // profileTotalTicks p

ccdTimeIndividual :: CostCentreData -> Double
ccdTimeIndividual ccd = timeIndividual (ccdProfile ccd) ccd

allocIndividual :: Profile -> CostCentreData -> Double
allocIndividual p node =
  100 * prAlloc (ccdRecord node) // profileTotalAlloc p

ccdAllocIndividual :: CostCentreData -> Double
ccdAllocIndividual ccd = allocIndividual (ccdProfile ccd) ccd

timeInherited :: Profile -> CostCentreData -> Double
timeInherited p node = 100 * inheritedSum node // profileTotalTicks p
  where
    inheritedSum node =
      prTicks (ccdRecord node) + sum (map inheritedSum $ ccdChildren node)

ticksInherited :: Profile -> CostCentreData -> Int64
ticksInherited p node = inheritedSum node
  where
    inheritedSum node =
      prTicks (ccdRecord node) + sum (map inheritedSum $ ccdChildren node)

ccdTimeInherited :: CostCentreData -> Double
ccdTimeInherited ccd = timeInherited (ccdProfile ccd) ccd

ccdTicksInherited :: CostCentreData -> Int64
ccdTicksInherited ccd = ticksInherited (ccdProfile ccd) ccd

allocInherited :: Profile -> CostCentreData -> Double
allocInherited p node = 100 * inheritedSum node // profileTotalAlloc p
  where
    inheritedSum node =
      prAlloc (ccdRecord node) + sum (map inheritedSum $ ccdChildren node)

ccdAllocInherited :: CostCentreData -> Double
ccdAllocInherited ccd = allocInherited (ccdProfile ccd) ccd

ccdLabel :: CostCentreData -> T.Text
ccdLabel = withCostCentre ccLabel

ccdModule :: CostCentreData -> T.Text
ccdModule = withCostCentre ccModule

ccdSource :: CostCentreData -> T.Text
ccdSource = withCostCentre ccSource

ccdIsCaf :: CostCentreData -> Bool
ccdIsCaf = withCostCentre ccIsCaf

ccdToIgnore :: CostCentreData -> Bool
ccdToIgnore = withCostCentre $ \cc -> ccIsCaf cc || ccLabel cc `elem` [
    "OVERHEAD_of",
    "DONT_CARE",
    "GC",
    "SYSTEM",
    "IDLE"
  ]

ccdEntries :: CostCentreData -> Int64
ccdEntries ccd = prEntries (ccdRecord ccd)

calcTotals :: CostCentreData -> (Int64, Int64)
calcTotals ccd = calc ccd
  where
    calc node =
      let (childTicks_s, childAlloc_s) = unzip $ map calc $ ccdChildren node
      in (prTicks (ccdRecord node) + sum childTicks_s,
          prAlloc (ccdRecord node) + sum childAlloc_s)

updateTotals :: CostCentreData -> CostCentreData
updateTotals node =
  let (totalTicks, totalAlloc) = calcTotals node
      profile' = (ccdProfile node) {
                     profileTotalTicks = totalTicks,
                     profileTotalAlloc = totalAlloc
                  }
      updateCcd ccd = ccd {
                        ccdProfile = profile',
                        ccdChildren = map updateCcd (ccdChildren ccd)
                      }
  in  updateCcd node
          
