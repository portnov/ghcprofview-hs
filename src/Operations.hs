{-# LANGUAGE OverloadedStrings #-}
module Operations where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
  case prTimeIndividual (ccdRecord node) of
    Just value -> value
    Nothing ->
      case prTicks (ccdRecord node) of
        Nothing -> error "no individual time percentage and no ticks data provided"
        Just ticks -> 100 * ticks // profileTotalTicks p

ccdTimeIndividual :: CostCentreData -> Double
ccdTimeIndividual ccd = timeIndividual (ccdProfile ccd) ccd

allocIndividual :: Profile -> CostCentreData -> Double
allocIndividual p node =
  case prAllocIndividual (ccdRecord node) of
    Just value -> value
    Nothing ->
      case prAlloc (ccdRecord node) of
        Nothing -> error "no individual alloc percentage and no bytes data provided"
        Just bytes -> 100 * bytes // profileTotalAlloc p

ccdAllocIndividual :: CostCentreData -> Double
ccdAllocIndividual ccd = allocIndividual (ccdProfile ccd) ccd

timeInherited :: Profile -> CostCentreData -> Double
timeInherited p node =
    case prTimeInherited (ccdRecord node) of
      Just value -> value
      Nothing -> 100 * inheritedSum node // profileTotalTicks p
  where
    inheritedSum node =
      case prTicks (ccdRecord node) of
        Nothing -> error "no inherited time percentage and no ticks data provided"
        Just ticks -> ticks + sum (map inheritedSum $ ccdChildren node)

ticksInherited :: Profile -> CostCentreData -> Maybe Integer
ticksInherited p node = inheritedSum node
  where
    inheritedSum node = do
      individual <- prTicks (ccdRecord node) 
      children <- mapM inheritedSum $ ccdChildren node
      return $ individual + sum children

ccdTimeInherited :: CostCentreData -> Double
ccdTimeInherited ccd = timeInherited (ccdProfile ccd) ccd

ccdTicksInherited :: CostCentreData -> Maybe Integer
ccdTicksInherited ccd = ticksInherited (ccdProfile ccd) ccd

allocInherited :: Profile -> CostCentreData -> Double
allocInherited p node =
    case prAllocInherited (ccdRecord node) of
      Just value -> value
      Nothing -> 100 * inheritedSum node // profileTotalAlloc p
  where
    inheritedSum node =
      case prAlloc (ccdRecord node) of
        Nothing -> error "no inherited alloc percentage and no bytes data provided"
        Just bytes -> bytes + sum (map inheritedSum $ ccdChildren node)

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

ccdEntries :: CostCentreData -> Integer
ccdEntries ccd = prEntries (ccdRecord ccd)

calcTotals :: CostCentreData -> Maybe (Integer, Integer)
calcTotals ccd = calc ccd
  where
    calc node = do
      (childTicks_s, childAlloc_s) <- unzip <$> mapM calc (ccdChildren node)
      ticks <- prTicks (ccdRecord node) 
      alloc <- prAlloc (ccdRecord node) 
      return (ticks + sum childTicks_s, alloc + sum childAlloc_s)

updateTotals :: CostCentreData -> CostCentreData
updateTotals node =
  case calcTotals node of
    Nothing -> node
    Just (totalTicks, totalAlloc) ->
      let profile' = (ccdProfile node) {
                         profileTotalTicks = totalTicks,
                         profileTotalAlloc = totalAlloc
                      }
          updateCcd ccd = ccd {
                            ccdProfile = profile',
                            ccdChildren = map updateCcd (ccdChildren ccd)
                          }
      in  updateCcd node
          
printTree :: CostCentreData -> IO ()
printTree node = go 0 node
  where
    go i node = do
      let prefix = T.replicate i " "
      TIO.putStrLn $ prefix <>
          ccdLabel node <> "\t"
          <> T.pack (show $ ccdTicksInherited node) <> "\t"
          <> T.pack (show $ ccdTimeInherited node)
      forM_ (ccdChildren node) $ \child ->
        go (i+1) child

