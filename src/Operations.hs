{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Operations where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tree
import Data.Int
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Types

(//) :: (Integral a, Fractional b) => a -> a -> b
x // y = fromIntegral x / fromIntegral y

fromSingletonSet :: IS.IntSet -> Id
fromSingletonSet set =
  if IS.size set == 1
    then head (IS.toList set)
    else error "fromSingletonSet for a non-singleton set"

resolveProfile :: Profile -> CostCentreData
resolveProfile p = go (profileTree p)
  where
    go node =
      let root = CostCentreData {
                     ccdProfile = p
                   , ccdRecords = [rootLabel node]
                   , ccdCostCentre = cc
                   , ccdChildren = children
                  }
          children = map go (subForest node)
          IndividualId id = prCcId (rootLabel node)
          Just cc = IM.lookup id (profileCostCentres p)
      in  root

withCostCentre :: (CostCentre -> a) -> CostCentreData -> a
withCostCentre fn ccd =
  fn (ccdCostCentre ccd)

toAggregated :: ProfileRecord Individual -> ProfileRecord Aggregated
toAggregated r@(ProfileRecord {prCcId = IndividualId id}) =
  r {prCcId = AggregatedId (IS.singleton id)}

summaryRecord :: CostCentreData -> ProfileRecord Aggregated
summaryRecord ccd = summary (ccdRecords ccd)
  where
    summary = foldr plus zero
    zero = ProfileRecord {
               prCcId = AggregatedId IS.empty
             , prEntries = 0
             , prTicks = Nothing
             , prAlloc = Nothing
             , prTimeIndividual = Nothing
             , prAllocIndividual = Nothing
             , prTimeInherited = Nothing
             , prAllocInherited = Nothing
            }

    plusN :: Num a => Maybe a -> Maybe a -> Maybe a
    plusN Nothing Nothing = Nothing
    plusN (Just x) Nothing = Just x
    plusN Nothing (Just y) = Just y
    plusN (Just x) (Just y) = Just (x+y)
    
    plus :: ProfileRecord Individual -> ProfileRecord Aggregated -> ProfileRecord Aggregated
    plus r@(ProfileRecord {prCcId = IndividualId id}) agg@(ProfileRecord {prCcId = AggregatedId set})
      | id `IS.member` set = agg
      | otherwise = ProfileRecord {
                        prCcId = AggregatedId (IS.insert id set)
                      , prEntries = prEntries r + prEntries agg
                      , prTicks = prTicks r `plusN` prTicks agg
                      , prAlloc = prAlloc r `plusN` prAlloc agg
                      , prTimeIndividual = prTimeIndividual r `plusN` prTimeIndividual agg
                      , prAllocIndividual = prAllocIndividual r `plusN` prAllocIndividual agg
                      , prTimeInherited = prTimeInherited r `plusN` prTimeInherited agg
                      , prAllocInherited = prAllocInherited r `plusN` prAllocInherited agg
                    }

ccdId :: CostCentreData -> (T.Text, T.Text, T.Text)
ccdId ccd = (ccdModule ccd, ccdSource ccd, ccdLabel ccd)

ccdPlus :: CostCentreData -> CostCentreData -> CostCentreData
ccdPlus c1 c2 = c1 {
                    ccdRecords = addRecords (ccdRecords c1) (ccdRecords c2)
                  , ccdChildren = addChildren (ccdChildren c1) (ccdChildren c2)
                }
  where
    addRecords rs1 rs2 =
      let ids1 = IS.fromList (map singleRecordId rs1)
          rs2' = filter (\r -> singleRecordId r `IS.notMember` ids1) rs2
      in  rs1 ++ rs2'

    addChildren cs1 cs2 =
      let zero :: M.Map (T.Text, T.Text, T.Text) CostCentreData
          zero = M.fromList [(ccdId c, c) | c <- cs1]

          plus :: CostCentreData -> M.Map (T.Text, T.Text, T.Text) CostCentreData -> M.Map (T.Text, T.Text, T.Text) CostCentreData
          plus c result = M.insertWith ccdPlus (ccdId c) c result
      in  M.elems $ foldr plus zero cs2

ccdSum :: [CostCentreData] -> CostCentreData
ccdSum list = foldr1 ccdPlus list

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
        in  node' : go nodes
      | otherwise = go nodes

filterCcdRecursive :: (CostCentreData -> Bool) -> CostCentreData -> CostCentreData
filterCcdRecursive check node = node {ccdChildren = go (ccdChildren node)}
  where
    go [] = []
    go (node : nodes)
      | check node =
        let node' = node {ccdChildren = go (ccdChildren node)}
        in  node' : go nodes
      | otherwise =
        let children' = go (ccdChildren node)
            node' = node {ccdChildren = children'}
        in  if null children'
              then go nodes
              else node' : go nodes

ccdCheckRecursive :: (CostCentreData -> Bool) -> CostCentreData -> Bool
ccdCheckRecursive check ccd = go ccd
  where
    go ccd
      | check ccd = True
      | otherwise = any go (ccdChildren ccd)

timeIndividual :: Profile -> CostCentreData -> Double
timeIndividual p node =
  case prTimeIndividual (summaryRecord node) of
    Just value -> value
    Nothing ->
      case prTicks (summaryRecord node) of
        Nothing -> error "no individual time percentage and no ticks data provided"
        Just ticks -> 100 * ticks // profileTotalTicks p

ccdTimeIndividual :: CostCentreData -> Double
ccdTimeIndividual ccd = timeIndividual (ccdProfile ccd) ccd

allocIndividual :: Profile -> CostCentreData -> Double
allocIndividual p node =
  case prAllocIndividual (summaryRecord node) of
    Just value -> value
    Nothing ->
      case prAlloc (summaryRecord node) of
        Nothing -> error "no individual alloc percentage and no bytes data provided"
        Just bytes -> 100 * bytes // profileTotalAlloc p

ccdAllocIndividual :: CostCentreData -> Double
ccdAllocIndividual ccd = allocIndividual (ccdProfile ccd) ccd

timeInherited :: Profile -> CostCentreData -> Double
timeInherited p node =
    case prTimeInherited (summaryRecord node) of
      Just value -> value
      Nothing -> 100 * inheritedSum node // profileTotalTicks p
  where
    inheritedSum node =
      case prTicks (summaryRecord node) of
        Nothing -> error "no inherited time percentage and no ticks data provided"
        Just ticks -> ticks + sum (map inheritedSum $ ccdChildren node)

ticksInherited :: Profile -> CostCentreData -> Maybe Integer
ticksInherited p node = inheritedSum node
  where
    inheritedSum node = do
      individual <- prTicks (summaryRecord node) 
      children <- mapM inheritedSum $ ccdChildren node
      return $ individual + sum children

ccdTimeInherited :: CostCentreData -> Double
ccdTimeInherited ccd = timeInherited (ccdProfile ccd) ccd

ccdTicksInherited :: CostCentreData -> Maybe Integer
ccdTicksInherited ccd = ticksInherited (ccdProfile ccd) ccd

allocInherited :: Profile -> CostCentreData -> Double
allocInherited p node =
    case prAllocInherited (summaryRecord node) of
      Just value -> value
      Nothing -> 100 * inheritedSum node // profileTotalAlloc p
  where
    inheritedSum node =
      case prAlloc (summaryRecord node) of
        Nothing -> error "no inherited alloc percentage and no bytes data provided"
        Just bytes -> bytes + sum (map inheritedSum $ ccdChildren node)

ccdAllocInherited :: CostCentreData -> Double
ccdAllocInherited ccd = allocInherited (ccdProfile ccd) ccd

ccdLabel :: CostCentreData -> T.Text
ccdLabel = withCostCentre ccLabel

ccdRecordIds :: CostCentreData -> String
ccdRecordIds ccd = show $ concatMap listRecordId (ccdRecords ccd)

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
ccdEntries ccd = prEntries (summaryRecord ccd)

calcTotals :: CostCentreData -> Maybe (Integer, Integer)
calcTotals ccd = calc ccd
  where
    calc node = do
      (childTicks_s, childAlloc_s) <- unzip <$> mapM calc (ccdChildren node)
      ticks <- prTicks (summaryRecord node) 
      alloc <- prAlloc (summaryRecord node) 
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

ccdFind :: T.Text -> T.Text -> T.Text -> CostCentreData -> [CostCentreData]
ccdFind mod src label ccd = self ++ children
  where
    self
      | ccdId ccd == (mod, src, label) = [ccd]
      | otherwise = []
    
    children = concatMap (ccdFind mod src label) (ccdChildren ccd)

ccdByIdStr :: String -> CostCentreData -> Maybe CostCentreData
ccdByIdStr idStr ccd
      | ccdRecordIds ccd == idStr = Just ccd
      | otherwise = go (ccdChildren ccd)
  where
    go [] = Nothing
    go (child : children) =
      case ccdByIdStr idStr child of
        Just found -> Just found
        Nothing -> go children

ccdByPath :: [Int32] -> CostCentreData -> Maybe CostCentreData
ccdByPath path ccd = go (tail path) ccd
  where
    go [] ccd = Just ccd
    go (ix : ixs) ccd
      | fromIntegral ix >= length (ccdChildren ccd) = Nothing
      | otherwise = go ixs (ccdChildren ccd !! fromIntegral ix)

checkFilter :: FilterParams -> CostCentreData -> Bool
checkFilter (FilterParams {..}) ccd =
    ccdEntries ccd >= fpEntries &&
    ccdTimeIndividual ccd >= fpTimeIndividual &&
    ccdAllocIndividual ccd >= fpAllocIndividual &&
    ccdTimeInherited ccd >= fpTimeInherited &&
    ccdAllocInherited ccd >= fpAllocInherited &&
    fpSource `T.isInfixOf` ccdSource ccd &&
    fpModule `T.isInfixOf` ccdModule ccd
          
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

