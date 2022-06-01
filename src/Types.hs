{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Types where

import qualified Data.Text as T
import Data.Int
import Data.Tree
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Scientific
-- import Data.Typeable
import Data.GI.Base.GValue

type Id = Int

class IsTree t a | t -> a where
  treeRoot :: t -> a
  treeChildren :: t -> [t]

data AggregateState = Individual | Aggregated
  deriving (Eq, Show)

data RecordId (a :: AggregateState) where
  IndividualId :: Id -> RecordId Individual
  AggregatedId :: IS.IntSet -> RecordId Aggregated

instance Show (RecordId a) where
  show (IndividualId id) = show id
  show (AggregatedId set) = show set

instance IsGValue Int where
  gvalueGType_ = gvalueGType_ @Int64
  gvalueSet_ ptr v = gvalueSet_ ptr (fromIntegral v :: Int64)
  gvalueGet_ v = fromIntegral `fmap` (gvalueGet_ v :: IO Int64)

instance IsGValue Integer where
  gvalueGType_ = gvalueGType_ @Int64
  gvalueSet_ ptr v = gvalueSet_ ptr (fromIntegral v :: Int64)
  gvalueGet_ v = fromIntegral `fmap` (gvalueGet_ v :: IO Int64)

instance IsGValue Scientific where
  gvalueGType_ = gvalueGType_ @Double
  gvalueSet_ ptr v = gvalueSet_ ptr (toRealFloat v :: Double)
  gvalueGet_ v = fromFloatDigits `fmap` (gvalueGet_ v :: IO Double)

data CostCentreData = CostCentreData {
    ccdProfile :: !Profile
  , ccdParent :: Maybe CostCentreData
  , ccdRecords :: ![ProfileRecord Individual]
  , ccdCostCentre :: !CostCentre
  , ccdChildren :: ![CostCentreData]
  }
  deriving (Show)

instance IsTree CostCentreData CostCentreData where
  treeRoot = id
  treeChildren = ccdChildren

data CostCentre = CostCentre {
    ccLabel :: !T.Text
  , ccId :: !Id
  , ccModule :: !T.Text
  , ccSource :: !T.Text
  , ccIsCaf :: !Bool
  }
  deriving (Eq, Show)

data ProfileRecord s = ProfileRecord {
    prCcId :: !(RecordId s)
  , prEntries :: !Integer
  , prTicks :: !(Maybe Integer)             -- ^ If present in input file
  , prAlloc :: !(Maybe Integer)             -- ^ If present in input file
  , prTimeIndividual :: !(Maybe Double) -- ^ If present in input file
  , prAllocIndividual :: !(Maybe Double) -- ^ If present in input file
  , prTimeInherited :: !(Maybe Double) -- ^ If present in input file
  , prAllocInherited :: !(Maybe Double) -- ^ If present in input file
  }
  deriving (Show)

singleRecordId :: ProfileRecord Individual -> Id
singleRecordId r@(ProfileRecord {prCcId = IndividualId id}) = id

listRecordId :: ProfileRecord a -> [Id]
listRecordId (ProfileRecord {prCcId = IndividualId id}) = [id]
listRecordId (ProfileRecord {prCcId = AggregatedId set}) = IS.toList set

data Profile = Profile {
    profileProgram :: !T.Text
  , profileTotalTime :: !Double
  , profileRtsArguments :: ![T.Text]
  , profileInitCaps :: !Int32
  , profileTickInterval :: !Int32
  , profileTotalAlloc :: !Integer
  , profileTotalTicks :: !Integer
  , profileTree :: Tree (ProfileRecord Individual)
  , profileTreeMap :: IM.IntMap (Tree (ProfileRecord Individual))
  , profileCostCentres :: IM.IntMap CostCentre
  }
  deriving (Show)

data FilterParams = FilterParams {
        fpEntries :: Integer
      , fpTimeIndividual :: Double
      , fpAllocIndividual :: Double
      , fpTimeInherited :: Double
      , fpAllocInherited :: Double
      , fpModule :: T.Text
      , fpSource :: T.Text
    }

data SearchMetohd = Contains | Exact | Regexp
  deriving (Eq, Show, Read, Enum, Bounded)

