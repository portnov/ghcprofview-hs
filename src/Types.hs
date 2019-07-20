{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

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
  toGValue x = toGValue (fromIntegral x :: Int64)
  fromGValue v = fromIntegral `fmap` (fromGValue v :: IO Int64)

instance IsGValue Integer where
  toGValue x = toGValue (fromIntegral x :: Int64)
  fromGValue v = fromIntegral `fmap` (fromGValue v :: IO Int64)

instance IsGValue Scientific where
  toGValue x = toGValue (toRealFloat x :: Double)
  fromGValue v = fromFloatDigits `fmap` (fromGValue v :: IO Double)

data CostCentreData = CostCentreData {
    ccdProfile :: ! Profile
  , ccdRecords :: ! [ProfileRecord Individual]
  , ccdCostCentre :: ! CostCentre
  , ccdChildren :: ! [CostCentreData]
  }
  deriving (Show)

instance IsTree CostCentreData CostCentreData where
  treeRoot = id
  treeChildren = ccdChildren

data CostCentre = CostCentre {
    ccLabel :: ! T.Text
  , ccId :: ! Id
  , ccModule :: ! T.Text
  , ccSource :: ! T.Text
  , ccIsCaf :: ! Bool
  }
  deriving (Eq, Show)

data ProfileRecord s = ProfileRecord {
    prCcId :: ! (RecordId s)
  , prEntries :: ! Integer
  , prTicks :: ! (Maybe Integer)             -- ^ If present in input file
  , prAlloc :: ! (Maybe Integer)             -- ^ If present in input file
  , prTimeIndividual :: ! (Maybe Double) -- ^ If present in input file
  , prAllocIndividual :: ! (Maybe Double) -- ^ If present in input file
  , prTimeInherited :: ! (Maybe Double) -- ^ If present in input file
  , prAllocInherited :: ! (Maybe Double) -- ^ If present in input file
  }
  deriving (Show)

singleRecordId :: ProfileRecord Individual -> Id
singleRecordId r@(ProfileRecord {prCcId = IndividualId id}) = id

data Profile = Profile {
    profileProgram :: ! T.Text
  , profileTotalTime :: ! Double
  , profileRtsArguments :: ! [T.Text]
  , profileInitCaps :: ! Int32
  , profileTickInterval :: ! Int32
  , profileTotalAlloc :: ! Integer
  , profileTotalTicks :: ! Integer
  , profileTree :: Tree (ProfileRecord Individual)
  , profileTreeMap :: IM.IntMap (Tree (ProfileRecord Individual))
  , profileCostCentres :: IM.IntMap CostCentre
  }
  deriving (Show)

