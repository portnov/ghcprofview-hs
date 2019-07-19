{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

import qualified Data.Text as T
import Data.Int
import Data.Tree
import qualified Data.Map as M
import Data.Scientific
-- import Data.Typeable
import Data.GI.Base.GValue

type Id = Int

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
  , ccdRecordId :: ! Id
  , ccdRecord :: ! ProfileRecord
  , ccdCostCentre :: ! CostCentre
  , ccdChildren :: ! [CostCentreData]
  }
  deriving (Eq, Show)

class IsTree t a | t -> a where
  treeRoot :: t -> a
  treeChildren :: t -> [t]

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

data ProfileRecord = ProfileRecord {
    prCcId :: ! Id
  , prEntries :: ! Integer
  , prTicks :: ! (Maybe Integer)             -- ^ If present in input file
  , prAlloc :: ! (Maybe Integer)             -- ^ If present in input file
  , prTimeIndividual :: ! (Maybe Double) -- ^ If present in input file
  , prAllocIndividual :: ! (Maybe Double) -- ^ If present in input file
  , prTimeInherited :: ! (Maybe Double) -- ^ If present in input file
  , prAllocInherited :: ! (Maybe Double) -- ^ If present in input file
  }
  deriving (Eq, Show)

data Profile = Profile {
    profileProgram :: ! T.Text
  , profileTotalTime :: ! Double
  , profileRtsArguments :: ! [T.Text]
  , profileInitCaps :: ! Int32
  , profileTickInterval :: ! Int32
  , profileTotalAlloc :: ! Integer
  , profileTotalTicks :: ! Integer
  , profileTree :: Tree ProfileRecord
  , profileTreeMap :: M.Map Id (Tree ProfileRecord)
  , profileCostCentres :: M.Map Id CostCentre
  }
  deriving (Eq, Show)

