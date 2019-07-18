{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

import qualified Data.Text as T
import Data.Int
import Data.Tree
import qualified Data.Map as M
-- import Data.Typeable

type Id = Int32

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
  , prTicks :: ! Int64
  , prEntries :: ! Int64
  , prAlloc :: ! Int64
  }
  deriving (Eq, Show)

data Profile = Profile {
    profileProgram :: ! T.Text
  , profileTotalTime :: ! Double
  , profileRtsArguments :: ! [T.Text]
  , profileInitCaps :: ! Int32
  , profileTickInterval :: ! Int32
  , profileTotalAlloc :: ! Int64
  , profileTotalTicks :: ! Int64
  , profileTree :: Tree ProfileRecord
  , profileTreeMap :: M.Map Id (Tree ProfileRecord)
  , profileCostCentres :: M.Map Id CostCentre
  }
  deriving (Eq, Show)

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

