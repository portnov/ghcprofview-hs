{-# LANGUAGE OverloadedStrings #-}

module Gui where

import qualified Data.Text as T () -- instances only

import Data.GI.Base.GType
import Data.GI.Base.GValue

import Types
import Operations
import Gui.TreeWidget

treeWidgetConfig :: TreeWidgetConfig CostCentreData
treeWidgetConfig = TreeWidgetConfig 
  [
    Column "No" gtypeInt TextColumn (toGValue . Just . ccdRecordIds),
    Column "Name" gtypeString TextColumn (toGValue . Just . ccdLabel),
    Column "Entries" gtypeInt TextColumn (toGValue . ccdEntries),
    Column "Individual Time" gtypeDouble PercentColumn (toGValue . ccdTimeIndividual),
    Column "Individual Alloc" gtypeDouble PercentColumn (toGValue . ccdAllocIndividual),
    Column "Inherited Time" gtypeDouble PercentColumn (toGValue . ccdTimeInherited),
    Column "Inherited Alloc" gtypeDouble PercentColumn (toGValue . ccdAllocInherited),
    Column "Module" gtypeString TextColumn (toGValue . Just . ccdModule),
    Column "Source" gtypeString TextColumn (toGValue . Just . ccdSource)
  ]
      

