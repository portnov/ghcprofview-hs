{-# LANGUAGE OverloadedStrings #-}

module Gui where

import qualified Data.Text as T () -- instances only
import Data.Int

import Data.GI.Base.GType
import Data.GI.Base.GValue

import Types
import Operations
import Gui.TreeWidget

treeWidgetConfig :: TreeWidgetConfig CostCentreData
treeWidgetConfig =
  TreeWidgetConfig {
      twcColumns = [
          Column "No" gtypeString TextColumn (toGValue . Just . ccdRecordIds),                -- 0
          Column "Name" gtypeString TextColumn (toGValue . Just . ccdLabel),                  -- 1
          Column "Entries" gtypeInt64 TextColumn (toGValue . ccdEntries),                     -- 2
          Column "Individual Time" gtypeDouble PercentColumn (toGValue . ccdTimeIndividual),  -- 3
          Column "Individual Alloc" gtypeDouble PercentColumn (toGValue . ccdAllocIndividual), -- 4
          Column "Inherited Time" gtypeDouble PercentColumn (toGValue . ccdTimeInherited),     -- 5
          Column "Inherited Alloc" gtypeDouble PercentColumn (toGValue . ccdAllocInherited),   -- 6
          Column "Relative Time" gtypeDouble PercentColumn (toGValue . ccdTimeRelative),       -- 7
          Column "Relative Alloc" gtypeDouble PercentColumn (toGValue . ccdAllocRelative),     -- 8
          Column "Module" gtypeString TextColumn (toGValue . Just . ccdModule),                -- 9
          Column "Source" gtypeString TextColumn (toGValue . Just . ccdSource)                 -- 10
        ]
  }

noColumn :: Int32
noColumn = 0

nameColumn :: Int32
nameColumn = 1

entriesColumn :: Int32
entriesColumn = 2

individualTimeColumn :: Int32
individualTimeColumn = 3

individualAllocColumn :: Int32
individualAllocColumn = 4

inheritedTimeColumn :: Int32
inheritedTimeColumn = 5

inheritedAllocColumn :: Int32
inheritedAllocColumn = 6

relativeTimeColumn :: Int32
relativeTimeColumn = 7

relativeAllocColumn :: Int32
relativeAllocColumn = 8

moduleColumn :: Int32
moduleColumn = 9

sourceColumn :: Int32
sourceColumn = 10

