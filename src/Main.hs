{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main (main) where

import qualified Data.Text as T () -- instances only

import qualified GI.Gtk as GI (main, init)
import GI.Gtk hiding (main)

import System.Environment

import Types
import Operations
import Loader
import Gui
import Gui.TreeWidget
import Gui.Utils

main :: IO ()
main = do
  [path] <- getArgs
  treeData <- loadProfile path
  let treeData' = updateTotals $ filterCcd (not . ccdToIgnore) treeData
  print $ profileTotalTicks $ ccdProfile treeData'
  printTree treeData'

  GI.init Nothing

  -- Create a new window
  window <- windowNew WindowTypeToplevel

  -- Here we connect the "destroy" event to a signal handler.
  onWidgetDestroy window mainQuit

  -- Sets the border width of the window.
  setContainerBorderWidth window 10

  vbox <- boxNew OrientationVertical 10
  hbox <- buttonBoxNew OrientationHorizontal

  entry <- searchEntryNew
  boxPackStart hbox entry True True 0
  searchButton <- buttonNewWithLabel "Search"
  boxPackStart hbox searchButton False False 0
  boxPackStart vbox hbox False False 0

  tree <- mkTreeView treeWidgetConfig treeData'
  treeViewSetSearchColumn tree 1
  treeViewSetEnableSearch tree False
  let noAdjustment = Nothing :: Maybe Adjustment
  scroll <- scrolledWindowNew noAdjustment noAdjustment
  containerAdd scroll tree
  boxPackStart vbox scroll True True 0

  on searchButton #clicked $ do
    text <- entryGetText entry
    mbIter <- locate tree text
    case mbIter of
      Nothing -> print "Not found"
      Just iter -> do
        print "found"
        Just store <- treeViewGetModel tree
        path <- treeModelGetPath store iter
        treeViewExpandToPath tree path
        treeViewSetCursor tree path (Nothing :: Maybe TreeViewColumn) False

  setContainerChild window vbox

  -- The final step is to display everything (the window and all the widgets
  -- contained within it)
  widgetShowAll window

  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  GI.main

