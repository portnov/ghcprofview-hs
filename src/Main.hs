{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main (main) where

import Control.Monad
import qualified Data.Text as T
import Data.IORef

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
--   printTree treeData'

  GI.init Nothing

  -- Create a new window
  window <- windowNew WindowTypeToplevel

  -- Here we connect the "destroy" event to a signal handler.
  onWidgetDestroy window mainQuit

  -- Sets the border width of the window.
  setContainerBorderWidth window 10

  vbox <- boxNew OrientationVertical 10
  hbox <- boxNew OrientationHorizontal 10

  entry <- searchEntryNew
  boxPackStart hbox entry True True 0
  searchButton <- buttonNewWithLabel "Search"
  searchNextButton <- buttonNewWithLabel "Next"
  boxPackStart hbox searchButton False False 0
  boxPackStart hbox searchNextButton False False 0
  boxPackStart vbox hbox False False 0

  tree <- mkTreeView treeWidgetConfig treeData'
  treeViewSetSearchColumn tree 1
  treeViewSetEnableSearch tree False
  let noAdjustment = Nothing :: Maybe Adjustment
  scroll <- scrolledWindowNew noAdjustment noAdjustment
  containerAdd scroll tree
  boxPackStart vbox scroll True True 0

  searchResults <- newIORef (0, [])

  on searchButton #clicked $ do
    text <- entryGetText entry
    unless (T.null text) $ do
      results <- treeSearch tree text
      if null results
        then print "not found"
        else do
          print $ "found: " ++ show (length results)
          writeIORef searchResults (0, results)
          Just store <- treeViewGetModel tree
          let path = head results
          treeViewExpandToPath tree path
          treeViewSetCursor tree path (Nothing :: Maybe TreeViewColumn) False

  on searchNextButton #clicked $ do
    (prevIndex, results) <- readIORef searchResults
    if null results
      then print "not found"
      else do
        let n = length results
            index = (prevIndex + 1) `mod` n
            path = results !! index
        print $ "found: " ++ show index ++ "/" ++ show n
        writeIORef searchResults (index, results)
        treeViewExpandToPath tree path
        treeViewSetCursor tree path (Nothing :: Maybe TreeViewColumn) False

  setContainerChild window vbox

  -- The final step is to display everything (the window and all the widgets
  -- contained within it)
  widgetShowAll window

  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  GI.main

