{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main (main) where

import qualified GI.Gtk as GI (main, init)
import GI.Gtk hiding (main)

import System.Environment

import Operations
import Loader
import Gui.Page

main :: IO ()
main = do
  [path] <- getArgs
  treeData <- loadProfile path
  let treeData' = updateTotals $ filterCcd (not . ccdToIgnore) treeData
--   print $ profileTotalTicks $ ccdProfile treeData'
--   printTree treeData'
--   print $ ccdLabel `fmap` ccdByPath [0, 20] treeData'

  GI.init Nothing

  -- Create a new window
  window <- windowNew WindowTypeToplevel

  -- Here we connect the "destroy" event to a signal handler.
  onWidgetDestroy window mainQuit

  -- Sets the border width of the window.
  setContainerBorderWidth window 10
  vbox <- boxNew OrientationVertical 0

  notebook <- notebookNew
  status <- statusbarNew

  let showTree label ccd = do
        page <- pageWidget `fmap` mkPage status label ccd showTree
        widgetShowAll page
        notebookAppendPage notebook page noWidget
        notebookSetTabLabelText notebook page label

  showTree "All" treeData'

  boxPackStart vbox notebook True True 0
  boxPackStart vbox status False False 0
  setContainerChild window vbox


  -- The final step is to display everything (the window and all the widgets
  -- contained within it)
  widgetShowAll window

  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  GI.main

