{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Gui.Page where

import Control.Monad
import qualified Data.Text as T
import Data.IORef

import GI.Gtk hiding (main)

import Gui
import Gui.TreeWidget
import Gui.Utils

import Types
import Operations

data Page = Page {
    pageWidget :: Box
  , pageSearchState :: IORef (Int, [TreePath])
  }

type ShowTree = T.Text -> CostCentreData -> IO ()

mkItem :: Menu -> T.Text -> MenuItemActivateCallback -> IO ()
mkItem menu label callback = do
  item <- menuItemNewWithLabel label
  menuShellAppend menu item
  on item #activate callback
  widgetShow item
  return ()

mkContextMenu :: TreeView -> CostCentreData -> ShowTree -> IO Menu
mkContextMenu tree ccd showTree = do
  menu <- menuNew
  mkItem menu "Test" $ do
    withSelected tree $ \store selected -> do
      Just value <- fromGValue =<< treeModelGetValue store selected 1
      print (value :: T.Text)

  mkItem menu "Focus" $ do
    withSelected tree $ \store selected -> do
      path <- treeModelGetPath store selected
      Just idxs <- treePathGetIndices path
      case ccdByPath idxs ccd of
        Nothing -> return ()
        Just child -> do
          let label = ccdLabel child
          showTree ("Focus: " <> label) child
      
  return menu

mkPage :: CostCentreData -> ShowTree -> IO Page
mkPage ccd showTree = do
  vbox <- boxNew OrientationVertical 10
  hbox <- boxNew OrientationHorizontal 0

  entry <- searchEntryNew
  boxPackStart hbox entry True True 0
  searchButton <- buttonNewWithLabel "Search"
  searchNextButton <- buttonNewWithLabel "Next"
  boxPackStart hbox searchButton False False 0
  boxPackStart hbox searchNextButton False False 0
  boxPackStart vbox hbox False False 0

  tree <- mkTreeView treeWidgetConfig ccd
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

  on tree #buttonPressEvent $ \ev -> do
    button <- get ev #button
    when (button == 3) $ do
      menu <- mkContextMenu tree ccd showTree
      menuPopupAtPointer menu Nothing
    return False

  return $ Page vbox searchResults

