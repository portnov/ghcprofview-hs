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
      path <- getTruePath store selected
      Just idxs <- treePathGetIndices path
      case ccdByPath idxs ccd of
        Nothing -> return ()
        Just child -> do
          let label = ccdLabel child
          showTree ("Focus: " <> label) child

  mkItem menu "Group outgoing calls" $
    withSelected tree $ \store selected -> do
      Just name <- fromGValue =<< treeModelGetValue store selected 1
      Just mod <- fromGValue =<< treeModelGetValue store selected 7
      Just src <- fromGValue =<< treeModelGetValue store selected 8
      let subtrees = ccdFind mod src name ccd
          result = ccdSum subtrees
      showTree ("Outgoing calls: " <> name) result
      
  return menu

mkPage :: Statusbar -> T.Text -> CostCentreData -> ShowTree -> IO Page
mkPage status label ccd showTree = do
  vbox <- boxNew OrientationVertical 0
  searchHbox <- boxNew OrientationHorizontal 0
  filterBox <- boxNew OrientationHorizontal 0

  entry <- searchEntryNew
  boxPackStart searchHbox entry True True 0
  searchButton <- buttonNewWithLabel "Search"
  searchNextButton <- buttonNewWithLabel "Next"
  boxPackStart searchHbox searchButton False False 0
  boxPackStart searchHbox searchNextButton False False 0
  boxPackStart vbox searchHbox False False 0

  fltrTimeInherited <- spinButtonNewWithRange 0 100 1
  filterButton <- buttonNewWithLabel "Filter"
  resetFilterButton <- buttonNewWithLabel "Reset"

  boxPackStart filterBox fltrTimeInherited True True 0
  boxPackStart filterBox filterButton False False 0
  boxPackStart filterBox resetFilterButton False False 0
  boxPackStart vbox filterBox False False 0

  filterParams <- newIORef 0
  (tree, filtered) <- mkTreeView (treeWidgetConfig filterParams) ccd
  treeViewSetSearchColumn tree 1
  treeViewSetEnableSearch tree False
  let noAdjustment = Nothing :: Maybe Adjustment
  scroll <- scrolledWindowNew noAdjustment noAdjustment
  containerAdd scroll tree
  boxPackStart vbox scroll True True 10

  statusContext <- statusbarGetContextId status label

  searchResults <- newIORef (0, [])

  let message text =
        void $ statusbarPush status statusContext (T.pack text)

  on searchButton #clicked $ do
    text <- entryGetText entry
    unless (T.null text) $ do
      results <- treeSearch tree text
      if null results
        then message "Not found."
        else do
          message $ "Found: " ++ show (length results)
          writeIORef searchResults (0, results)
          Just store <- treeViewGetModel tree
          let path = head results
          treeViewExpandToPath tree path
          treeViewSetCursor tree path (Nothing :: Maybe TreeViewColumn) False

  on searchNextButton #clicked $ do
    (prevIndex, results) <- readIORef searchResults
    if null results
      then message "Not found."
      else do
        let n = length results
            index = (prevIndex + 1) `mod` n
            path = results !! index
        message $ "Found: " ++ show index ++ "/" ++ show n
        writeIORef searchResults (index, results)
        treeViewExpandToPath tree path
        treeViewSetCursor tree path (Nothing :: Maybe TreeViewColumn) False

  on filterButton #clicked $ do
    bound <- spinButtonGetValue fltrTimeInherited
    writeIORef filterParams bound
    treeModelFilterRefilter filtered

  on resetFilterButton #clicked $ do
    writeIORef filterParams 0
    treeModelFilterRefilter filtered

  on tree #buttonPressEvent $ \ev -> do
    button <- get ev #button
    when (button == 3) $ do
      menu <- mkContextMenu tree ccd showTree
      menuPopupAtPointer menu Nothing
    return False

  return $ Page vbox searchResults

