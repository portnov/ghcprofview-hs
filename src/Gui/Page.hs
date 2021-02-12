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

mkContextMenu :: TreeView -> CostCentreData -> ShowTree -> IO Menu
mkContextMenu tree ccd showTree = do
  menu <- menuNew

--   mkMenuItem menu "Test" $ do
--     withSelected tree $ \store selected -> do
--       Just name <- getItem store selected nameColumn
--       print (name :: T.Text)
--       Just mod <- getItem store selected moduleColumn
--       Just src <- getItem store selected sourceColumn
--       let subtrees = ccdFind mod src name ccd
--       forM_ subtrees $ \child -> do
--         let parent = case ccdParent child of
--                        Nothing -> "no parent"
--                        Just parent -> T.pack (ccdRecordIds parent) <> ": " <> ccdLabel parent <> " = " <> T.pack (show $ ccdTimeInherited parent)
--         print $ T.pack (ccdRecordIds child) <> ": " <> ccdLabel child <> " = " <> T.pack (show $ ccdTimeInherited child) <> " => " <> parent

  mkMenuItem menu "Narrow view to this item" $ do
    withSelected tree $ \store selected -> do
      path <- getTruePath store selected
      Just idxs <- treePathGetIndices path
      case ccdByPath idxs ccd of
        Nothing -> return ()
        Just child -> do
          let label = ccdLabel child
          showTree ("Narrowed view: " <> label) child

  mkMenuItem menu "Group all outgoing calls" $
    withSelected tree $ \store selected -> do
      Just name <- getItem store selected nameColumn
      Just mod <- getItem store selected moduleColumn
      Just src <- getItem store selected sourceColumn
      let subtrees = ccdFind mod src name ccd
          result = ccdSum subtrees
      showTree ("Calls of " <> name) result

  mkMenuItem menu "Group all incoming calls" $
    withSelected tree $ \store selected -> do
      Just name <- getItem store selected nameColumn
      Just mod <- getItem store selected moduleColumn
      Just src <- getItem store selected sourceColumn
      let subtrees = ccdFindIncoming mod src name ccd
          result = ccdSum subtrees
      showTree ("Calls to " <> name) result

  return menu

mkPage :: Statusbar -> T.Text -> CostCentreData -> ShowTree -> IO Page
mkPage status label ccd showTree = do
  vbox <- boxNew OrientationVertical 0
  searchHbox <- boxNew OrientationHorizontal 0
  filterBox <- boxNew OrientationHorizontal 0
  filterSettingsBox <- flowBoxNew
  flowBoxSetSelectionMode filterSettingsBox SelectionModeNone

  entry <- searchEntryNew
  boxPackStart searchHbox entry True True 0
  searchButton <- buttonNewWithLabel "Search"
  searchNextButton <- buttonNewWithLabel "Next"
  searchMethodCombo <- mkComboBox [
                           (Contains, "Contains")
                         , (Exact, "Exact")
                         , (Regexp, "Reg.Exp")
                       ]

  boxPackStart searchHbox searchButton False False 0
  boxPackStart searchHbox searchNextButton False False 0
  boxPackStart searchHbox searchMethodCombo False False 0
  boxPackStart vbox searchHbox False False 0

  on entry #activate $ buttonClicked searchButton

  let mkEntry :: IsWidget w => T.Text -> w -> IO (Box, w)
      mkEntry name widget = do
        lbl <- labelNew (Just name)
        box <- boxNew OrientationHorizontal 0
        boxPackStart box lbl False False 0
        boxPackStart box widget True True 10
        return (box, widget)

  let addFilterPercent name = do
        (box, spin) <- mkEntry name =<< spinButtonNewWithRange 0 100 1
        spinButtonSetDigits spin 2
        containerAdd filterSettingsBox box
        return spin

  let addFilterNumber name = do
        (box, spin) <- mkEntry name =<< spinButtonNewWithRange 0 (1e38) 1
        spinButtonSetDigits spin 0
        containerAdd filterSettingsBox box
        return spin

  let addFilterText name = do
        (box, spin) <- mkEntry name =<< entryNew
        containerAdd filterSettingsBox box
        return entry

  fltrEntries <- addFilterNumber "Entries:"
  fltrTimeIndividual <- addFilterPercent "Time Individual:"
  fltrAllocIndividual <- addFilterPercent "Alloc Individual:"
  fltrTimeInherited <- addFilterPercent "Time Inherited:"
  fltrAllocInherited <- addFilterPercent "Alloc Inherited:"
  fltrModule <- addFilterText "Module:"
  fltrSource <- addFilterText "Source:"

  filterButton <- buttonNewWithLabel "Filter"

  boxPackStart filterBox filterSettingsBox True True 0
  boxPackStart filterBox filterButton False False 0
  boxPackStart vbox filterBox False False 0

  tree <- mkTreeView treeWidgetConfig ccd
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
      Just methodId <- comboBoxGetActiveId searchMethodCombo
      let method = read $ T.unpack methodId
      results <- treeSearch tree method text
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
    entries <- spinButtonGetValueAsInt fltrEntries
    timeIndividual <- spinButtonGetValue fltrTimeIndividual
    allocIndividual <- spinButtonGetValue fltrAllocIndividual
    timeInherited <- spinButtonGetValue fltrTimeInherited
    allocInherited <- spinButtonGetValue fltrAllocInherited
    mod <- entryGetText fltrModule
    src <- entryGetText fltrSource

    let params = FilterParams {
          fpEntries = fromIntegral entries
        , fpTimeIndividual = timeIndividual
        , fpAllocIndividual = allocIndividual
        , fpTimeInherited = timeInherited
        , fpAllocInherited = allocInherited
        , fpModule = mod
        , fpSource = src
      }
    let ccd' = filterCcdRecursive (checkFilter params) ccd
    showTree "Filtered" ccd'

  on tree #buttonPressEvent $ \ev -> do
    button <- get ev #button
    when (button == 3) $ do
      menu <- mkContextMenu tree ccd showTree
      menuPopupAtPointer menu Nothing
    return False

  return $ Page vbox searchResults

