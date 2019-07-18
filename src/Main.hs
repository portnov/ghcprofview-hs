{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main (main) where

import Control.Monad

import Control.Monad
import Data.Tree as Tree
import Data.Int
import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.GI.Base.GType
import Data.GI.Base.GValue
import qualified GI.Gtk as GI (main, init)
import GI.Gtk hiding (main)
import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ButtonBoxStyle(..))

import System.Environment

import Types
import Operations
import Json
import TreeWidget

treeWidgetConfig :: TreeWidgetConfig CostCentreData
treeWidgetConfig = TreeWidgetConfig 
  [
    Column "No" gtypeInt TextColumn (toGValue . ccdRecordId),
    Column "Name" gtypeString TextColumn (toGValue . Just . ccdLabel),
    Column "Entries" gtypeInt TextColumn (toGValue . ccdEntries),
    Column "Individual Time" gtypeDouble PercentColumn (toGValue . ccdTimeIndividual),
    Column "Individual Alloc" gtypeDouble PercentColumn (toGValue . ccdAllocIndividual),
    Column "Inherited Time" gtypeDouble PercentColumn (toGValue . ccdTimeInherited),
    Column "Inherited Alloc" gtypeDouble PercentColumn (toGValue . ccdAllocInherited),
    Column "Module" gtypeString TextColumn (toGValue . Just . ccdModule),
    Column "Source" gtypeString TextColumn (toGValue . Just . ccdSource)
  ]
      

loadJson :: FilePath -> IO CostCentreData
loadJson path = do
  r <- eitherDecodeFileStrict path
  case r of
    Left err -> fail err
    Right profile -> return $ resolveProfile profile

iterChildren :: TreeModel -> TreeIter -> (TreeIter -> IO (Maybe a)) -> IO (Maybe a)
iterChildren store parent func = do
    (hasFirst, first) <- treeModelIterChildren store (Just parent)
    if not hasFirst
      then return Nothing
      else go first
  where
    go iter = do
      mbResult <- func iter
      case mbResult of
        Just result -> return (Just result)
        Nothing -> do
          hasNext <- treeModelIterNext store iter
          if hasNext
            then go iter
            else return Nothing

iterChildrenR :: TreeModel -> TreeIter -> (TreeIter -> IO (Maybe a)) -> IO (Maybe a)
iterChildrenR store parent func =
  iterChildren store parent $ \child -> do
    mbResult <- func child
    case mbResult of
      Just result -> return (Just result)
      Nothing -> iterChildrenR store child func

treeSearch :: TreeView -> TreeViewSearchEqualFunc
treeSearch view store columnId needle row = do
    found <- checkValue row
    if found
      then return False -- found
      else do
        mbResult <- iterChildrenR store row $ \child -> do
            found <- checkValue child
            if found
              then do
                treeViewExpandToPath view =<< treeModelGetPath store row
                return (Just True)  -- found
              else return Nothing
        case mbResult of
          Just result -> return $ not result
          Nothing -> do
            treeViewCollapseRow view =<< treeModelGetPath store row
            return True -- not found
  where
    checkValue row = do
      mbValue <- fromGValue =<< treeModelGetValue store row columnId
      case mbValue of
        Nothing -> return False -- not ok
        Just value -> return $ needle `T.isInfixOf` value

locate :: TreeView -> T.Text -> IO (Maybe TreeIter)
locate view needle = do
    Just store <- treeViewGetModel view
    (hasFirst, first) <- treeModelGetIterFirst store
    if not hasFirst
      then return Nothing
      else iterChildrenR store first $ \child -> do
              found <- checkValue store child
              if found
                then return (Just child)
                else return Nothing
  where
    checkValue store row = do
      mbValue <- fromGValue =<< treeModelGetValue store row 1
      case mbValue of
        Nothing -> return False -- not ok
        Just value -> return $ needle `T.isInfixOf` value

printTree :: CostCentreData -> IO ()
printTree node = go 0 node
  where
    go i node = do
      let prefix = T.replicate i " "
      TIO.putStrLn $ prefix <>
          ccdLabel node <> "\t"
          <> T.pack (show $ ccdTicksInherited node) <> "\t"
          <> T.pack (show $ ccdTimeInherited node)
      forM_ (ccdChildren node) $ \child ->
        go (i+1) child

main :: IO ()
main = do
  [path] <- getArgs
  treeData <- loadJson path
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
  treeViewSetSearchEqualFunc tree (treeSearch tree)
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

