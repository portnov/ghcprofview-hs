{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Gui.Utils where

import Control.Monad
import qualified Data.Text as T
import Data.Int
import Data.IORef
import Text.Regex.TDFA
import Text.Regex.TDFA.Text () -- instances only

import Data.GI.Base.GValue
import GI.Gtk hiding (main)

import Types

iterChildren :: TreeModel
             -> TreeIter  -- ^ Root
             -> (TreeIter -> IO (Bool, [a])) -- ^ Should return (whether to stop iterations; result)
             -> IO [a]
iterChildren store parent func = do
    (hasFirst, first) <- treeModelIterChildren store (Just parent)
    if not hasFirst
      then return []
      else go first
  where
    go iter = do
      (stop, results) <- func iter
      if stop
        then return results
        else do
          hasNext <- treeModelIterNext store iter
          if hasNext
            then do
                 rest <- go iter
                 return $ results ++ rest
            else return results

iterChildrenR :: TreeModel -> TreeIter -> (TreeIter -> IO (Bool, [a])) -> IO [a]
iterChildrenR store parent func = do
  childResults <- iterChildren store parent $ \child -> do
        (stop, result) <- func child
        if stop
          then return (True, [result])
          else do
               rest <- iterChildrenR store child func
               return (False, [result ++ rest])
  return $ concat childResults

treeSearch :: TreeView -> SearchMetohd -> T.Text -> IO [TreePath]
treeSearch view method needle = do
    Just store <- treeViewGetModel view
    (hasFirst, first) <- treeModelGetIterFirst store
    if not hasFirst
      then return []
      else iterChildrenR store first $ \child -> do
              found <- checkValue store child
              if found
                then do
                  path <- treeModelGetPath store child
                  return (False, [path])
                else return (False, [])
  where
    checkValue store row = do
      mbValue <- fromGValue =<< treeModelGetValue store row 1
      case mbValue of
        Nothing -> return False -- not ok
        Just value ->
          case method of
            Contains -> return $ needle `T.isInfixOf` value
            Exact -> return $ needle == value
            Regexp -> return $ value =~ needle

treeCheck :: TreeModel -> (TreeIter -> IO Bool) -> IO Bool
treeCheck store check = or <$> do
    (hasFirst, first) <- treeModelGetIterFirst store
    if not hasFirst
      then return []
      else iterChildrenR store first $ \child -> do
              found <- check child
              if found
                then return (True, [True])
                else return (False, [])

withSelected :: TreeView -> (TreeModel -> TreeIter -> IO ()) -> IO ()
withSelected tree fn = do
    (isSelected, store, selected) <- treeSelectionGetSelected =<< treeViewGetSelection tree
    when isSelected $ fn store selected

getTruePath :: TreeModel -> TreeIter -> IO TreePath
getTruePath top iter = do
  Just sorted <- castTo TreeModelSort top
  topPath <- treeModelGetPath top iter
  Just truePath <- treeModelSortConvertPathToChildPath sorted topPath
  return truePath

defFilterParams :: FilterParams
defFilterParams = FilterParams 0 0 0 0 0 "" ""

getItem :: IsGValue a => TreeModel -> TreeIter -> Int32 -> IO a
getItem store row col = 
    fromGValue =<< treeModelGetValue store row col

treeFilterFunc :: IORef FilterParams -> TreeModelFilterVisibleFunc
treeFilterFunc paramsRef store row = do
    params <- readIORef paramsRef
    hasChild <- treeModelIterHasChild store row
    good <- do
        entries <- getItem store row 2 :: IO Integer
        timeIndividual <- getItem store row 3
        allocIndividual <- getItem store row 4
        timeInherited <- getItem store row 5
        allocInherited <- getItem store row 6
        Just mod <- getItem store row 7
        Just src <- getItem store row 8
        return $
            entries >= fpEntries params &&
            timeIndividual >= fpTimeIndividual params &&
            allocIndividual >= fpAllocIndividual params &&
            timeInherited >= fpTimeInherited params &&
            allocInherited >= fpAllocInherited params &&
            fpModule params `T.isInfixOf` mod &&
            fpSource params `T.isInfixOf` src
    return $ hasChild || good

mkComboBox :: (Show a) => [(a, T.Text)] -> IO ComboBoxText
mkComboBox pairs = do
  combo <- comboBoxTextNew
  forM_ pairs $ \(value, title) -> do
    let id = T.pack (show value)
    comboBoxTextAppend combo (Just id) title
  comboBoxSetActive combo 0
  return combo

mkMenuItem :: Menu -> T.Text -> MenuItemActivateCallback -> IO ()
mkMenuItem menu label callback = do
  item <- menuItemNewWithLabel label
  menuShellAppend menu item
  on item #activate callback
  widgetShow item
  return ()

mkTabLabelWidget :: T.Text -> ButtonClickedCallback -> IO Box
mkTabLabelWidget text callback = do
    label <- labelNew (Just text)
    let size = fromIntegral $ fromEnum IconSizeMenu
    button <- buttonNewFromIconName (Just "window-close") size
    on button #clicked callback
    buttonSetRelief button ReliefStyleNone
    box <- boxNew OrientationHorizontal 0
    boxPackStart box label True True 0
    boxPackStart box button False False 0
    widgetShowAll box
    return box

