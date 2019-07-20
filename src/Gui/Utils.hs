
module Gui.Utils where

import Control.Monad
import qualified Data.Text as T
import Data.IORef

import Data.GI.Base.GValue
import GI.Gtk hiding (main)
import GI.Gtk.Structs.TreePath

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

treeSearch :: TreeView -> T.Text -> IO [TreePath]
treeSearch view needle = do
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
        Just value -> return $ needle `T.isInfixOf` value

withSelected :: TreeView -> (TreeModel -> TreeIter -> IO ()) -> IO ()
withSelected tree fn = do
    (isSelected, store, selected) <- treeSelectionGetSelected =<< treeViewGetSelection tree
    when isSelected $ fn store selected

getTruePath :: TreeModel -> TreeIter -> IO TreePath
getTruePath top iter = do
  Just sorted <- castTo TreeModelSort top
  Just filtered <- castTo TreeModelFilter =<< treeModelSortGetModel sorted

  topPath <- treeModelGetPath top iter

  Just filteredPath <- treeModelSortConvertPathToChildPath sorted topPath
  Just truePath <- treeModelFilterConvertPathToChildPath filtered filteredPath

  return truePath

type FilterParams = Double

treeFilterFunc :: IORef FilterParams -> TreeModelFilterVisibleFunc
treeFilterFunc paramsRef store row = do
    bound <- readIORef paramsRef
    time <- fromGValue =<< treeModelGetValue store row 5
    return (time >= bound)

