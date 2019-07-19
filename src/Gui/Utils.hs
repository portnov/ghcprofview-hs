
module Gui.Utils where

import qualified Data.Text as T

import Data.GI.Base.GValue
import GI.Gtk hiding (main)

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

