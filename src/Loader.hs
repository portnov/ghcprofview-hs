
module Loader where

import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.Text.IO as TIO
import qualified GHC.Prof as P -- from ghc-prof package

import Types
import Converter
import Operations
import Json

loadProfile :: FilePath -> IO CostCentreData
loadProfile path = do
  r <- eitherDecodeFileStrict path
  case r of
    Left _ -> do
      text <- TIO.readFile path
      let r = P.decode' text
      case r of
        Left err -> fail err
        Right profile -> do 
          let Just centres = P.costCentres profile
          return $ convertCc profile centres

    Right profile -> return $ resolveProfile profile

