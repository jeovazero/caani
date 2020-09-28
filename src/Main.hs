{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import Caani (caani, CaaniConfig(..))
import System.Environment (lookupEnv, getArgs)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

getResourceDir = fmap (fromMaybe "./resources") (lookupEnv "RESOURCE_DIR")

main :: IO ()
main = do
  input <- TIO.getContents
  args <- getArgs
  resourceDir <- getResourceDir
  r <- caani (CaaniConfig {
    boundary = (200, 1000),
    fontPath = resourceDir ++ "/FiraCode-Medium.ttf",
    tagPath = resourceDir ++ "/haskell-flag.png",
    code = input,
    outPath = "./caani-out.png"
  })
  case r of
    Right () -> TIO.putStrLn "Success"  
    Left err -> TIO.putStrLn . T.append "Error: " $ T.pack . show $ err