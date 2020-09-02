{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import Caani (caaniFromFile)
import System.Environment (getExecutablePath)

main :: IO ()
main = do
  print $ show __FILE__
  e <- getExecutablePath
  print e
  caaniFromFile "./fonts/FiraCode-Medium.ttf" "./haskell-flag.png" "code" "caani-a.png"