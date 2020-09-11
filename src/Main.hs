{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import Caani (caaniFromFile, CaaniConfig(..))
import System.Environment (getExecutablePath)

main :: IO ()
main = do
  caaniFromFile "code" (CaaniConfig {
    boundary = (200, 1000),
    fontPath = "./fonts/FiraCode-Medium.ttf",
    tagPath = "./haskell-flag.png",
    code = "",
    outPath = "caani-c.png"
  })  