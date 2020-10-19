module Main (main) where

import qualified Prelude as P (IO, print, unwords, ($))

-- | Hue hue Comment

main :: P.IO ()
main = do
  P.print P.$ P.unwords ["Hello", "Friend"]
