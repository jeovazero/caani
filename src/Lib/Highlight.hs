{-# LANGUAGE OverloadedStrings #-}
module Lib.Highlight (highlightHaskell, ColorWord(..)) where

import Codec.Picture
import Data.Bits (Bits(complement, shiftR, (.&.)))
import Data.Vector (empty, (!), Vector, fromList)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified GHC.SyntaxHighlighter as GHC

data ColorWord = ColorWord T.Text (Float,Float,Float) deriving (Show)

colorFromToken GHC.KeywordTok = (319,1,1)
colorFromToken GHC.PragmaTok = (319,0.11,0.89)
colorFromToken GHC.SymbolTok = (43,1,0.94)
colorFromToken GHC.VariableTok = (248,0.32,0.94)
colorFromToken GHC.ConstructorTok = (181,0.84,0.94)
colorFromToken GHC.OperatorTok = (21,0.84,0.94)
colorFromToken GHC.CharTok = (60,0.84,0.94)
colorFromToken GHC.StringTok = (43,0.84,0.94)
colorFromToken GHC.RationalTok = (194,0.84,0.94)
colorFromToken GHC.CommentTok = (184,0.04,0.82)
-- colorFromToken GHC.SpaceTok = (330,1,0.94)
colorFromToken _ = (330,1,0.94)

colorWordFromToken (tok, text) = ColorWord text (colorFromToken tok)

splitBy :: (ColorWord -> Bool) -> [ColorWord] -> [[ColorWord]]
splitBy _ [] = []
splitBy p list = line:(splitBy p list')
  where
    (line,rest) = span p list
    dropSpaces (_:_:xs) = xs
    dropSpaces _ = []
    spaceToCW "" = " "
    spaceToCW s = s
    list' =
      case rest of
        [] -> []
        (ColorWord s c:xs) ->
          case T.tail s of
            "" -> xs
            s' -> (ColorWord s' c):xs

breakLines = splitBy (\(ColorWord t _) -> not $ T.any  (== '\n') t)

highlightHaskell = fmap breakLines . fmap (fmap colorWordFromToken) . GHC.tokenizeHaskell