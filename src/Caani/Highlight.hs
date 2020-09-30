{-# LANGUAGE OverloadedStrings #-}

module Caani.Highlight
    ( highlightHaskell,
      ColorWord (..),
    )
where

import Codec.Picture
import Data.Bits (Bits ((.&.), complement, shiftR))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Vector ((!), Vector, empty, fromList)
import qualified GHC.SyntaxHighlighter as GHC

data ColorWord = ColorWord T.Text (Float, Float, Float) deriving (Show)

-- colors in HSV (0-360, 0-1, 0-1)
colorFromToken GHC.KeywordTok = (319, 1, 1) -- #FF00AE
colorFromToken GHC.PragmaTok = (319, 0.11, 0.89) -- #E3CADB
colorFromToken GHC.SymbolTok = (43, 1, 0.94) -- #F0A800
colorFromToken GHC.VariableTok = (248, 0.32, 0.94) -- #ADA3F0
colorFromToken GHC.ConstructorTok = (181, 0.84, 0.94) -- #26ECF0
colorFromToken GHC.OperatorTok = (21, 0.84, 0.94) -- #EF6D26
colorFromToken GHC.CharTok = (60, 0.84, 0.94) -- #EFEF26
colorFromToken GHC.StringTok = (43, 0.84, 0.94) -- #EFB626
colorFromToken GHC.RationalTok = (194, 0.84, 0.94) -- #26C0EF
colorFromToken GHC.CommentTok = (184, 0.04, 0.82) -- #C9D1D1
-- colorFromToken GHC.SpaceTok = (330,1,0.94)
colorFromToken _ = (330, 1, 0.94) -- #F00078

colorWordFromToken (tok, text) = ColorWord text (colorFromToken tok)

splitBy :: (ColorWord -> Bool) -> [ColorWord] -> [[ColorWord]]
splitBy _ [] = []
splitBy p list = line:(splitBy p list')
    where
        (line, rest) = span p list
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

breakLines = splitBy (\(ColorWord t _) -> not $ T.any (== '\n') t)

highlightHaskell = fmap breakLines . fmap (fmap colorWordFromToken) . GHC.tokenizeHaskell
