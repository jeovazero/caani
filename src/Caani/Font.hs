module Caani.Font (Glyph (..), loadChar, loadFontFace, FontFace, FreeTypeError (..)) where

import Control.Exception.Base (Exception)
import qualified Data.Vector as Vec (Vector, empty, fromList)
import Foreign (Word8, Storable (peek))
import Foreign.Marshal (peekArray)
import qualified FreeType.Core.Base as FT_Base
import qualified FreeType.Core.Types as FT_Type

data FreeTypeError = FreeTypeError String
    deriving (Show, Eq)

instance Exception FreeTypeError

type FontFace = FT_Base.FT_Face

newFontFace :: FT_Base.FT_Library -> String -> IO FontFace
newFontFace ftLib filepath = FT_Base.ft_New_Face ftLib filepath 0

ubyteToInt :: Word8 -> Int
ubyteToInt u = if u < 0 then 256 + iu else iu
    where
        iu = fromIntegral u

data Glyph = Glyph
    { gWidth :: Int,
      gHeight :: Int,
      gLeft :: Int,
      gTop :: Int,
      gBaseWidth :: Int,
      gBuffer :: Vec.Vector Int
    }

loadChar :: FT_Base.FT_Face -> Int -> Char -> IO Glyph
loadChar _ base ' ' = pure $ Glyph 0 0 0 0 base Vec.empty
loadChar _ base '\t' = pure $ Glyph 0 0 0 0 (2 * base) Vec.empty
loadChar face base chr = do
    charCode <- FT_Base.ft_Get_Char_Index face $ fromIntegral $ fromEnum chr
    FT_Base.ft_Load_Glyph face charCode 0
    faceRec <- peek face    
    let slot = FT_Base.frGlyph faceRec
    FT_Base.ft_Render_Glyph slot FT_Base.FT_RENDER_MODE_NORMAL
    slotRec <- peek slot
    let l = FT_Base.gsrBitmap_left slotRec
    let t = FT_Base.gsrBitmap_top slotRec
    let bitmap = FT_Base.gsrBitmap slotRec
    let w = fromIntegral $ FT_Type.bWidth bitmap
        h = fromIntegral $ FT_Type.bRows bitmap
        arrSize = w * h
    arr <- peekArray arrSize $ FT_Type.bBuffer bitmap
    let vec = Vec.fromList $ fmap ubyteToInt arr
    pure $ Glyph w h (fromIntegral l) (fromIntegral t) base vec

loadFontFace :: Integral a => String -> a -> IO FontFace
loadFontFace filepath sizePx = do
    ftLib <- FT_Base.ft_Init_FreeType
    face <- newFontFace ftLib filepath
    -- ft_Set_Pixel_Sizes :: face width height
    -- "a value of 0 for one of the dimensions means ‘same as the other’"
    FT_Base.ft_Set_Pixel_Sizes face 0 (fromIntegral sizePx)
    pure face
