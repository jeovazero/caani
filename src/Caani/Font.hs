module Caani.Font (Glyph(..),loadChar,loadFontFace,FontFace) where

import qualified Graphics.Rendering.FreeType.Internal as FT
import Foreign (Storable(peek), alloca)
import qualified Graphics.Rendering.FreeType.Internal.Library as FTL
import Foreign.C (CChar, withCString)
import Control.Exception (throw)
import Control.Exception.Base (Exception)
import qualified Graphics.Rendering.FreeType.Internal.Face as Face
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as BT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FP
import Foreign.Marshal (peekArray)
import Data.Bits (Bits(shiftR))
import qualified Data.Vector as Vec (empty, Vector, fromList)

data FreeTypeError = FreeTypeError String
  deriving (Show, Eq)

instance Exception FreeTypeError

runFreeType effect = do
  err <- effect
  case err of
    0 -> pure ()
    e -> throw (FreeTypeError $ show e)

freeType :: IO FTL.FT_Library
freeType = alloca $ \ptr -> do
  runFreeType $ FT.ft_Init_FreeType ptr
  peek ptr

type FontFace = Face.FT_Face

fontFace :: FTL.FT_Library -> String -> IO FontFace
fontFace ftLib filepath = withCString filepath $ \cstr ->
  alloca $ \ptr -> do
    runFreeType $ FT.ft_New_Face ftLib cstr 0 ptr
    peek ptr

ubyteToInt :: CChar -> Int
ubyteToInt u = if u < 0 then 256 + iu else iu
  where iu = fromIntegral u

data Glyph = Glyph
  { gWidth :: Int
  , gHeight :: Int
  , gLeft :: Int
  , gTop :: Int
  , gBaseWidth :: Int
  , gBuffer :: Vec.Vector Int
  }

loadChar face base ' ' = pure $ Glyph 0 0 0 0 base Vec.empty
loadChar face base '\t' = pure $ Glyph 0 0 0 0 (2 * base) Vec.empty
loadChar face base chr = do
  charCode <- FT.ft_Get_Char_Index face $ fromIntegral $ fromEnum chr
  runFreeType $ FT.ft_Load_Glyph face charCode 0
  slot <- peek $ Face.glyph face
  runFreeType $ FT.ft_Render_Glyph slot FP.ft_RENDER_MODE_NORMAL
  l <- peek $ GS.bitmap_left slot
  t <- peek $ GS.bitmap_top slot
  bitmap <- peek $ GS.bitmap slot
  let w = fromIntegral $ BT.width bitmap
      h = fromIntegral $ BT.rows bitmap
      arrSize = w * h
  arr <- peekArray arrSize $ BT.buffer bitmap
  let vec = Vec.fromList $ fmap ubyteToInt arr
  pure $ Glyph w h (fromIntegral l) (fromIntegral t) base vec

loadFontFace filepath sizePx = do
  ftLib <- freeType
  face <- fontFace ftLib filepath
  runFreeType $ FT.ft_Set_Pixel_Sizes face 0 (fromIntegral sizePx)
  pure face