module Main where

import qualified Graphics.Rendering.FreeType.Internal as FT
import Foreign (Storable(peek), alloca)
import qualified Graphics.Rendering.FreeType.Internal.Library as FTL
import Foreign.C (withCString)
import qualified Graphics.Rendering.FreeType.Internal.FaceType as FTF
import Control.Exception (throw)
import Control.Exception.Base (Exception)
import qualified Graphics.Rendering.FreeType.Internal.Face as Face
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as BT
import Data.Char (chr)
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FP
import Foreign.Marshal (peekArray)

output :: [Char]
output = "caani.png" 

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

fontFace :: FTL.FT_Library -> String -> IO FTF.FT_Face
fontFace ftLib filepath = withCString filepath $ \cstr ->
  alloca $ \ptr -> do
    runFreeType $ FT.ft_New_Face ftLib cstr 0 ptr
    peek ptr

loadChar filePath char sizePx unit = do
  ftLib <- freeType
  face <- fontFace ftLib filePath
  runFreeType $ FT.ft_Set_Pixel_Sizes face (fromIntegral sizePx) 0
  charCode <- FT.ft_Get_Char_Index face $ fromIntegral $ fromEnum char
  runFreeType $ FT.ft_Load_Glyph face charCode 0
  slot <- peek $ Face.glyph face
  print slot
  n <- peek $ Face.num_glyphs face
  putStrLn $ "glyphs:" ++ show n
  runFreeType $ FT.ft_Render_Glyph slot FP.ft_RENDER_MODE_NORMAL
  fmt <- peek $ GS.format slot
  putStrLn $ "glyph format:" ++ show fmt
  bmp <- peek $ GS.bitmap slot
  a <- peekArray 100 $ BT.buffer bmp
  putStrLn $ "width:" ++ show (BT.width bmp)
  putStrLn $ " rows:" ++ show (BT.rows bmp)
  putStrLn $ " buffer:" ++ show  a

main :: IO ()
main = do
  loadChar "./fonts/FiraCode-Medium.ttf" 'A' 24 1