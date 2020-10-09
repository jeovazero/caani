{-# LANGUAGE OverloadedStrings #-}

module Caani.Render
    ( renderLine,
      WorldConfig (..),
    )
where

import Caani.Color (add, fromHsvNorm)
import qualified Caani.Font as Font (FontFace, Glyph (..), loadChar)
import qualified Caani.Highlight as H (ColorWord (..))
import Caani.Image (MutImage)
import Codec.Picture ( PixelRGBA8(PixelRGBA8), Pixel(writePixel) )
import qualified Data.Text as T
import Data.Vector ((!))

data WorldConfig = WorldConfig
    { wFace :: Font.FontFace,
      wBaseWidth :: Int,
      wSize :: Int,
      wOffsetLeft :: Int,
      wOffsetTop :: Int,
      wImage :: MutImage
    }

renderLine :: WorldConfig -> [H.ColorWord] -> Int -> IO ()
renderLine = renderLine' 0

renderLine' :: Int -> WorldConfig -> [H.ColorWord] -> Int -> IO ()
renderLine' _ _ [] _ = pure ()
renderLine' accOffset worldConfig (cw:cws) line = do
    accOffset' <- drawWord accOffset worldConfig cw line
    renderLine' accOffset' worldConfig cws line

drawWord :: Int -> WorldConfig -> H.ColorWord -> Int -> IO Int
drawWord gLeftOffset worldConfig (H.ColorWord word color) line = do
    let face = wFace worldConfig
        base = wBaseWidth worldConfig
        sizePx = wSize worldConfig
        gTopOffset = truncate $ fromIntegral (sizePx * line) * 1.2
    bitmapList <- sequence $ fmap (Font.loadChar face base) $ T.unpack word
    renderWord (gLeftOffset, gTopOffset) bitmapList worldConfig color

renderWord :: (Int, Int) -> [Font.Glyph] -> WorldConfig -> (Float, Float, Float) -> IO Int
renderWord (gLeft, _) [] _ _ = pure gLeft
renderWord (gLeft, gTop) (Font.Glyph w h l t bs buffer:xs) worldConfig color =
    m >> renderWord (offset', gTop) xs worldConfig color
    where
        m = applyBitmap (fLeft, fTop) ((w, h), buffer) (wImage worldConfig) color (0, 0)
        offset' = gLeft + bs
        fLeft = l + gLeft + (wOffsetLeft worldConfig)
        fTop = (wSize worldConfig) - t + gTop + (wOffsetTop worldConfig)

applyBitmap (offsetW, offsetT) v@((w, h), bitmap) mutImage color@(cr, cg, cb) u@(a, b)
    | b + 1 >= h && a + 1 >= w = pure ()
    | otherwise = m >> applyBitmap (offsetW, offsetT) v mutImage color next
    where
        next = nextPixel u $ fst v
        p = fromIntegral $ bitmap ! (a + b * w)
        (r1, g1, b1) = fromHsvNorm (cr, (p / 255) * cg, cb)
        (r2, g2, b2) = add (r1, g1, b1) (20 / 255, 20 / 255, 20 / 255) (p / 255)
        ra = fromIntegral $ truncate (r2 * 255)
        ga = fromIntegral $ truncate (g2 * 255)
        ba = fromIntegral $ truncate (b2 * 255)
        qx = PixelRGBA8 ra ga ba 255
        m = writePixel mutImage (a + offsetW) (offsetT + b) qx

nextPixel :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a)
nextPixel (a, b) (c, _)
    | a + 1 == c = (0, b + 1)
    | otherwise = (a + 1, b)
