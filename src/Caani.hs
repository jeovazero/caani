{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Caani (caaniFromFile,caani, CaaniConfig(..)) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Caani.Highlight (highlightHaskell)
import Caani.Image (toPixel, createImage, savePng)
import Caani.Font (FontFace, loadFontFace, loadChar, gWidth)
import Caani.Render (WorldConfig(..), renderLine)
import Caani.Error (rightOrThrow, tryIO, CaaniError(..), CaaniErrorType(..), tryOrThrow)
import Graphics.Rasterific
import Codec.Picture (convertRGBA8, writePng, readPng, Image(..), PixelRGBA8(..))
import Graphics.Rasterific.Texture (uniformTexture)
import Codec.Picture.Types (thawImage)
import Control.Exception (throwIO)

-- | Naive :(
dimensions :: T.Text -> (Int, Int)
dimensions text = (maximum $ fmap T.length lns, length lns)
  where lns = T.lines text

sizePx = 24
margin = 2 * sizePx

highlightWith :: FontFace -> Image PixelRGBA8 -> (T.Text,(Int,Int)) -> (Int, Int, Int) -> String -> IO ()
highlightWith face tagBitmap (code,(w,h)) background outPath = do
  glyph <- loadChar face 0 'M'
  let base = gWidth glyph
  
  -- text <- TIO.getContents
  let tokens = highlightHaskell code
  let width = w * base + margin
  let height = (truncate $ fromIntegral (h * sizePx) * 1.2) + margin
  let frameWidth = width + 76 + margin
  let frameHeight = height + margin
  let fullWidth = frameWidth + 2 * margin
  let fullHeight = frameHeight + 2 * margin

  let im = drawFrame tagBitmap (fullWidth, fullHeight) (frameWidth, frameHeight) background
  mutImage <- thawImage im
 
  let config = WorldConfig {
      wFace = face
      , wBaseWidth = base
      , wSize = sizePx
      , wOffsetLeft = sizePx + div (fullWidth - frameWidth) 2 + 76
      , wOffsetTop = div (fullHeight - frameHeight) 2 + margin + 4
      , wImage = mutImage
      }

  case tokens of
    Nothing -> throwIO $ CaaniError InvalidCode
    Just colorWords -> do
      let t = zipWith (\a b -> (a,b)) (colorWords) [0..]
      sequence_ $ fmap (\(cws,ln) -> renderLine config cws ln) t
      savePng outPath mutImage

bg = (229,229,229)

drawFrame :: Image PixelRGBA8 -> (Int,Int) -> (Int,Int) -> (Int,Int,Int) -> Image PixelRGBA8
drawFrame tag (fw,fh) (w,h) bgColor =
  renderDrawing fw fh (toPixel bgColor) $
    withTexture (uniformTexture $ toPixel (20,20,20)) $ do
      fill $ roundedRectangle (V2 mw mh') (fromIntegral w) (fromIntegral h') 11 11
      withTexture (uniformTexture $ toPixel (209,209,209)) $ do
          fill $ roundedRectangle (V2 mw mh) (fromIntegral w) (40) 9 9
          withTexture (uniformTexture $ toPixel (20,20,20)) $ do
            fill $ rectangle (V2 mw mh'') (fromIntegral w) 25
            drawImageAtSize tag 0 (V2 (mw + 20) mh'') 44 56  
  where
    mw = fromIntegral $ div (fw - w) 2
    mh' = mh + 2
    mh'' = mh + 20
    h' = h - 2
    mh = fromIntegral $ div (fh - h) 2

data CaaniConfig
  = CaaniConfig
  { fontPath :: String
  , tagPath :: String
  , boundary :: (Int,Int)
  , outPath :: String
  , code :: T.Text
  }

caaniFromFile :: String -> CaaniConfig -> IO ()
caaniFromFile filepath config = do
  text <- tryOrThrow (TIO.readFile filepath) LoadFileError
  caani (config { code = text })

caani :: CaaniConfig -> IO ()
caani (CaaniConfig {..}) = do
  tagRaw <- rightOrThrow (readPng tagPath) LoadTagError
  let tag = convertRGBA8 tagRaw
  fontFace <- tryOrThrow (loadFontFace fontPath sizePx) LoadFontError
  let (w,h) = dimensions code
  let (boundW, boundH) = boundary
  case (w > boundW,h > boundH) of
    (False, False) -> highlightWith fontFace tag (code,(w,h)) bg outPath
    _ -> throwIO $ CaaniError BoundaryLimit