{-# LANGUAGE OverloadedStrings #-}
module Caani (caaniFromFile,caaniFromText) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Lib.Highlight (highlightHaskell)
import Lib.Image (toPixel, createImage, savePng)
import Lib.Font (loadFontFace, loadChar, gWidth)
import Lib.Render (WorldConfig(..), renderLine)
import Graphics.Rasterific
import Codec.Picture (convertRGBA8, writePng, readPng, Image(..), PixelRGBA8(..))
import Graphics.Rasterific.Texture (uniformTexture)
import Codec.Picture.Types (thawImage)

-- | Naive :(
dimensions :: T.Text -> (Int, Int)
dimensions text = (maximum $ fmap T.length lns, length lns)
  where lns = T.lines text

sizePx = 24
margin = 2 * sizePx

highlightWith fontPath tag code bg outPath = do
  face <- loadFontFace fontPath sizePx
  glyph <- loadChar face 0 'M'
  let base = gWidth glyph
  
  -- text <- TIO.getContents
  let tokens = highlightHaskell code
  let (w,h) = dimensions code

  let width = w * base + margin
  let height = (truncate $ fromIntegral (h * sizePx) * 1.2) + margin
  let frameWidth = width + 76 + margin
  let frameHeight = height + margin
  let fullWidth = frameWidth + 2 * margin
  let fullHeight = frameHeight + 2 * margin

  let im = drawFrame tag (fullWidth, fullHeight) (frameWidth, frameHeight) bg
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
    Nothing -> putStrLn "Error on parsing"
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

caaniFromFile :: String -> String -> String -> String -> IO ()
caaniFromFile fontPath tagPath codePath outPath = do
  eTagRaw <- readPng tagPath
  text <- TIO.readFile codePath
  case eTagRaw of
    Left err -> putStrLn err
    Right tagRaw -> do
      let tag = convertRGBA8 tagRaw
      highlightWith fontPath tag text bg outPath

caaniFromText :: String -> String -> T.Text -> String -> IO ()
caaniFromText fontPath tagPath code outPath = do
  eTagRaw <- readPng tagPath
  case eTagRaw of
    Left err -> putStrLn err
    Right tagRaw -> do
      let tag = convertRGBA8 tagRaw
      highlightWith fontPath tag code bg outPath
    
caani :: T.Text -> String -> IO ()
caani code outPath = do
  caaniFromText "./fonts/FiraCode-Medium.ttf" "./haskell-tag.png" code outPath