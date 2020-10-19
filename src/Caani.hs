{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Caani
    ( caaniFromFile,
      caani,
      CaaniConfig (..),
    )
where

import Caani.Error (CaaniError(..), CaaniErrorType(..), caanify, tryCaani)
import Caani.Font (FontFace, gWidth, loadChar, loadFontFace)
import Caani.Highlight (highlightHaskell)
import Caani.Image (savePng, toPixel)
import Caani.Render (WorldConfig(..), renderLine)
import Codec.Picture (Image(..), PixelRGBA8(..), convertRGBA8, readPng)
import Codec.Picture.Types (thawImage)
import Control.Exception (throwIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Graphics.Rasterific
import Graphics.Rasterific.Texture (uniformTexture)

-- | Naive :(
dimensions :: T.Text -> (Int, Int)
dimensions text =
    case tLines of
        []     -> (0, 0)
        lines' -> (maximum $ fmap T.length lines', length lines')
    where
        tLines = T.lines text

sizePx, margin :: Int
sizePx = 24
margin = 2 * sizePx

corner, flagWidth, flagHeight, topBorderHeight, offsetFlag, lineHeight :: Float
corner = 11
flagWidth = 44
flagHeight = 56
topBorderHeight = 20
offsetFlag = 20
lineHeight = 1.2

highlightWith :: FontFace -> Image PixelRGBA8 -> (T.Text, (Int, Int)) -> String -> IO ()
highlightWith face tagBitmap (code, (w, h)) outPath = do
    glyph <- loadChar face 0 'M' -- It can throw FreeTypeError
    let base = gWidth glyph
    -- It's needed for the base width (monospace effect)
    -- I'm considering that 'M' is the biggest

    let tokens = highlightHaskell code
    let width = w * base + margin
    -- 1.2 is for the line height
    let height = (truncate $ fromIntegral (h * sizePx) * lineHeight) + margin
    -- 76 is a magic number XD
    let frameWidth = width + 76 + margin
    let frameHeight = height + margin
    let fullWidth = frameWidth + 2 * margin
    let fullHeight = frameHeight + 2 * margin
    let im = drawFrame tagBitmap (fullWidth, fullHeight) (frameWidth, frameHeight)
    mutImage <- thawImage im
    let config =
            WorldConfig
                { wFace = face,
                  wBaseWidth = base,
                  wSize = sizePx,
                  wOffsetLeft = sizePx + div (fullWidth - frameWidth) 2 + 76,
                  wOffsetTop = div (fullHeight - frameHeight) 2 + margin + 4,
                  wImage = mutImage
                }
    case tokens of
        Nothing -> throwIO $ CaaniError InvalidCode
        Just colorWords -> do
            let t = zipWith (\a b -> (a, b)) colorWords [0 ..]
            sequence_ $ fmap (\(cws, ln) -> renderLine config cws ln) t -- It can throw FreeTypeError
            savePng outPath mutImage
            pure ()

primaryBackground, secondaryBackground, borderColor :: (Integer, Integer, Integer)
primaryBackground = (229, 229, 229)
secondaryBackground = (20, 20, 20)
borderColor = (209, 209, 209)

drawFrame :: Image PixelRGBA8 -> (Int, Int) -> (Int, Int) -> Image PixelRGBA8
drawFrame tag (fullW, fullH) (w, h) =
    renderDrawing fullW fullH (toPixel primaryBackground)
        $ withTexture (uniformTexture $ toPixel secondaryBackground) -- the main background
        $ do
            fill $ roundedRectangle pointContainer intW intH' corner corner -- container
            withTexture (uniformTexture $ toPixel borderColor) $ do
                -- top border context
                fill $ roundedRectangle pointTopBorder intW 40 9 9 -- top border
                withTexture (uniformTexture $ toPixel secondaryBackground) $ do
                    -- code area context
                    fill $ rectangle pointCodeArea intW 25 -- code area
                    drawImageAtSize tag 0 pointFlag flagWidth flagHeight -- language flag
    where
        mW = fromIntegral $ div (fullW - w) 2
        intW = fromIntegral w
        mH = fromIntegral $ div (fullH - h) 2
        mH' = mH + 2
        mH'' = mH + topBorderHeight
        pointContainer = V2 mW mH'
        pointTopBorder = V2 mW mH
        pointCodeArea = V2 mW mH''
        pointFlag = V2 (mW + offsetFlag) mH''
        intH' = fromIntegral $ h - 2

data CaaniConfig = CaaniConfig
    { fontPath :: String,
      tagPath  :: String,
      boundary :: (Int, Int),
      outPath  :: String,
      code     :: T.Text
    }

caaniFromFile :: String -> CaaniConfig -> IO (Either CaaniError ())
caaniFromFile filepath config = do
    textE <- tryCaani (TIO.readFile filepath) (const LoadFileError)
    case textE of
        Right text -> caani (config {code = text})
        Left err   -> pure $ Left err

loadTagImage :: String -> IO (Either CaaniError (Image PixelRGBA8))
loadTagImage tagPath =
    readPng tagPath
        >>= pure
            . either
                (const (Left $ CaaniError LoadTagError))
                (Right . convertRGBA8)

loadFontFace' :: Integral a => String -> a -> IO (Either CaaniError FontFace)
loadFontFace' fontPath sizePx' =
    tryCaani (loadFontFace fontPath sizePx') (const LoadFontError)

caani :: CaaniConfig -> IO (Either CaaniError ())
caani (CaaniConfig {..}) = do
    let (w, h) = dimensions code
    let (boundW, boundH) = boundary
    tagE <- loadTagImage tagPath
    fontFaceE <- loadFontFace' fontPath sizePx
    case (w > boundW, h > boundH) of
        (False, False) -> either (pure . Left) caanify $ do
            tag <- tagE
            fontFace <- fontFaceE
            pure $ highlightWith fontFace tag (code, (w, h)) outPath
        _ -> pure . Left $ CaaniError BoundaryLimit
