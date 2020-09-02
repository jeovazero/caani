{-# LANGUAGE OverloadedStrings #-}
module Lib.Image (createImage, savePng, MutImage,toPixel) where

import Codec.Picture
import Data.Vector (empty, (!), Vector, fromList)
import Codec.Picture.Types (freezeImage, MutableImage, createMutableImage)
import Control.Monad.Primitive (PrimMonad(PrimState))

type MutImage = MutableImage (PrimState IO) PixelRGBA8

toPixel (r,g,b) =
  PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

createImage width height background =
  createMutableImage width height (toPixel background)

savePng :: String -> MutImage -> IO ()
savePng path mutImage = do
  im <- freezeImage mutImage
  writePng path im
  pure ()