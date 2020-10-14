{-# LANGUAGE OverloadedStrings #-}

module Caani.Image
    ( createImage,
      savePng,
      MutImage,
      toPixel,
    )
where

import Codec.Picture ( writePng, PixelRGBA8(..) )
import Codec.Picture.Types (MutableImage, createMutableImage, freezeImage)
import Control.Monad.Primitive (PrimMonad (PrimState))

type MutImage = MutableImage (PrimState IO) PixelRGBA8

toPixel :: (Integral a) => (a, a, a) -> PixelRGBA8
toPixel (r, g, b) =
    PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

createImage :: (Integral a) => Int -> Int -> (a, a, a) -> IO MutImage
createImage width height background =
    createMutableImage width height (toPixel background)

savePng :: String -> MutImage -> IO ()
savePng path mutImage = do
    im <- freezeImage mutImage
    writePng path im
    pure ()
