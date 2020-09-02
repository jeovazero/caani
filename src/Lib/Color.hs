{-# LANGUAGE OverloadedStrings #-}
module Lib.Color (fromHsvNorm,add) where

import Data.Fixed (mod')

-- from https://en.wikipedia.org/wiki/HSL_and_HSV
fromHsv (h,s,v) = (r,g,b)
  where
    c = v * s
    hi = (h / 60)
    x = c * (1 - abs ((mod' (hi) 2) - 1))
    m = v - c
    (r',g',b') = rgbh (hi,c,x)
    r = fromIntegral $ truncate $ (r' + m) * 255
    g = fromIntegral $ truncate $ (g' + m) * 255
    b = fromIntegral $ truncate $ (b' + m) * 255

fromHsvNorm (h,s,v) = (r / 255, g / 255, b / 255)
  where (r,g,b) = fromHsv (h, s, v)

rgbh (h,c,x)
  | h >= 0 && h <= 1 = (c,x,0)
  | h > 1 && h <= 2 = (x,c,0)
  | h > 2 && h <= 3 = (0,c,x)
  | h > 3 && h <= 4 = (0,x,c)
  | h > 4 && h <= 5 = (x,0,c)
  | h > 5 && h <= 6 = (c,0,x)
  | otherwise = (0, 0, 0)

-- Alpha blending
add :: (Float, Float, Float) -> (Float, Float, Float) -> Float -> (Float, Float, Float)
add (r1,g1,b1) (r2,g2,b2) a =
  (add' r1 r2 a, add' g1 g2 a, add' b1 b2 a)
  where add' x y a = x * a + y - y * a