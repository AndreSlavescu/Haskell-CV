module Main where

import Prelude as P 
import Graphics.Image as I 
import Graphics.Image.IO as IIO
import Graphics.Image.Drawing as ID
import Graphics.Image.Processing as IP
import Graphics.Image.Processing.Filters as F

-- function that takes an image and a list of points and returns an image with the points drawn on it
getPixel :: (Int, Int) -> Pixel RGB Word8
getPixel (i, j) = PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j))

getImage :: (Int, Int) -> Image VS RGB Word8
getImage (w, h) = makeImageR VS (w, h) getPixel

-- highlights the foreground of an image
highlight :: Image VS RGB Word8 -> Image VS RGB Word8
highlight image = mapImage (\(PixelRGB r g b) -> PixelRGB (min 255 (r + 100)) (min 255 (g + 100)) (min 255 (b + 100))) image

main :: IO ()
main = writeImageExact PNG [] "car.png" image
    where image = getImage (640, 480)


