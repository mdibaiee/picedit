{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Picture
Description : manipulation functions
Copyright   : (c) Mahdi Dibaiee, 2016
License     : GPL-3
Maintainer  : mdibaiee@aol.com
Stability   : experimental
Portability : POSIX
-}
module Data.Picture ( grayscale
                    , readPicture
                    , fromImage
                    , toImage
                    , writePicturePng
                    , fade
                    , rotate
                    , contrast
                    , brightness
                    , gamma
                    , invert
                    )
  where

    import Codec.Picture
    import Numeric.LinearAlgebra
    import qualified Data.Vector.Storable as V
    import System.IO
    import Data.Maybe

    -- | 'Picture' type is just a triple of color channel matrices: (R, G, B)
    type Picture = (Matrix Double, Matrix Double, Matrix Double)

    -- |Converts a JuicyPixel 'Image PixelRGB8' to 'Picture'
    fromImage :: Image PixelRGB8 -> Picture
    fromImage Image { imageWidth = w, imageHeight = h, imageData = vec } =
     let [r, g, b] = map (reshape w . V.fromList . reverse) (snd $ V.foldl' gp (0, [[],[],[]]) (V.map fromIntegral vec))
     in (r, g, b)
     where
       gp acc x =
         case acc of
           (0, [r, g, b]) -> (1, [x:r, g, b])
           (1, [r, g, b]) -> (2, [r, x:g, b])
           (2, [r, g, b]) -> (0, [r, g, x:b])

    -- |Converts a 'Picture' to JuicyPixel 'Image PixelRGB8'
    toImage :: Picture -> Image PixelRGB8
    toImage (r, g, b) = 
      let (fr, fg, fb) = (toList $ flatten r, toList $ flatten g, toList $ flatten b)
          img = V.map (fromIntegral . floor) . V.concat $ zipWith3 (\a b c -> vector [a, b, c]) fr fg fb
      in Image { imageWidth = cols r, imageHeight = rows r, imageData = img }


    -- | Reads a 'Picture' from specified path
    readPicture :: FilePath -> IO (Either String Picture)
    readPicture path = do
      img <- readImage path
      return $ case img of 
        Left err -> Left err
        Right im -> Right $ fromImage (convertRGB8 im)

    -- | Write the specified 'Picture' to a PNG file
    writePicturePng :: FilePath -> Picture -> IO ()
    writePicturePng path pic = writePng path (toImage pic)

    -- | Turn the 'Picture' grayscale
    grayscale :: Picture -> Picture
    grayscale (r, g, b) =
      let (fr, fg, fb) = (flatten r, flatten g, flatten b)
          mean = reshape (cols r) $ V.map (/ 3) (fr + fg + fb)
      in (mean, mean, mean)

    -- | Fade the 'Picture' by a number between 0 and 1
    fade :: Double -> Picture -> Picture
    fade opacity (r, g, b) = (f r, f g, f b)
      where
        f = cmap (*opacity)

    -- | Set contrast level of 'Picture', a number between -255 and 255
    contrast :: Double -> Picture -> Picture
    contrast level (r, g, b) = (f r, f g, f b)
      where
        cfactor = (259 * (level + 255)) / (255 * (259 - level))
        f = cmap (\x -> pixelBound $ cfactor * (x - 128) + 128)

    -- | Set brightness level of 'Picture', a number between -255 and 255
    brightness :: Double -> Picture -> Picture
    brightness level (r, g, b) = (f r, f g, f b)
      where
        f = cmap (pixelBound . (+level))

    -- | Set gamma level of 'Picture'
    gamma :: Int -> Picture -> Picture
    gamma level (r, g, b) = (f r, f g, f b)
      where
        f = cmap (\x -> pixelBound $ 255 * (x / 255) ^ level)

    -- | Inverts the 'Picture'
    invert :: Picture -> Picture
    invert (r, g, b) = (f r, f g, f b)
      where
        f = cmap (`subtract` 255)

    {- | Rotate 'Picture' for the specified degrees, around the specified origin.
     - If the origin is `Nothing`, rotates around the center
     -}
    rotate :: Double -> Maybe (Int, Int) -> Picture -> Picture
    rotate deg orig (r, g, b) = (f r, f g, f b)
      where
        -- rotation in radians
        rad = deg * pi / 180

        -- rotation matrix (clockwise)
        rm = fromLists [[cos rad, sin rad],
                        [negate $ sin rad, cos rad]]

        -- origin of rotation
        (originX, originY) = if isJust orig then fromJust orig else (cols r `div` 2, rows r `div` 2)

        -- all index pairs
        indices = [vector [fromIntegral x - fromIntegral originX, fromIntegral y - fromIntegral originY] | y <- [0..rows r - 1], x <- [0..cols r - 1]]

        -- rotate them using rotation matrix
        rotatedIndices :: [[Int]] = map (\r -> toList . V.map (fromIntegral . round) $ rm #> r) indices

        -- move them back to the origin
        movedIndices = map (\[x, y] -> [x + originX, y + originY]) rotatedIndices

        f m = reshape (cols m) $ fromList $ map (\[x, y] -> if y < 0 || y >= rows r || x < 0 || x >= cols r then 255 else m `atIndex` (y, x)) movedIndices

    bound (l, u) x = max l $ min u x
    pixelBound = bound (0, 255)
