{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Data.Picture
Description : picture manipulation functions
Copyright   : (c) Mahdi Dibaiee, 2016
License     : GPL-3
Maintainer  : mdibaiee@aol.com
Stability   : experimental
Portability : POSIX
-}
module Data.Picture ( Picture
                    -- * Manipulation functions
                    , grayscale
                    , fade
                    , rotate
                    , contrast
                    , brightness
                    , gamma
                    , invert
                    , compress
                    , embed
                    , resize
                    , Data.Picture.scale
                    -- * Converting between Image and Picture
                    , fromImage
                    , toImage
                    -- * IO operations
                    , readPicture
                    , writePicturePng
                    )
  where

    import Codec.Picture
    import Numeric.LinearAlgebra
    import qualified Data.Vector.Storable as V
    import System.IO
    import Data.Maybe
    import Debug.Trace
    import Data.List (zipWith4)

    -- | (R, G, B, A) color channels
    type Picture = (Matrix Double, Matrix Double, Matrix Double, Matrix Double)

    -- | Converts a JuicyPixel 'Image PixelRGBA8' to 'Picture'
    fromImage :: Image PixelRGBA8 -> Picture
    fromImage Image { imageWidth = w, imageHeight = h, imageData = vec } =
     let [r, g, b, a] = map (reshape w . V.fromList . reverse) (snd $ V.foldl' gp (0, [[],[],[],[]]) (V.map fromIntegral vec))
     in (r, g, b, a)
     where
       gp acc x =
         case acc of
           (0, [r, g, b, a]) -> (1, [x:r, g, b, a])
           (1, [r, g, b, a]) -> (2, [r, x:g, b, a])
           (2, [r, g, b, a]) -> (3, [r, g, x:b, a])
           (3, [r, g, b, a]) -> (0, [r, g, b, x:a])

    -- | Converts a 'Picture' to JuicyPixel 'Image PixelRGBA8'
    toImage :: Picture -> Image PixelRGBA8
    toImage (r, g, b, a) = 
      let (fr, fg, fb, fa) = (toList $ flatten r, toList $ flatten g, toList $ flatten b, toList $ flatten a)
          img = V.map (fromIntegral . floor) . V.concat $ zipWith4 (\a b c d -> vector [a, b, c, d]) fr fg fb fa
      in Image { imageWidth = cols r, imageHeight = rows r, imageData = img }


    -- | Reads a 'Picture' from specified path
    readPicture :: FilePath -> IO (Either String Picture)
    readPicture path = do
      img <- readImage path
      return $ case img of 
        Left err -> Left err
        Right im -> Right $ fromImage (convertRGBA8 im)

    -- | Write the specified 'Picture' to a PNG file
    writePicturePng :: FilePath -> Picture -> IO ()
    writePicturePng path pic = writePng path (toImage pic)

    -- | Turn the 'Picture' grayscale
    grayscale :: Picture -> Picture
    grayscale (r, g, b, a) =
      let (fr, fg, fb) = (flatten r, flatten g, flatten b)
          mean = reshape (cols r) $ V.map (/ 3) (fr + fg + fb)
      in (mean, mean, mean, a)

    -- | Fade the 'Picture' by a number between 0 and 1
    fade :: Double -> Picture -> Picture
    fade opacity (r, g, b, a) = (r, g, b, f a)
      where
        f = cmap (*opacity)

    -- | Set contrast level of 'Picture', a number between -255 and 255
    contrast :: Double -> Picture -> Picture
    contrast level (r, g, b, a) = (f r, f g, f b, a)
      where
        cfactor = (259 * (level + 255)) / (255 * (259 - level))
        f = cmap (\x -> pixelBound $ cfactor * (x - 128) + 128)

    -- | Set brightness level of 'Picture', a number between -255 and 255
    brightness :: Double -> Picture -> Picture
    brightness level (r, g, b, a) = (f r, f g, f b, a)
      where
        f = cmap (pixelBound . (+level))

    -- | Set gamma level of 'Picture'
    gamma :: Int -> Picture -> Picture
    gamma level (r, g, b, a) = (f r, f g, f b, a)
      where
        f = cmap (\x -> pixelBound $ 255 * (x / 255) ^ level)

    -- | Inverts the 'Picture'
    invert :: Picture -> Picture
    invert (r, g, b, a) = (f r, f g, f b, a)
      where
        f = cmap (`subtract` 255)

    -- | Rotate 'Picture' for the specified degrees, around the specified origin.
    -- If the origin is `Nothing`, rotates around the center
    rotate :: Double -> Maybe (Int, Int) -> Picture -> Picture
    rotate deg orig (r, g, b, a) = (f r, f g, f b, f a)
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

        f m = reshape (cols m) $ fromList $ map (\[x, y] -> if y < 0 || y >= rows r || x < 0 || x >= cols r then 0 else m `atIndex` (y, x)) movedIndices

    -- | Compress the image using SVD
    -- note: this is not size compression, it's just a k-rank approximation of the image
    compress :: Int -> Picture -> Picture
    compress rate (r, g, b, a) = (f r, f g, f b, a)
      where
        k = cols r - rate
        f m =
          let (u, s, v) = svd m
              si = diagRect 0 s (rows m) (cols m)
              (mu, ms, mv) = (u ?? (All, Take k), si ?? (Take k, Take k), (tr v) ?? (Take k, All))
          in mu <> ms <> mv

    -- | Embed a 'Picture' into another one, in the specified position-}
    embed :: Picture -> (Int, Int) -> Picture -> Picture
    embed (br, bg, bb, ba) (x, y) (lr, lg, lb, la) = (f br lmr, f bg lmg, f bb lmb, maxAlpha)
      where
        (lmr, lmg, lmb, lma) = (fit lr, fit lg, fit lb, fit la)
        scaledAlpha = cmap (/255) lma
      
        fit m =
          let distance = y * cols br
              total = rows br * cols br
              xPush = (rows m><x) (repeat 0)
              xPast = (rows m><(cols br - x - cols m)) (repeat 0)
              positioned = xPush ||| m ||| xPast
              flat = toList $ flatten positioned
          in (rows br><cols br) $ replicate distance 0 ++ flat ++ repeat 0

        f b lm = (b * (cmap (1-) scaledAlpha)) + (lm * scaledAlpha)
        maxAlpha = (rows ba><cols ba) $ zipWith max (toList . flatten $ ba) (toList . flatten . fit $ la)

    -- | Resize an image using nearest-neighbor interpolation
    resize :: (Int, Int) -> Picture -> Picture
    resize (sWidth, sHeight) (r, g, b, a) = (f r, f g, f b, f a)
      where
        initial = vector [0..fromIntegral sWidth * fromIntegral sHeight - 1]
        (width, height) = (cols r, rows r)
        factor = 2 ^ 16
        (xRatio, yRatio) = (width * factor `div` sWidth + 1, height * factor `div` sHeight + 1)
        f m = reshape sWidth $ V.map replace initial
          where
            v = flatten m
            replace index =
              let (x, y) = (floor index `mod` sWidth, floor index `div` sWidth)
                  (px, py) = (x * xRatio `div` factor, y * yRatio `div` factor)
              in v ! (py * width + px)

    -- | Scale an image using the resize function
    scale :: Double -> Picture -> Picture
    scale 1 p = p
    scale s (r, g, b, a) = resize (floor $ s * width, floor $ s * height) (r, g, b, a)
      where
        (width, height) = (fromIntegral $ cols r, fromIntegral $ rows r)


    bound (l, u) x = max l $ min u x
    pixelBound = bound (0, 255)
