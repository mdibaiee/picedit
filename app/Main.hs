module Main where
  import Data.Picture
  import System.Environment
  import Data.Either

  data Options = Options { file :: FilePath
                         , output :: FilePath
                         , argInvert :: Bool
                         , argGrayscale :: Bool
                         , argRotate :: Double
                         , argFade :: Double
                         , argContrast :: Double
                         , argGamma :: Int
                         , argBrightness :: Double
                         , argCompress :: Int
                         }

  opts = Options { file = ""
                 , output = "output.png"
                 , argInvert = False
                 , argGrayscale = False
                 , argRotate = 0
                 , argFade = 100
                 , argContrast = 0
                 , argGamma = 1
                 , argBrightness = 0
                 , argCompress = 0
                 }

  main :: IO ()
  main = do
    args <- getArgs

    if null args
      then do
        putStrLn "Usage: picedit <input> [OPTIONS]"
        putStrLn "Options:"
        putStrLn "  --contrast <n> - a number between -255 and 255"
        putStrLn "  --brightness <n> - a number between -255 and 255"
        putStrLn "  --gamma <n>"
        putStrLn "  --fade <n> - a number between 0 and 100"
        putStrLn "  --rotate <n> - rotate image by n degrees"
        putStrLn "  --grayscale - turn the image grayscale"
        putStrLn "  --invert - invert (negative) the image"
        putStrLn "  --compress <n> - approximate the (width - n)-th rank of image using SVD, note: this is not size compression, a number between 0 (no compression) and image width (full compression)"
        putStrLn "  --output <filename> - output name, defaults to output.png"
      else do 
        let options = parseArgs args opts

        pic <- readPicture (file options)
        Right other <- readPicture ("output.png")

        case pic of
          Left err -> print err
          Right p -> do
            let edited = rotate (argRotate options) Nothing 
                       . fade (argFade options / 100)
                       . contrast (argContrast options)
                       . gamma (argGamma options)
                       . brightness (argBrightness options)
                       . conditionalFn grayscale (argGrayscale options) 
                       . conditionalFn invert (argInvert options)
                       . compress (argCompress options) $ p
            writePicturePng (output options) edited
            
        return ()

    where
      conditionalFn f True = f
      conditionalFn f False = id

  parseArgs :: [String] -> Options -> Options
  parseArgs [] opts = opts
  parseArgs ("--invert":rest) opts = parseArgs rest (opts { argInvert = True })
  parseArgs ("--grayscale":rest) opts = parseArgs rest (opts { argGrayscale = True })
  parseArgs ("--rotate":n:rest) opts = parseArgs rest (opts { argRotate = read n })
  parseArgs ("--fade":n:rest) opts = parseArgs rest (opts { argFade = read n })
  parseArgs ("--contrast":n:rest) opts = parseArgs rest (opts { argContrast = read n })
  parseArgs ("--brightness":n:rest) opts = parseArgs rest (opts { argBrightness = read n })
  parseArgs ("--gamma":n:rest) opts = parseArgs rest (opts { argGamma = read n })
  parseArgs ("--compress":n:rest) opts = parseArgs rest (opts { argCompress = read n })
  parseArgs ("--output":n:rest) opts = parseArgs rest (opts { output = n })
  parseArgs (name:rest) opts = parseArgs rest (opts { file = name })

