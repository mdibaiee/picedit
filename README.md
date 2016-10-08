picedit
=======

A simple CLI and API for image manipulation

CLI Usage
---------

```
Usage: picedit <input> [OPTIONS]
Options:
  --contrast <n> - a number between -255 and 255
  --brightness <n> - a number between -255 and 255
  --gamma <n>
  --fade <n> - a number between 0 and 100
  --rotate <n> - rotate image by n degrees
  --grayscale - turn the image grayscale
  --invert - invert (negative) the image
  --output <filename> - output name, defaults to 'output.png'
```

Library
-------
Documentation available at hackage

```haskell
import Picture

main = do
  pic <- readPicture "myfile.png"

  writePicturePng "output.png" (grayscale pic)
```
