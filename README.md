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
  --compress <n> - approximate the (width - n)-th rank of image using SVD
                   a number between 0 (no compression) and image width (full compression)
                   note: this is not size compression
  --output <filename> - output name, defaults to 'output.png'
```

Example:
```
stack exec picedit -- dreamboy.jpg --grayscale --contrast 180 --gamma 2 --rotate 180
```

<img width='45%' src='https://github.com/mdibaiee/picedit/blob/master/dreamboy.jpg?raw=true' />
<img width='45%' src='https://github.com/mdibaiee/picedit/blob/master/output.png?raw=true' />

Embedding images (the zombie is embedded at top left, 100x + 10y):

<img width='5%' src='https://github.com/mdibaiee/picedit/blob/master/45.png?raw=true' />
<img width='40%' src='https://github.com/mdibaiee/picedit/blob/master/output.png?raw=true' />
<img width='40%' src='https://github.com/mdibaiee/picedit/blob/master/test.png?raw=true' />

Library
-------
[Documentation available at hackage](https://hackage.haskell.org/package/picedit-0.1.0.0/docs/Data-Picture.html)

```haskell
import Data.Picture

main = do
  pic <- readPicture "myfile.png"

  writePicturePng "output.png" (grayscale pic)
```
