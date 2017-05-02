#!/bin/bash

F=$1

convert ${F}.png -resize 16x16 16.png
convert ${F}.png -resize 32x32 32.png
convert ${F}.png -resize 64x64 64.png
convert ${F}.png -resize 128x128 128.png
convert ${F}.png -resize 256x256 256.png

convert 16.png 32.png 64.png 128.png 256.png ${F}.ico
