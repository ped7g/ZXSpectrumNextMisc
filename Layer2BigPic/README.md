# Big picture scroller example

## what it does

Bounces 640x512 8bpp image in Layer2 mode 320x256x8bpp, using HW scroll, copper and DMA
to minimize the performance requirements.

The vertical (up/down) scrolling is almost for free and takes the same amount of CPU for
any change in vertical position.

The horizontal scrolling has to redraw 512 pixel tall columns entering the screen, seems
in HDMI 60Hz it's possible to copy about 14 pixel columns before the overdraw becomes
visible (it has to be done in the vertical blank area to stay out of sight of user),
HDMI 50Hz has enough time to do +16px horizontal scroll per frame.

## how to build it

You will need [sjasmplus](https://github.com/z00m128/sjasmplus) (v1.15.0+, to be released
hopefully soon, or build from the github master branch the work-in-progress version).
(or break the 3-argument MMU command to 2-argument MMU + ORG supported by v1.14.5)

Makefile is provided with extra build tasks if you want to do more things, to modify the
image data you will need to provide 640x512 8bit indexed PNG in default Layer2 palette,
and have image-magick tools installed (for the "convert" command converting the PNG into
2x TGA files, which are used in ASM file directly).

## how it works

There are 320kiB of source pixel data in memory (2x 640x256 top/bottom source images),
and further 160kiB (2x 320x256) of working-buffer which is displayed in Layer2.

The 160kiB working buffer is like two 320x256 images, "top" and "bottom" one.

The copper code (updated by vertical scroller) does switch NextReg $12 to point either
to the upper or bottom image, to connect them seamlessly int single big 320x512 image.

Then the regular Layer Y offset register is used to scroll this up and down.

The horizontal scroller updates regular Layer X offset registers (0..319 value) and
overdraws the 512 pixel tall columns in the working buffer, each column which is to
leave the screen after positionX update is overdrawn with the new pixels from source
data, as it becomes visible at the other side as new part of image.

The copying of data happens separately for top/bottom part of work buffer, using DMA
transfer to quickly transfer "n" columns of 256px tall blocks in each part.

The horizontal scrolling is thus CPU intensive and requires about 0.5% of single frame
time for every column scrolled (+-16px delta is then about 8% of frame, which still
fits into the v-blank area in HDMI 50Hz mode).

The example does use the brand new NextRegister $64 "video line offset" introduced
in core 3.1.5 to simplify the copper coordinates (so the scanline "0" is the line
before the 32px tall block of "top border" area, and the example doesn't need to have
table with scanline counts for each possible video mode to target the top border area
correctly.

## thanks, author, license, ...

Big thanks to Allen A. for introducing the NextRegister $64, which allows for great
simplification of copper coordinates system, and David B. for testing my NEX files
at real board.

© Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT

v1.0 of example released at 2020-05-05.
