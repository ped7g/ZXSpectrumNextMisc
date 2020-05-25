# Fade out example

## what it does

Fades out image in Layer2 mode 320x256x8bpp, by updating palette entries until all is
black.

The speed of the fade can be adjusted by keys 1,2,3,..,7,8,9,0 (press idle screen
to set new duration of effect). The duration selection is (in number of video frames):
7, 12, 19, 28, 39, 52, 67, 84, 103, 124

## how to build it

You will need [sjasmplus](https://github.com/z00m128/sjasmplus) (v1.15.0+).

Makefile is provided with extra build tasks if you want to do more things, to modify the
image data you will need to provide 320x256 8bit indexed PNG in default Layer2 palette,
and have image-magick tools installed (for the "convert" command converting the PNG into
TGA file, which is used in ASM file directly).

If you will modify the code to include also custom palette and set it up before the code
starts, the effect will read it back from next registers and do the fade-out on your
custom palette.

## how it works

The code reads current Layer2 palette and stores it into memory (in swapped byte order
as optimization for the fade-out calculation).

Then it calculates init-values for Bresenham interpolation algorithm to make the fade-out
linear across the desired number of video frames, and starts looping the interpolation.

The interpolation will end after N frames with all palette items set to black color
(maintaining the priority bit unmodified).

After the interpolation there is 0.5s pause keeping the screen black, then it restores
original colours, and displays original image for another 0.5s.

During the pauses the wait routine will scan keyboard and adjust effect-duration
depending on the key pressed (the number keys: 1,2,3,..,8,9,0).

The minimum effect duration is 7 frames as it can decrease color channel value only
by -1 in single processing loop, and maximum bright white is \[7,7,7\].

## Bresenham algorithm

It is often used to draw lines, but it can be used for any linear interpolation when
you need to modify value "x" by "dY" steps across "dX" iterations.

For example when initial color is \[1,6,3\] and effect duration is 50 frames, then
the Bresenham is used for each color channel separately, the "dX" is 50 for all of
them, and the "dY" are 1, 6 and 3 (turning \[1,6,3\] into \[0,0,0\] after 50 frames).

Now during each interpolation the value "D" is being used to decide in which frame the
value should be modified and/or the modification skipped. The D is initialized as:
D = -dx -1 (-51 in this example), and "deltaD" is the value added to D every frame:
deltaD = 2*dy (2, 12, 6 in the example).

Every frame for every color channel the calculation D = D + deltaD is done, if D becomes
greater or equal than zero, the color channel value is modified (decremented by one),
and D is adjusted by -2*dx (-100 in example).

So for the red channel with color value 1 the "D" is progressing as -51, -49, -47, ...
-3, -1, (here the color value is decremented from 1 to 0) -99, -97, -95, ...

And for the green channel with color value 6 the "D" is progressing as -51, -39, -27,
-15, -3, (here the color value is decremented from 6 to 5) -91, -79, -67, ...

For even more theory check [the wiki page](https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm).

## thanks, author, license, ...

Big thanks to Shrek128 for testing the example with real board.

© Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT

v1.0 of example released at 2020-05-25.
