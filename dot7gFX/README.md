# dot7gFX

Home url: https://github.com/ped7g/ZXSpectrumNextMisc/tree/master/dot7gFX
(check for latest updates, source code, build instructions, etc)

"dot7gFX" should become set of "FX" (effects) dot-files for NextBASIC users.

ATM the plan is to produce only one "fade out" effect for Matt's DotJam event,
then later based on feedback and interest level maybe more effects will be added.

## fadeout "7gfx_fo.dot"

### what it does and how to use it

Fades out selected palette `<pal>` until all is black, in 7 steps.

The speed of the fade can be adjusted by `<delay>` option, default delay
is 6 frames per step, that is 6 * 7 = 42 frames in total, ~0.84s at 50Hz.

The faded palette is being uploaded at scanline 224 (below 320x256 area with
nextreg $64 (100) set to zero).

The fadeout effect takes about ~40 scanlines to calculate and apply palette
changes, so there should be plenty of CPU time left for your own routines
running in interrupt, like music player, etc.

After the dot command is finished, the selected palette is set completely
to black color, it's up to caller to modify it further.

#### SYNOPSIS

    ../7GFX_FO.DOT <pal> [<delay>]

#### ARGUMENTS

    pal = <letter u|l|s|t>[0|1] (default is first palette)
    delay = <integer 1..255> (default is 6)

#### EXAMPLES

fade second ULA palette in 21 frames (delay * 7): `../7GFX_FO.DOT U1 3`

fade first Layer2 palette with default delay 6: `../7GFX_FO.DOT L`

### how to build it

You will need [sjasmplus](https://github.com/z00m128/sjasmplus) (v1.20.0+).

Makefile is provided, but `sjasmplus fadeout.asm --raw=7gfx_fo.dot` should
be enough.

### how it works

First selected palette is read into working buffer `new_palette` as raw 512
bytes, in same format as the nextreg $44 (68) requires for upload of values.

This is copied and converted to `orig_palette` buffer with second byte
mirrored and priority bit cleared, forming nice 16bit value
`%rrrg'ggbb'b000'0000` with 9 bit color value in top bits.

Then in loop the working buffer is darkened and uploaded to nextreg $44 in
seven steps, waiting `<delay>` frames between each step.

The priority bit of Layer2 colors is left unmodified (also true for extra bits
in palettes for other layers, which in current cores 3.1.x don't matter, but
may become priority bits in future cores).

### This is NOT using Bresenham algorithm

Contrary to my original Layer2FadeOut example, this is **NOT** using Bresenham
algorithm, instead it is using the bit-trick from my classic ZX attribute
fade-out routine, where the color channel value is decremented when bit-mask
against original channel value is non-zero. The bitmasks used in 7 steps
are: 4,2,4,1,4,2,4.

Example for color (R = 7, G = 5, B = 1), *tR*,*tG*,*tB* means "test
original Red/Green/Blue channel value against bitmask if it is non-zero" and
*R*,*G*,*B* is resulting new value.


step | bitmask | tR | tG | tB | R | G | B
 :-: |   :--:  | :-:| :-:| :-:|:-:|:-:|:-:
  0  |   init  |  - |  - |  - | 7 | 5 | 1
  1  |    4    |  y |  y |  n | 6 | 4 | 1
  2  |    2    |  y |  n |  n | 5 | 4 | 1
  3  |    4    |  y |  y |  n | 4 | 3 | 1
  4  |    1    |  y |  y |  y | 3 | 2 | 0
  5  |    4    |  y |  y |  n | 2 | 1 | 0
  6  |    2    |  y |  n |  n | 1 | 1 | 0
  7  |    4    |  y |  y |  n | 0 | 0 | 0

As you can see, the channel values are fading out linearly, with Blue becoming
0 in the middle of the loop, not after first step.

So this should produce mostly identical result compared to Bresenham, but with
much cheaper computation. The bitmask 4 will cause total decrement -4 in channel
value, bitmask 2 will do -2, and bitmask 1 will do -1, the bitmasks/decrements
are interleaved to approximate linear fade out.

## thanks, author, license, ...

Big thanks to Matt for doing the DotJam event, which gave me extra incentive
to start this sub-project.

© Peter Helcmanovsky 2022, license: https://opensource.org/licenses/MIT

v0.1 of dot7gFX released at 2022-06-12.
