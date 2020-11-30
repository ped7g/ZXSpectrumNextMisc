# Show 512 colors example

## what it does

Displays all 512 colours of ZX Spectrum Next in 256x192 Layer2 (as 8x8 pixel squares), using copper code to modify palette on the fly and avoid transparency-hiding-two-shades.

There are four different orderings of the colours, with the fourth screen showing only subset of 188 colours instead of full 512, but in some kind of "human" grouping (each screen shows the same 512 colours, as there are no other available on ZX Next).

Press "i" to change the sorting of colours, press cursors (+delete key) to read details about particular square, or move mouse around and click to select square.

## how to build it

You will need [sjasmplus](https://github.com/z00m128/sjasmplus), but with the new `ELSEIF` feature (currently released v1.17.0 does not have it yet, to be released in next version). Meanwhile you will have to build the sjasmplus from github sources.

Then in command line: `sjasmplus show512.asm`

## how it works

There are 8x8 pixel squares drawn with index 255 to 0, twice. Code does set up both primary and secondary Layer 2 palettes (256 + 256 = total 512 colours).

Then small copper code is alternating the palettes for upper/bottom half of the palette, also adjusting global transparency register value to make it never match (as transparency is only 8bit comparison, so it would hide two 9bit colors, making possible to display only 510 colours with one transparency fallback - by avoiding the transparency the full 512 distinct colours are displayed at the same screen).

## thanks, author, license, ...

The font is "Envious" by DamienG https://damieng.com/zx-origins

© Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT

v1.2 of example released at 2020-11-15.
