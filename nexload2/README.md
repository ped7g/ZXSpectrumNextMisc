# ZXNext dot command to load NEX files

This is "dot command" for NextZXOS running at TBBlue (aka ZXNext) board (there's basically
no point to try it to run at different HW or with different OS, the whole functionality
is ZXNext-specific).

This is an alternative to the regular NEXLOAD from the main distribution, slightly
ahead on features - supporting the V1.3 of [NEX file format](https://wiki.specnext.dev/NEX_file_format).

Why rewriting it from scratch in the first place? I thought the original NEXLOAD source
can be better structured and simpler, made me curious how my version would look, so
I wrote one. Whether I achieved "nicer" source is probably very subjective.

But at least I could easily reuse this project to prototype the V1.3 features without
interfering with the main branch just around the hectic period of stabilizing the distribution
for cased Nexts shipping.

The V1.3 NEX files can be produced with [sjasmplus assembler](https://github.com/z00m128/sjasmplus/releases/latest)
since v1.14.4 (but you can produce still also V1.2 files with it).

I hope this will eventually get included in the main distro (either by re-implementing V1.3
in NEXLOAD or by adding NEXLOAD2 to the distro).
