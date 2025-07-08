# ZXSpectrumNextMisc
[![Build Status](https://api.cirrus-ci.com/github/ped7g/ZXSpectrumNextMisc.svg)](https://cirrus-ci.com/github/ped7g/ZXSpectrumNextMisc/master)
[![GitHub repo size in bytes](https://img.shields.io/github/repo-size/ped7g/ZXSpectrumNextMisc.svg)](https://github.com/ped7g/ZXSpectrumNextMisc/)
[![MIT License](https://img.shields.io/github/license/ped7g/ZXSpectrumNextMisc.svg)](https://github.com/ped7g/ZXSpectrumNextMisc/blob/master/LICENSE)

Miscellaneous small unrelated pieces of source code for ZX Spectrum Next

Sub-project | Description
----------- | -----------
displayedge | dot command `.displayedge` to check visible area of your ZX Next display, storing the information into global config file + runtime library to read the config and use user's values in your own SW to narrow down used screen space by your SW.
snippets | various small Z80N code snippets - ready to be reused in your projects or just stepping through them in debugger to exercise your Z80 assembly skills
Layer2BigPic | layer2 320x256 8 direction (mostly HW) scrolling of big 640x512 image
Layer2FadeOut | layer2 palette "fade out" effect doing linear interpolation for 256 RGB elements at 60 FPS
ShowAll512Colors | displays all possible ZX Next colours (512 of them) at single screen, use mouse or keyboard to read colour values
ReadingAtariDrivingController | test utility and code example for Atari "driving" paddle controller (the digital one!)
Z80_ISA_tools | [Z80N instruction HTML table](http://ped.7gods.org/Z80N_table_ClrHome.html), code to calculate opcode length (Z80 asm and C source for both Z80N and Z80)
nexload2 | from-scratch rewrite of official NEXLOAD - is offered as replacement to core team (as it should be [subjectivelly] better source and easier to maintain), also I used it to prototype and test NEX format extensions suggested for "V1.3", but none of that happened, so at this moment this is unofficial extension (please do not release Next SW using my V1.3 extensions)
tile8xN | copper adjusted HW tilemode to have 8xN tiles instead of 8x8 (reasonable N: 4,5,6,7,8), with sub-window logic and virtual tile-map 80xM lines

Licensing details: you may find few rare files included which were not created by me. Some font files are by DamienG and should be marked as such in the code including them with their respective licensing terms, ClrHome web page (used as base for Z80 instruction table) has no copyright information, but their current github repository has parts of it under MIT license. In future I may eventually include some example/gfx/font files from various sources, hopefully always attributing them correctly. But generally speaking any Z80 code in this repository is very likely my own work and the MIT license does apply (as main license). If for your use case that's still too limiting, you can use my code also under [The Unlicense](https://unlicense.org/) as alternative to MIT license (I'm listing this option explicitly in "snippets", but you can apply it to my other code in this repository too). If you are unsure about origin of some code, check comments in code and git history of the file how it originated or open github issue to clarify with me.
