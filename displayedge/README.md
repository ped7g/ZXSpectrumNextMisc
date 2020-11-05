# "runtime" library to read+parse the "sys/env.cfg"

You can include the file "displayedge_rt.i.asm" in your own project and call the provided
API calls to read the user's configuration.

The cfg file is ordinary ASCII text file, see included test.cfg for details, so user can
also edit the values in any text editor (recommended end-of-line is MS-DOS "CRLF").

## using "runtime" as source code included into your project

If you are using sjasmplus, you can use the runtime library like this:

        ORG xyz     ; where you want to place the runtime code
        INCLUDE "displayedge_rt.i.asm"

        ; ... in data area, reserve space ...
    DisplayMarginsArray:    DS      dspedge.S_MARGINS * dspedge.MODE_COUNT  ; 4 * 9 = 36
            ; if the app reacts dynamically to video mode changes, preserve this array

                            ALIGN   256
    ParsingBuffer:          DS      256     ; 256B buffer for parsing file, aligned ($xx00)
            ; this buffer is used temporarily by ParseCfgFile and can be re-used afterward

        ; .. in your init code, when starting the app ...

            ; optional - read local cfg in the app directory (if your sw supports local config)
                ld      hl,yourAppLocalCfgName
                ld      de,DisplayMarginsArray
                ld      bc,ParsingBuffer
                call    dspedge.ParseCfgFile    ; set array to -1 values even when error happens
                jr      nc,NoEsxError_allDone
                ; esx error, check if it is "file does not exist" - no local config
                cp      5                       ; esx_enoent
                jr      nz,someEsxError         ; some other error happened
                ; continue to read the default global config instead

            ; regular global config

                ; read default /sys/env.cfg file
                ld      hl,dspedge.defaultCfgFileName
                ld      de,DisplayMarginsArray
                ld      bc,ParsingBuffer
                call    dspedge.ParseCfgFile    ; set array to -1 values even when error happens
                jr      c,someEsxError          ; A = esx error number

        ; .. in your gfx init code or mainloop (if you watch for mode changes)
            call    dspedge.DetectMode      ; A = current video mode
            ; (optional) check if mode did change, or you need to init margins
            ; A = video mode you want to get margins for
            ld      de,DisplayMarginsArray
            call    dspedge.GetMargins      ; B C D E = left right top bottom
            ; if the mode is not stored in the file yet, 4x 255 is returned

            ; use the values to adjust your drawing routines and screen layout

You can also check the `displayedge.asm` of the dot command to see the runtime code
in the action.

## using "runtime" as binary library in your project

Use sjasmplus to build the binary (set the ORG address to what you need):

    sjasmplus -DDISPLAYEDGE_ORG_ADR=0x8000 --exp=displayedge_rt.exp displayedge_rt.i.asm

This will produce the `displayedge_rt.bin` and `displayedge_rt.exp` files, which you
can then use in your favourite assembler, for example in sjasmplus it would look like:

        INCLUDE "displayedge_rt.exp"        ; include the symbol values
        ORG dspedge.Begin                   ; DISPLAYEDGE_ORG_ADR
        INCBIN "displayedge_rt.bin"         ; include the machine code of library

        ; the actual usage will be similar to the example above
        ; just adjust syntax for your assembler


# ZXNext dot command DISPLAYEDGE to edit "sys/env.cfg" visually

This is "dot command" for NextZXOS running at TBBlue (aka ZXNext) board (does use tiles
video-mode for graphics).

It lets you edit visually display margins - the visible area of your display in particular
video mode - and store that in the config file. Any SW aware of this config file can then
read it upon start, and shrink the playfield to display everything important within the
area of screen which is visible on your display.

Use the controls (F and T) to switch to the video mode you want to edit margins for (the
machine type/timing can be switched only in VGA mode, VGA<->HDMI switch must be done
while restarting machine by user).

Then edit the values (screen corners) until the "green frame" is visible well on your
display, then save the config.

The controls are:

    SPACE  : select active corner of display (top left vs bottom right)
    arrows : edit visible position of the active corner ("cursor joystick")
    (also Kempston/MD controller can be used, fire then selects corner)
    Q      : exit back to NextZXOS
    R      : reload the cfg file currently stored on disk (discards any changes)
    S      : save the currently modified values to cfg file
    F      : change between 50Hz/60Hz mode (also regular "F3" key works)
    T      : in VGA modes you can switch between different video-mode timings
    (Q/R/S/F/T usually requires confirmation by pressing "Y" after)
