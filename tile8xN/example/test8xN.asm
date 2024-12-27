; ZX Spectrum Next - tilemodes 8xN, requires ZX Spectrum Next with core3.1.5+
; © Peter Helcmanovsky 2021, license: https://opensource.org/licenses/MIT
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.18.2+)
; The Makefile has the full-rebuild instructions
;
; This is example of tile8xN.i.asm usage/capabilites.
;

    ; setup Next device and sjasmplus syntax to my liking
        DEVICE ZXSPECTRUMNEXT : OPT reset --zxnext=cspect --syntax=abfw

    ; load example binary into 16ki_bank 15 at $C000 (used also mostly during execution)
        MMU $C000 $E000, 30, $C000      ; bank 15 to make example work as savenex/nexload test too
workBuffer:     DS      256     ; aligned buffer for temporary stuff (like displayedge)
im2Ivt:         DS      257, 1 + high im2Ivt
        ORG     ($FF00 & $) + 1 + high im2Ivt
im2Isr:
        ei
        ret

    ; include the libraries code
        INCLUDE "constants.i.asm"                           ; helper constants for ZX Next dev
        INCLUDE "../../displayedge/displayedge_rt.i.asm"    ; displayedge runtime library
        INCLUDE "../tile8xN.i.asm"                          ; tile8xN library

    ; definition of "display map", how many sub-windows should display, where and what part of tile-map
displayMap:
               ;tile8xN.SDisplayMap { skipLines,  Rows,   tilemapY,   xOffset }
.win_welcome:   tile8xN.SDisplayMap {         0,     9,          0 }
.win_space:     tile8xN.SDisplayMap {         6,     4,         10 }
.win_ofs_x:     tile8xN.SDisplayMap {         6,     4,         15 }
.win_bottom:    tile8xN.SDisplayMap {         6,     3,         73 }
.win_rest:      tile8xN.SDisplayMap {         6,    45,         24 }
        DB      -1

currentEdgePtr:
        DW      tile8xN.DisplayMarginsArr

currentRowHeight:
        DB      6

fakeMarginsArr:
        DS      dspedge.S_MARGINS * dspedge.MODE_COUNT, 24  ; fake 24px margins in all modes

    ; START of the example code
start:
    ; enable F8, F3 and Multiface, set 28MHz
        nextreg TURBO_CONTROL_NR_07,3
        ld      a,PERIPHERAL_2_NR_06
        call    dspedge.ReadNextReg
        or      %1010'1000
        nextreg PERIPHERAL_2_NR_06,a

    ; set up 4bpp 80x32 HW tilemode, clip window, palette, clear screen, offset reg $64=33, disable ULA
        call    tile8xN.InitVideo
        ; copy font data to Bank5
        ld      de,tile8xN.FONT_ADR
        ld      hl,font_src
        ld      bc,font_src.SZ
        ldir
        nextreg TRANSPARENCY_FALLBACK_COL_NR_4A,$E3         ; fallback colour to $E3 ("transparent")

    ; call display edge to read the system config into tile8xN.DisplayMarginsArr
        ; read default /sys/env.cfg file
        ld      hl,dspedge.defaultCfgFileName
        ld      de,tile8xN.DisplayMarginsArr
        ld      bc,workBuffer
        call    dspedge.ParseCfgFile    ; set array to -1 values even when error happens
        ; just ignore esxdos errors if reading cfg file fails

    ; init IM2 interrupt handler
        ld      a,high im2Ivt
        ld      i,a
        im      2

    ; init static parts of the tile map (print windows, various info texts, etc.. stuff which is permanent)
        call    drawStaticScreen

    ; main loop, checking HW for video mode changes and reconfiguring the copper code on-fly to accept new mode
    ; and reading keyboard and updating example screen based on that
mainLoop:
        ei
        halt

    ; check if copper needs re-init (for example due to video mode change or user controlling the demo)
        call    tile8xN.CopperNeedsReinit
        jr      z,.tilesAreOk
        call    tile8xN.SetCopperIsInitialized      ; clear the "needs re-init" flag
        ; get current video mode data
        ld      de,(currentEdgePtr)
        call    tile8xN.GetModeData
        ;       HLDE = L/R/T/B user defined margins (sanitized to 0..31 even if not found in cfg file)
        ;       C = pixel height of row "N" (6px in default mode)
        ;       B = fully visible text rows
        ;       A = remaining visible scanlines after last full row (0..N-1)
        ld      ix,displayMap
        ld      (ix+tile8xN.SDisplayMap.skipScanlines),d    ; (first item).skipScanlines = margin at top
        ; the last "rest" sub-window will be truncated by copper code generator to fit onto screen,
        ; so not patching rows here. But if you want to have extra sub-window at bottom, do the math...
        call    tile8xN.CopperReinit

    ; continuing in main loop, if there's no need to re-init copper code, or it was re-initialised
.tilesAreOk:
        ld      ix,displayMap           ; reset IX in case it was modified by CopperReinit
        ; handle all control keys
        call    handleKeysOP
        jr      nz,.forceReinit
        call    handleKeysAS
        jr      nz,.forceReinit
        call    handleKeysQWE
        jr      nz,.forceReinit
        call    handleKeyH
        jr      z,mainLoop
.forceReinit:
        ; force copper code re-init in next main loop iteration (because some config was modified)
        ld      a,dspedge.MODE_COUNT
        ld      (tile8xN.CopperNeedsReinit.CurrentMode),a
        jr      mainLoop

handleKeyH:
        ; handle H -> pixel height of row
        ld      a,~(1<<6)               ; seventh key-half-row: <enter>LKJH
        in      a,(ULA_P_FE)
        cpl
        and     %1'0000
        ret     z                       ; H is not pressed
        ld      a,(currentRowHeight)
        inc     a
        cp      9
        jr      c,.okHeight
        ld      a,4
.okHeight:
        ld      (currentRowHeight),a    ; new row height 4..8
        call    tile8xN.ChangeRowHeight ; reconfigure the library code
        call    highlightCurrentSelection
        jr      handleKeysQWE.waitForNoKey

handleKeysQWE:
        ; handle Q, W, E -> scroll vertically content in "rest" window or move "space" window up, E modifies edge data
        ld      a,~(1<<2)               ; third key-half-row: QWERT
        in      a,(ULA_P_FE)
        rra
        jr      c,.checkW
        ; decrement tilemapY of "rest" window down to 0
        ld      a,(ix+displayMap.win_rest.tilemapY-displayMap)
        or      a
        ret     z                       ; already at zero (no change)
        dec     (ix+displayMap.win_rest.tilemapY-displayMap)
        or      1
        ret                             ; ZF=0 enforced
.checkW:
        rra
        jr      c,.checkE
        ; move "space" window up
        ld      a,(ix+displayMap.win_space.skipScanlines-displayMap)
        or      a
        ret     z                       ; there are no more scanlines left above "space" window
        dec     (ix+displayMap.win_space.skipScanlines-displayMap)
        inc     (ix+displayMap.win_ofs_x.skipScanlines-displayMap)
        ret                             ; return with ZF=0 from INC
.checkE:
        rra
        jr      c,.checkDone
        ; alternate the edge-config pointer between tile8xN.DisplayMarginsArr and fakeMarginsArr
.alternateXor:  EQU     tile8xN.DisplayMarginsArr ^ fakeMarginsArr
        ld      hl,(currentEdgePtr)
        ld      a,low .alternateXor
        xor     l
        ld      l,a
        ld      a,high .alternateXor
        xor     h
        ld      h,a
        ld      (currentEdgePtr),hl     ; write new pointer
        call    highlightCurrentSelection
        ; wait until all keys are released
.waitForNoKey:
        xor     a
        in      a,(ULA_P_FE)
        cpl
        and     %1'1111                 ; A = 0 no key, 1..31 = some key pressed
        cp      1
        jr      nc,.waitForNoKey
        ret                             ; ZF=0 -> force new config
.checkDone:
        xor     a                       ; ZF=1 -> nothing modified
        ret

handleKeysAS:
        ; handle A, S -> scroll vertically content in "rest" window or move "space" window down
        ld      a,~(1<<1)               ; second key-half-row: ASDFG
        in      a,(ULA_P_FE)
        rra
        jr      c,.checkS
        ; increment tilemapY of "rest" window up to 70
        ld      a,(ix+displayMap.win_rest.tilemapY-displayMap)
        cp      70
        ret     z
        inc     (ix+displayMap.win_rest.tilemapY-displayMap)
        ret                             ; ZF=0 from INC
.checkS:
        rra
        jr      c,.checkDone
        ; move "space" window down
        ld      a,(ix+displayMap.win_ofs_x.skipScanlines-displayMap)
        or      a
        ret     z                       ; there are no more scanlines left below "space" window
        dec     (ix+displayMap.win_ofs_x.skipScanlines-displayMap)
        inc     (ix+displayMap.win_space.skipScanlines-displayMap)
        ret                             ; return with ZF=0 from INC
.checkDone:
        xor     a                       ; ZF=1 -> nothing modified
        ret

handleKeysOP:
        ; handle O/P -> scroll horizontally "ofs_x" window
        ld      a,~(1<<5)               ; sixth key-half-row: POIUY
        in      a,(ULA_P_FE)
        ld      c,(ix+displayMap.win_ofs_x.xOffset-displayMap)   ; C = x-offset 0..79 for "ofs_x" sub-window
        and     %11
        rra
        sbc     a,0                     ; convert pressed O/P to +1/0/-1 (+1 for P, -1 for O)
        add     a,c
        cp      80
        jr      c,.xOfsOk               ; 0..79 is ok
        add     a,80
        jr      c,.xOfsOk               ; -1 -> 79
        xor     a                       ; 80 -> 0
.xOfsOk:
        cp      (ix+displayMap.win_ofs_x.xOffset-displayMap)     ; compare if x-offset did change to get ZF flag
        ld      (ix+displayMap.win_ofs_x.xOffset-displayMap),a   ; write value (either new or old) x-offset
        ret

    ;-----------------------------------------------------------------------------------------------------------
    ; helper functions to draw content of example on screen

highlightCurrentSelection:
        ; set height values to light blue color: 4 5 6 7 8
        ld      bc,$052B
        call    tile8xN.CalcTileAddress
        inc     l                       ; HL = attribute of "4"
        ld      b,5
.loopHeightOff:
        ld      (hl),4<<4
        add     hl,4
        djnz    .loopHeightOff
        ; highlight currently selected height value
        ld      a,(currentRowHeight)
        add     a,a
        add     a,$2B-8
        ld      c,a
        ld      b,$05
        call    tile8xN.CalcTileAddress
        inc     l                       ; HL = attribute of current value
        ld      (hl),6<<4               ; set to light cyan
        ; set edge-config values to light blue color: user / fake
        ld      bc,$062A
        call    tile8xN.CalcTileAddress
        inc     l                       ; HL = attribute of "u"
        ld      bc,((4+3+4)<<8)+(4<<4)
        call    .loopEdgeSet
        ; highlight currently selected edge-config
        ld      bc,(4<<8)+(6<<4)        ; 4 chars, light cyan
        add     hl,-(4+3+4)*2           ; back to "u"
        ld      a,(currentEdgePtr)      ; I'm lazy, will compare only bottom byte
        ASSERT  low tile8xN.DisplayMarginsArr != low fakeMarginsArr     ; so they can't be equal, LUL
        cp      low tile8xN.DisplayMarginsArr
        jr      z,.loopEdgeSet          ; set + exit
        add     hl,2*(4+3)              ; switch to "f"
.loopEdgeSet:
        ld      (hl),c
        inc     hl
        inc     hl
        djnz    .loopEdgeSet
        ret

PrintAt:
    ; input:
    ;   DE = text address
    ;   following bytes after CALL instruction: column, row, color
        ex      (sp),hl
        ldi     bc,(hl)                 ; fake-ok ; ld c,(hl) : inc hl : ld b,(hl) : inc hl
        ldi     a,(hl)                  ; fake-ok ; ld a,(hl) : inc hl
        ex      (sp),hl
        jp      tile8xN.Print

drawFrame:
    ; In:
    ;   following bytes after CALL instruction: x 0..78, y 0..74, w 2..80, h 2..76, color 0..15 (sub-palette)
    ; Uses:
    ;   AF, HL, BC, DE
        ex      (sp),hl                 ; read arguments from the machine code after the call instruction
        ldi     bc,(hl)                 ; fake-ok ; ld c,(hl) : inc hl : ld b,(hl) : inc hl
        ldi     de,(hl)                 ; fake-ok ; ld e,(hl) : inc hl : ld d,(hl) : inc hl
        ldi     a,(hl)                  ; fake-ok ; ld a,(hl) : inc hl
        ex      (sp),hl
        ; start drawing
        call    tile8xN.CalcTileAddress ; HL = left corner address
        push    hl                      ; preserve left-top corner address
        push    de                      ; preserve size of frame
        swapnib
        ld      c,a                     ; colour
        ld      b,e
        call    .h_frame                ; top frame
        ld      b,d
        call    .v_frame                ; left frame
        push    hl
        ld      a,e
        dec     a
        add     a,a
        add     hl,a
        ld      b,d
        call    .v_frame                ; right frame
        pop     hl                      ; HL = top left corner
        ld      b,e
        ld      e,160
        dec     d
        mul     de
        add     hl,de                   ; HL = bottom left corner
        call    .h_frame                ; bottom frame
        ld      (hl),FNT_CHR_FRAME_BL
        pop     de                      ; DE = H:W again
        ld      a,e
        dec     a
        add     a,a
        add     hl,a
        ld      (hl),FNT_CHR_FRAME_BR
        pop     hl                      ; HL = top left corner
        ld      (hl),FNT_CHR_FRAME_TL
        add     hl,a
        ld      (hl),FNT_CHR_FRAME_TR
        ret
.h_frame:
        push    hl,,de
        ld      a,FNT_CHR_FRAME_H
        ld      de,2-1
        call    .draw_frame
        pop     de,,hl
        ret
.v_frame:
        push    hl,,de
        ld      a,FNT_CHR_FRAME_V
        ld      de,160-1
        call    .draw_frame
        pop     de,,hl
        ret
.draw_frame:
        ld      (hl),a
        inc     l
        ld      (hl),c
        add     hl,de
        djnz    .draw_frame
        ret

drawStaticScreen:
        ; draw light blue frame around whole virtual map ; 80x76 frame at [0,0]
        call    drawFrame : db 0, 0, 80, 76, 4
        ; draw line numbers on each line at left and right side
        ld      bc,7
.lNumLoop:
        push    bc
        ld      de,.lineNumTxt
        xor     a
        push    de,,bc
        call    tile8xN.Print
        pop     bc,,de
        ld      c,80-7-3
        xor     a
        call    tile8xN.Print
        pop     bc
        ; increment the line number in text (ASCII increment)
        ld      hl,.lineNumTxt+2
        call    .incrementDecimal
        inc     b
        ld      a,76
        cp      b
        jr      nz,.lNumLoop
        ; draw sub-frames and texts within them - "welcome" frame
        call    drawFrame : db 11, 1, 58, 8, 6
        ld      de,.txt_welcome
        call    PrintAt : db $0D, $02, 5
        call    PrintAt : db $0D, $03, 7
        call    PrintAt : db $0D, $04, 4
        call    PrintAt : db $0D, $05, 4
        call    PrintAt : db $0D, $06, 4
        call    PrintAt : db $0D, $07, 4
        ; highlight functional keys in "welcome" frame
        ld      bc,$050D
        call    tile8xN.CalcTileAddress
        inc     l
        ld      (hl),2<<4
        ld      a,160
        add     hl,a
        ld      (hl),2<<4
        add     hl,a
        ld      (hl),2<<4
        ld      a,4
        add     hl,a
        ld      (hl),2<<4
        ; highlight default selections
        call    highlightCurrentSelection

        ; draw sub-frames and texts within them - "space" frame
        call    drawFrame : db 11, 10, 58, 4, 6
        ld      de,.txt_space
        call    PrintAt : db $0D, $0B, 5
        call    PrintAt : db $0D, $0C, 4
        ; highlight functional keys in "space" frame
        ld      bc,$0C0D
        call    tile8xN.CalcTileAddress
        inc     l
        ld      (hl),2<<4
        ld      a,4
        add     hl,a
        ld      (hl),2<<4

        ; draw sub-frames and texts within them - "ofs_x" frame
        call    drawFrame : db 11, 15, 58, 4, 6
        ld      de,.txt_ofs_x
        call    PrintAt : db $0D, $10, 5
        call    PrintAt : db $0D, $11, 4
        ; highlight functional keys in "ofs_x" frame
        ld      bc,$110D
        call    tile8xN.CalcTileAddress
        inc     l
        ld      (hl),2<<4
        ld      a,4
        add     hl,a
        ld      (hl),2<<4

        ; draw texts at "bottom" (no frame for these)
        ld      de,.txt_bottom
        call    PrintAt : db $0B, $49, 5
        call    PrintAt : db $0B, $4A, 4
        call    PrintAt : db $00, $4C, 4

        ; draw "copyright" texts
        ld      de,.txt_copyright
        call    PrintAt : db $0B, $14, 4
        call    PrintAt : db $0B, $15, 4
        call    PrintAt : db $0B, $16, 2

        ; draw "info" texts
        ld      de,.txt_info
        call    PrintAt : db $0B, $18, 4
        call    PrintAt : db $0B, $19, 4
        call    PrintAt : db $0B, $1A, 4
        call    PrintAt : db $0B, $1B, 4
        call    PrintAt : db $0B, $1D, 0    ; +1 Y to make one blank line between
        call    PrintAt : db $0B, $1E, 0

        ; draw "font" frame
        call    drawFrame : db 11, 32, 34, 6, 6
        ld      de,.txt_font
        call    PrintAt : db $0D, $20, 5
        ld      bc,$210C
        call    tile8xN.CalcTileAddress
        xor     a
.fullFontLoop:
        ld      (hl),a
        inc     l
        inc     hl
        inc     a
        test    31
        jr      nz,.fullFontLoop
        add     hl,160-32*2
        or      a
        jp      p,.fullFontLoop

        ; draw "palette" frame
        call    drawFrame : db 47, 32, 22, 18, 6
        ld      de,.txt_palette
        call    PrintAt : db $31, $20, 5
        ld      bc,$2130
        xor     a
.fullPalLoop:
        push    af,,bc
        call    tile8xN.Print
        pop     bc,,af
        inc     b
        inc     a
        test    15
        jr      nz,.fullPalLoop

        ; draw "filler" (7 Gods logo ASCII art)
        ld      de,.txt_filler
        ld      bc,$310B
        ld      a,.txt_filler_lines
.fillerLoop:
        push    af,,bc
        ld      a,4
        call    tile8xN.Print
        pop     bc,,af
        inc     b
        dec     a
        jr      nz,.fillerLoop

        ; draw sub-frames and texts within them - "sjasmplus" frame
        call    drawFrame : db 11, 39, 34, 4, 6
        ld      de,.txt_sjasmplus
        call    PrintAt : db $0D, $28, 4
        call    PrintAt : db $0D, $29, 2
        call    PrintAt : db $0D, $2D, 4

        ret

.incrementDecimal:
        inc     (hl)
        ld      a,(hl)
        cp      '0'+10
        ret     c
        ld      (hl),'0'
        dec     hl
        inc     (hl)
        ret

.lineNumTxt:
        DZ      "L00"

.txt_welcome:
        DZ      "tile8xN test/example, showing tilemode with 8xN tiles"
        DZ      "the regular 80x32 HW tilemode is squished by copper"
        DZ      "with support for sub-windows and displayedge config"
        DZ      "H to change row pixel height: 4 5 6 7 8 (8x6 font)"
        DZ      "E to use displayedge config: user / 24px (fake)"
        DZ      "Q/A to scroll last sub-window over 80x76 tile-map"

.txt_space:
        DZ      "space between sub-windows can be adjusted by +-1 line"
        DZ      "W/S to move this window up and down between others"

.txt_ofs_x:
        DZ      "sub-window can also scroll/wrap horizontally per char"
        DZ      "O/P to scroll this window horizontally"

.txt_bottom:
        DZ      "sub-window can start at any line in the 80x76 tile-map"
        DZ      "this one shows very bottom of map: L73..L75"
        DZ      "tile pixel data L76..L102:"

.txt_copyright:
        DZ      "tilemodes 8xN for ZX Spectrum Next (core 3.1.5+)"
        DZ      $7F," Peter Helcmanovsky 2021, MIT license, source at:"
        DZ      "https://github.com/ped7g/ZXSpectrumNextMisc"

.txt_info:
        DZ      "This is meant mostly as support library for Text UI-like"
        DZ      "tools, offering easy way to have 80x32, 80x36 and 80x42"
        DZ      "text modes. But you can of course take the copper code"
        DZ      "generator and adjust it for other purposes."
        DZ      "Rest of the map is filled with *some* stuff to make"
        DZ      "scrolling through it less boring and show the full font."

.txt_font:
        DZ      " 8x6 font by Ped & Hadiak "

.txt_palette:
        DZ      " 16 colour sets "
        DZ      " 0 white            "
        DZ      " 1 inverse 0        "
        DZ      " 2 bright white     "
        DZ      " 3 inverse 2        "
        DZ      " 4 light blue       "
        DZ      " 5 light green      "
        DZ      " 6 light cyan       "
        DZ      " 7 light yellow     "
        DZ      " 8 white selected   "
        DZ      " 9 br.white on red  "
        DZ      "10 br.white selected"
        DZ      "11 white on grey    "
        DZ      "12 light blue sel.  "
        DZ      "13 light green sel. "
        DZ      "14 light cyan sel.  "
        DZ      "15 light yellow sel."

.txt_filler:
        DZ      "                 ''  '.'''''   '"
        DZ      "           ''' ''.             .''  ' '"
        DZ      "         '   ''                   ''   '"
        DZ      "         .'      '''''''''''''''       ."
        DZ      "       '''    .-..'''''''''''''''''    '''"
        DZ      "   '' ''    .-.              ''    ''    '' ''"
        DZ      "   '      '-''''''''''''.' '''       ''      '"
        DZ      "    .    '.'         '''''''          ''    .'"
        DZ      "   ''   ''         '' '''              ''   ''"
        DZ      " ''.    '       '''''''                 ''   '''"
        DZ      "'      ''     '' '''                     .      '"
        DZ      " ''    .    ''  .''                      .    ''"
        DZ      "  ''   '   '  '-''''''''''' .''''''''''''.   ''"
        DZ      "  '.   '' '   -.....'-'.''.'.'.'.'.....'-.   ''"
        DZ      " '''    .''   -.'''''' ''''. '''''''''''-'    ''"
        DZ      " ''     '-.    ''                      ''      '"
        DZ      "   '''   ':.   '     7gods.org        ''   ''''"
        DZ      "     '.    ..''                     ''    ''"
        DZ      "      .     '''                   '''     ."
        DZ      "      '        ''''           ''''        '"
        DZ      "       ' ''''       ''''''''''      ''''"
        DZ      "            '.                     .'"
        DZ      "             ''  ''''''   ''''''  ''"
        DZ      "                      '   '"
.txt_filler_lines:  EQU     24

.txt_sjasmplus:
        DZ      "Z80 assembler sjasmplus:"
        DZ      "github.com/z00m128/sjasmplus"
        DZ      FNT_CHR_DOT_RED, FNT_CHR_DOT_YELLOW, FNT_CHR_DOT_GREEN, " greetings to demo scene ", FNT_CHR_DOT_GREEN, FNT_CHR_DOT_YELLOW, FNT_CHR_DOT_RED

    ; include font data
font_src:
        INCLUDE "tilemap_font_8x6.i.asm"  ; 8x6 font made by Hadiak and Ped, copyleft (feel free to adjust/change)
font_src.SZ: EQU $-font_src

    ; stack space
        DS      256
stack:

    ; save the NEX file
        SAVENEX OPEN "test8xN.nex", start, stack, $$start>>1, 2
        SAVENEX CORE 3,1,5 : SAVENEX CFG 0
        SAVENEX AUTO : SAVENEX CLOSE
        CSPECTMAP "test8xN.map"

    ; launch it in emulator on my personal machine, if assemble with define LAUNCH_EMULATOR=1
        IFNDEF LAUNCH_EMULATOR : DEFINE LAUNCH_EMULATOR 0 : ENDIF
        IF 0 == __ERRORS__ && 0 == __WARNINGS__ && 1 == LAUNCH_EMULATOR
            SHELLEXEC "( sleep 0.1s ; runCSpect -brk -map=test8xN.map test8xN.nex ) &"
        ENDIF

    ; end of file: test8xN.asm
