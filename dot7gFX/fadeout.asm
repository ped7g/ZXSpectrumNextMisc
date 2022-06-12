; dot7gFX "Fade Out" v0.1 2022-06-12, fading out chosen palette to black
; © Peter Helcmanovsky 2022, license: https://opensource.org/licenses/MIT
; requires ZX Spectrum Next with core3.0+
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.20.0+)
; The Makefile has build commands, but it should be enough to just run:
;   sjasmplus fadeout.asm --raw=7gfx_fo.dot
;
; TODO:
; - add optional input handling to skip delay (keyboard, joysticks, CLI option)
; - add option to preserve particular color (transparency by index, not by value, sry)
;

        OPT reset --zxnext --syntax=abf
        INCLUDE "constants.i.asm"
        ORG $2000

;----------------------------------------------------------------------------------------------------------
startDot:
        ; preserve current CPU speed, and set up 28MHz for fadeout
            ld      a,TURBO_CONTROL_NR_07
            call    ReadNextReg
            and     $0F
            ld      (exit.speed),a
            nextreg TURBO_CONTROL_NR_07,3               ; 28MHz
        ; preserve current palette control (nextreg $43)
            ld      a,PALETTE_CONTROL_NR_43
            call    ReadNextReg
            ld      (exit.palCt),a

        ; parse command line options
            call    parseCommandLine
            jr      c,exit                              ; CF=1 signals error, don't run effect

        ; read chosen palette (current values) and prepare the working copy of it
            call    readCurrentPalette

        ; do the fade-out effect
fadeLoop:
        ; calculate one step of fade-out for next palette upload
            call    FadeColors
        ; delay N frames and sync with display beam at line 224
            ld      hl,(delay)                          ; L = delay value
.delayLoop: call    WaitForScanline224
            dec     l
            jr      nz,.delayLoop
        ; upload new palette
            call    setNewColors
        ; check mask_data pointer, if at .end, that was last upload (all black)
            ld      a,(FadeColors.mdptr)
            cp      low mask_data.end
            jr      nz,fadeLoop

        ; exit back to NextZXOS (restore important next registers)
exit:
.palCt+3:   nextreg PALETTE_CONTROL_NR_43,0             ; self-modify ; restore palette control
.speed+3:   nextreg TURBO_CONTROL_NR_07,0               ; self-modify ; restore CPU speed
            or      a                                   ; clear CF to not signal error to NextZXOS
            ret

;----------------------------------------------------------------------------------------------------------
mask_data:  ; shift, LSB, MSB
            DB 2 : DW %100'100'100'0000001              ; bitmask 4 per each channel (lsb = 1 for loop terminator)
            DB 1 : DW %010'010'010'0000001              ; bitmask 2 per each channel
            DB 2 : DW %100'100'100'0000001
            DB 0 : DW %001'001'001'0000001              ; bitmaks 1 per each channel
            DB 2 : DW %100'100'100'0000001
            DB 1 : DW %010'010'010'0000001
            DB 2 : DW %100'100'100'0000001
.end:
            ASSERT high $ == high mask_data             ; must fit into same 256B page

;----------------------------------------------------------------------------------------------------------
ReadNextReg:
        ; reads nextreg in A into A (does modify currently selected NextReg on I/O port)
            ld      bc,TBBLUE_REGISTER_SELECT_P_243B
            out     (c),a
            inc     b                                   ; bc = TBBLUE_REGISTER_ACCESS_P_253B
            in      a,(c)                               ; read desired NextReg state
            ret

;----------------------------------------------------------------------------------------------------------
WaitForScanline224:                                     ; wait for scanline 224 (just below 320x256 area)
        ; read NextReg $1F - LSB of current raster line (select the $1F register on I/O port)
            ld      a,VIDEO_LINE_LSB_NR_1F
            call    ReadNextReg                         ; also sets BC = TBBLUE_REGISTER_ACCESS_P_253B
        ; if already at scanline 224, then wait extra whole frame (for super-fast game loops)
.cantStartAt224:
            in      a,(c)                               ; read the raster line LSB
            cp      224
            jr      z,.cantStartAt224
        ; if not yet at scanline 224, wait for it ... wait for it ...
.waitLoop:
            in      a,(c)                               ; read the raster line LSB
            cp      224
            jr      nz,.waitLoop
        ; and because the max scanline number is between 260..319 (depends on video mode),
        ; I don't need to read MSB. 256+224 = 480 -> such scanline is not part of any mode.
            ret

;----------------------------------------------------------------------------------------------------------
FadeColors:
        ; calculate new version of palette data doing single step of fade-out
            ASSERT orig_palette == 513 + new_palette && (1 & orig_palette)      ; verify code-assumptions
        ; fetch mask data for this iteration
.mdptr+1:   ld      hl,mask_data
            ld      b,(hl)                              ; mask_shift
            inc     l
            ld      c,(hl)                              ; mask_LSB
            inc     l
            ld      a,(hl)                              ; mask_MSB
            inc     l
            ld      (.maskMSB),a
            ld      (.mdptr),hl
        ; do the fade-out in memory buffers
            ld      hl,orig_palette + 511
            ; HL = second byte of orig_palette item, B = mask_shift, C = mask_LSB
.loop:
        ; prepare DE = (original_color_word & mask) >> mask_shift
            ld      a,(hl)                              ; orig_palette second byte (mirrored already) or terminator
            dec     hl                                  ; this one must be --HL, as it can cross 256B boundaries
            and     c                                   ; mask LSB
            ld      e,a
            rra
            ret     c                                   ; if (1 & orig_palette & mask) -> terminator of loop
            ld      a,(hl)                              ; orig_palette first byte (MSB)
            dec     h
            dec     h                                   ; HL = new_palette second byte (HL -= 512)
.maskMSB+1: and     $AA                                 ; mask MSB
            ld      d,a                                 ; DE = original_color_word & mask
            bsrl    de,b                                ; B = 0,1,2 for bitmask 1,2,4
            ; DE = value to subtract from current color

        ; adjust current color in memory buffer
            ld      a,(hl)                              ; extra 9th bit of color + priority bits
            mirror                                      ; mirror it so blue0 becomes b7 in color LSB
            sub     e                                   ; subtract 1 or 0 from blue (0x80 or 0x00)
            mirror                                      ; mirror it back to %p---'---B form for palette, CF preserved
            ld      (hl),a                              ; store the new value in new_palette array
            dec     l                                   ; move to 8b color byte (color MSB), CF preserved
            ld      a,(hl)
            sbc     a,d
            ld      (hl),a
            inc     h
            inc     h                                   ; back to orig_palette area (HL += 512)
            jp      .loop

;----------------------------------------------------------------------------------------------------------
setNewColors:
        ; upload modified palette to FPGA
            nextreg PALETTE_INDEX_NR_40,0               ; select color 0 before loop
            ; select nextregister $44 at nextreg I/O port
            ld      bc,TBBLUE_REGISTER_SELECT_P_243B
            ld      a,PALETTE_VALUE_9BIT_NR_44
            out     (c),a
            inc     b                                   ; BC = TBBLUE_REGISTER_ACCESS_P_253B
            ld      hl,new_palette
            ld      a,512/16                            ; total payload is 512 bytes
.loop:      ; unroll 16x outinb
            .16 outinb
            dec     a
            jp      nz,.loop
            ret

;----------------------------------------------------------------------------------------------------------
readCurrentPalette:
        ; select the target palette
            ld      a,(exit.palCt)
            and     $0F
            ld      l,a
            ld      a,(palette)
            or      l
            nextreg PALETTE_CONTROL_NR_43,a             ; select desired palette
        ; read current palette values into new_palette array
            ld      l,0                                 ; repeat counter (0 == 256x)
            ld      de,new_palette
.readPaletteLoop:
        ; format is same as HW, first byte is 8bit %RRRGGGBB, second byte is %pp-----B
            ld      a,l
            nextreg PALETTE_INDEX_NR_40,a               ; select color
            ld      a,PALETTE_VALUE_NR_41
            call    ReadNextReg                         ; read RRRGGGBB part of color
            ld      (de),a
            inc     de
            ld      a,PALETTE_VALUE_9BIT_NR_44
            call    ReadNextReg                         ; read p000000B part of color
            ld      (de),a
            inc     de
            inc     l
            jr      nz,.readPaletteLoop
        ; setup orig_palette terminator value
            ld      a,1
            ld      (de),a
            inc     de
        ; prepare orig_palette data, here DE == orig_palette
            ld      hl,new_palette
            ld      bc,257
.prepareOrigLoop:
        ; format is: %RRRGGGBB, %B000'000t -> second byte is mirrored and priority bits removed (b0 is loop terminator)
            ldi                                         ; copy first byte as is (does one junk byte after loop is done)
            ret     po                                  ; whole orig_palette is set up
            ld      a,(hl)                              ; second byte needs mirroring and to remove priority bits
            inc     hl
            mirror  a
            and     $80
            ld      (de),a
            inc     de
            jr      .prepareOrigLoop

;----------------------------------------------------------------------------------------------------------
printMsg:
        ; print zero terminated string from HL by using `rst $10` (to be compatible with any user mode)
            ld      a,(hl)
            inc     hl
            and     $7F                     ; clear 7th bit
            ret     z                       ; exit if terminator ($00 or $80)
            rst     $10
            jr      printMsg

;----------------------------------------------------------------------------------------------------------
isWhiteOrEol:
        ; returns:
        ;   ZF=1, CF=0 : the char in A is space or EOL-like
        ;   ZF=0, CF=0 : other char
            cp      ' '
            ret     z
.eolOnly:
            cp      ':'
            ret     z
            cp      13
            ret     z
            cp      10
            ret     z
            or      a
            ret

;----------------------------------------------------------------------------------------------------------
skipWhite:
        ; returns:
        ;   CF=0 : A = non-whitespace char, HL points after it
        ;   CF=1 : end of line was reached without any non-whitespace char
.loop:  ; skip through spaces
            ld      a,(hl)
            inc     hl
            cp      ' '
            jr      z,.loop
        ; check for EOLs: 0, 10, 13, ':'
            call    isWhiteOrEol.eolOnly
            ret     nz                                  ; non-white char, return with CF=0 and char in A
            scf
            ret                                         ; signal EOL by CF=1

letterPal:  DB      "ulst"

;----------------------------------------------------------------------------------------------------------
parseCommandLine:
            ld      a,h
            or      l
            jr      z,displayHelp                       ; 0 == HL -> empty command line, display help
        ; some command line is available, try to parse it
            call    skipWhite
            jr      c,displayHelp                       ; encountering end of line too soon
        ; select palette by the letter
            or      $20                                 ; lowercase it
            ex      de,hl
            ld      hl,letterPal
            ld      bc,5                                ; +1 length to run beyond the buffer in case of mismatch
            cpir
            ld      a,l
            sub     1 + low letterPal                   ; A = 0,1,2,3,4 for [u,l,s,t,<other>]
            cp      4
            jr      nc,displayHelp                      ; invalid palette letter, display help
            ex      de,hl
            ld      c,a                                 ; C = 0,1,2,3 -> palette select (will become bits 5-4 for NR $43)
        ; check if there is optional digit 0/1 to select first/second palette
            ld      a,(hl)
            sub     '0'
            jr      c,.not_a_01digit
            cp      2
            jr      nc,.not_a_01digit
        ; A = 0/1 depending on the digit 0/1 in command line
            inc     hl                                  ; '0'/'1' char accepted
        .2  add     a,a                                 ; A<<2
            or      c
            ld      c,a                                 ; C = palette select with future bit 6 (first/second palette)
.not_a_01digit:
        ; check if there is whitespace or EOL after palette option (otherwise display help)
            ld      a,(hl)
            call    isWhiteOrEol
            jr      nz,displayHelp
        ; convert the palette select value to final form and store it in variable
            ld      a,c
            swapnib
            ld      (palette),a
        ; check if there is optional <delay> argument
            call    skipWhite
            ccf
            ret     nc                                  ; that was all, done, return with CF=0 signalling OK
        ; non-white char in A, parse it as integer
            ld      e,0
.parseDelay:
            sub     '0'
            jr      c,displayHelp                       ; non-digit char, display help
            ld      d,10
            cp      d
            jr      nc,displayHelp                      ; non-digit char, display help
            mul     de                                  ; E *= 10
            add     de,a                                ; DE += digit
            ld      a,d
            or      a
            jr      nz,displayHelp                      ; integer overflow, display help
        ; parse next char, should be digit or white/eol
            ld      a,(hl)
            inc     hl
            call    isWhiteOrEol
            jr      nz,.parseDelay                      ; some char, could be digit, check it
            dec     hl                                  ; space or EOL, revert ++HL for final check
        ; <delay> parsed, store it
            ld      a,e
            or      a
            jr      z,displayHelp                       ; only values 1..255 are valid
            ld      (delay),a
        ; check if EOL can be reached
            call    skipWhite
            jr      nc,displayHelp                      ; something unexpected on the remaining line, display help
        ; all parsed, all OK, clear CF -> run the effect
            or      a
            ret

;----------------------------------------------------------------------------------------------------------
displayHelp:
            ld      hl,txtHelp
            call    printMsg
            scf                                         ; set CF -> skip running the effect
            ret

                    ;12345678901234567890123456789012; 32chars width
txtHelp:    DB      "dot7gFX fadeout v0.1 by Ped7g",13
            DB      "fade-out palette to black",13,13
            DB      "SYNOPSIS:",13
            DB      " ../7GFX_FO.DOT <pal> [<delay>]",13
            DB      "ARGUMENTS:",13
            DB      " pal = <letter u|l|s|t>[0|1]",13
            DB      " delay = <integer 1..255>",13,13
            DB      "EXAMPLE:",13
            DB      " ../7GFX_FO.DOT u1 3",13
            DB      "     fades second ULA palette",13
            DB      "     in 21 frames (delay * 7)",13
            DB      0

;----------------------------------------------------------------------------------------------------------
; initialised data (with default values)

delay:      DB      6                                   ; default delay is 6 -> 6 * 7 = 42 frames = ~0.84s at 50Hz
palette:    DB      %0'0'01'0000                        ; default is Layer2 first palette

;----------------------------------------------------------------------------------------------------------
; uninitialised data -> not part of the binary

new_palette:    EQU     ($+1) & $FFFE                   ; aligned (2) 512B buffer where colors are faded out
orig_terminator EQU     new_palette + 512               ; 1B after it is loop terminator
orig_palette:   EQU     orig_terminator + 1             ; 512B buffer of original color values, transformed a bit

; for debugging in CSpect (with full card image)
;         DEVICE ZXSPECTRUM48 : CSPECTMAP "fadeout.map"
