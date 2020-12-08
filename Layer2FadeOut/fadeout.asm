; "Fade Out" example, fading out to black image in Layer2 320x256 mode
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
; requires ZX Spectrum Next with core3.0+
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.15.0+)
; Does use imagemagick "convert" tool to prepare image data for direct INCBIN
; The Makefile has the full-rebuild instructions

    OPT reset --zxnext --syntax=abfw
    DEVICE ZXSPECTRUMNEXT

    INCLUDE "constants.i.asm"
    MMU 0 n, 9*2, $0000     ; include fadeout image into 16ki banks 9-13 (8ki pages 18-26)
fadeout_pic:
    INCBIN "fadeout.tga", 0x12 + 3*256, 256*320

BORDER  MACRO   color?
            ld      a,color?
;             out     (254),a
        ENDM

; Read NextReg into A (does modify A, and NextReg selected on the I/O port)
; is not optimized for speed + restores BC
    MACRO NEXTREG2A register?
        ld     a,register?
        call   ReadNextReg
    ENDM

    ORG $E000
start:
        di
        nextreg TURBO_CONTROL_NR_07,3                   ; 28MHz
        nextreg LAYER2_RAM_BANK_NR_12,$$fadeout_pic/2   ; Layer 2 from bank where l2buf starts
        nextreg LAYER2_CONTROL_NR_70,%00'01'0000        ; 320x256x8 mode, palette offset +0
        nextreg DISPLAY_CONTROL_NR_69,%1'0'000000       ; Layer 2 ON, ULA shadow off, Timex = 0
        nextreg LAYER2_XOFFSET_MSB_NR_71,0 ,, LAYER2_XOFFSET_NR_16,0 ,, LAYER2_YOFFSET_NR_17,0  ; layer2 X/Y offset = [+0, +0]
        nextreg CLIP_WINDOW_CONTROL_NR_1C,1             ; layer2 clip 320x256
        nextreg CLIP_LAYER2_NR_18,0 ,, CLIP_LAYER2_NR_18,159 ,, CLIP_LAYER2_NR_18,0 ,, CLIP_LAYER2_NR_18,255
        ; the example image does use the Layer2 default palette, so no setting of that
        ; the fadeout effect will read *current* (any) palette and do fadeout from it

    ; read current palette values
        nextreg PALETTE_CONTROL_NR_43,%0'001'0'0'0'0    ; select Layer 2 palette, select first palette
        ld      b,0                                     ; repeat counter (0 == 256x)
        ld      de,originalPalette
.readPaletteLoop:
        ld      a,b
        nextreg PALETTE_INDEX_NR_40,a                   ; select color
        NEXTREG2A   PALETTE_VALUE_9BIT_NR_44            ; read p000000B part of color
        ld      (de),a
        inc     de
        NEXTREG2A   PALETTE_VALUE_NR_41                 ; read RRRGGGBB part of color
        ld      (de),a
        inc     de
        inc     b
        jr      nz,.readPaletteLoop

.reinitFadeOutLoop:
    ; copy current palette values to "live colors" buffer which will be fading out
        ld      hl,originalPalette
        ld      de,liveColorsBuffer
        ld      bc,2*256
        ldir

    ; prepare fade-out constants - the fadeout will use "Bresenham line algorithm"
    ; to go from initial channel value 0..7 to 0 in N steps linearly
        ld      de,liveColorsBuffer
        ld      hl,BresenhamData
        ld      b,0         ; counter: 0 == 256x
        ld      a,(duration)
        neg
        dec     a
        ld      c,a         ; initial "D" for Bresenham = -duration - 1
.initBresenhamDataLoop:
    ; blue channel init
        ld      a,(de)      ; p000000B
        inc     e
        rra
        ld      a,(de)      ; RRRGGGBB
        rla
        rla                 ; A = xxxxBBBx
        and     7<<1        ; A = 2*Blue (2*{0..7}) = 2*dy
        ld      (hl),a      ; deltaD = 2*dy
        inc     l
        ld      (hl),c      ; init D
        inc     hl
    ; green channel init
        ld      a,(de)      ; RRRGGGBB
        rrca
        and     7<<1        ; A = 2*Green (2*{0..7}) = 2*dy
        ld      (hl),a      ; deltaD = 2*dy
        inc     l
        ld      (hl),c      ; init D
        inc     hl
    ; red channel init
        ld      a,(de)      ; RRRGGGBB
        inc     de
        swapnib
        and     7<<1        ; A = 2*Red (2*{0..7}) = 2*dy
        ld      (hl),a      ; deltaD = 2*dy
        inc     l
        ld      (hl),c      ; init D
        inc     hl
        djnz    .initBresenhamDataLoop

    ; do the fade-out effect
        ld      a,(duration)
        ld      b,a         ; number of frames to fadeout
FadeOutLoop:
        BORDER  0
        push    bc
        call    WaitForScanline224      ; do not read keyboard while fading out
        BORDER  1
        call    FadeOutColors
        pop     bc
        djnz    FadeOutLoop
    ; fade out done, wait 0.5s, then restore original colors
        BORDER  0
        ld      e,25
.blackWait:
        call    IdleLoopWithKeyboard    ; while idling, read keyboard (to modify duration)
        dec     e
        jr      nz,.blackWait
    ; restore colors in palette
        ld      hl,originalPalette
        ld      b,0
        nextreg PALETTE_INDEX_NR_40,0       ; select color 0 before loop
.resetColors:
        ld      c,(hl)
        inc     l
        ld      a,(hl)
        inc     hl
        nextreg PALETTE_VALUE_9BIT_NR_44,a
        ld      a,c
        nextreg PALETTE_VALUE_9BIT_NR_44,a
        djnz    .resetColors
    ; wait another 0.5s, then re-run the fadeout effect
        ld      e,25
.originalWait:
        call    IdleLoopWithKeyboard    ; while idling, read keyboard (to modify duration)
        dec     e
        jr      nz,.originalWait
    ; restore live colors and bresenham init
        jr      start.reinitFadeOutLoop

IdleLoopWithKeyboard:
        call    WaitForScanline224
        ld      hl,durationPickTable
        ld      a,~(1<<3)               ; keys 12345
        in      a,(254)
        DUP     5
            rrca
            jr      nc,.setNewDuration
            inc     hl
        EDUP
        ld      a,~(1<<4)               ; keys 67890
        in      a,(254)
        rlca
        rlca
        rlca                            ; check 6, 7, ... (not 0, 9, 8, ...)
        DUP     5
            rlca
            jr      nc,.setNewDuration
            inc     hl
        EDUP
        ret
.setNewDuration:
        ld      a,(hl)
        ld      (duration),a
        ret

FadeOutColors:
        ld      de,liveColorsBuffer
        ld      hl,BresenhamData
        nextreg PALETTE_INDEX_NR_40,0       ; select color 0 before loop
        ld      a,(duration)
        add     a,a
        ld      b,a         ; constant for adjusting D when it gets >= 0 (2*dx)
    ; do the subroutine twice, as it will return upon processing every 256 bytes of palette data
        call    .fadeOutLoop
        inc     d           ; fix "only inc e" done on last byte to reach other 256 bytes
.fadeOutLoop:
    ; do Bresenham linear subtraction of R,G,B channel value (single frame tick)
    ; check if Blue channel should decrement
        ld      a,(hl)      ; read deltaD for current channel (deltaD = 2*dy)
        inc     l
        add     a,(hl)      ; regA = D + deltaD
        jp      c,.decrementChannelB    ; D < 0 -> don't decrement yet
        ld      (hl),a      ; store updated D
        inc     hl
        ld      a,(de)      ; p000000B part of current color (live value, fading out)
        ld      (.b2),a     ; modify it in nextreg instruction setting second color byte
        ld      c,0         ; "sub" value for final color (no change to "RRRGGGBB" so far)
        jp      .continueWithOtherChannels
.decrementChannelB:
        sub     b           ; regA = D - 2*dx (D >= 0)
        ld      (hl),a      ; store updated D
        inc     hl
        ; decrement "BBB" composed from one bit in first byte and two bits in second byte
        ld      a,(de)      ; p000000B part of current color (live value, fading out)
        xor     1           ; can't just `dec a` because of priority bits
        ld      (de),a      ; store the modified lowB
        ld      (.b2),a     ; modify it in nextreg instruction setting second color byte
        and     1           ; if "1", then "BB" in main color byte needs to be adjusted too
        ld      c,a         ; "sub" value for final color is 0 or 1 depending on new lowB
.continueWithOtherChannels:
    ; check if Green channel should decrement
        ld      a,(hl)      ; read deltaD for current channel (deltaD = 2*dy)
        inc     l
        add     a,(hl)      ; regA = D + deltaD
        jp      nc,.doNotDecrementChannelG  ; D < 0 -> don't decrement yet
        sub     b           ; regA = D - 2*dx (D >= 0)
        set     2,c         ; add %000'001'00 to "sub" value to decrement Green channel
.doNotDecrementChannelG:
        ld      (hl),a      ; store updated D
        inc     hl
    ; check if Red channel should decrement
        ld      a,(hl)      ; read deltaD for current channel (deltaD = 2*dy)
        inc     l
        add     a,(hl)      ; regA = D + deltaD
        jp      nc,.doNotDecrementChannelR  ; D < 0 -> don't decrement yet
        sub     b           ; regA = D - 2*dx (D >= 0)
        set     5,c         ; add %001'000'00 to "sub" value to decrement Red channel
.doNotDecrementChannelR:
        ld      (hl),a      ; store updated D
        inc     hl
    ; output the final color and update the "live" color buffer
        inc     e
        ld      a,(de)      ; RRRGGGBB part of current color (live value, fading out)
        sub     c           ; adjust it by calculated "sub" value
        ld      (de),a      ; store the modified color
        nextreg PALETTE_VALUE_9BIT_NR_44,a
.b2=$+3:nextreg PALETTE_VALUE_9BIT_NR_44,0  ; self-modified code to set second byte of color
        inc     e
        jr      nz,.fadeOutLoop
        ret

WaitForScanline224:         ; wait for scanline 224 (just below 320x256 area)
    ; read NextReg $1F - LSB of current raster line
        ld      bc,TBBLUE_REGISTER_SELECT_P_243B
        ld      a,VIDEO_LINE_LSB_NR_1F
        out     (c),a       ; select NextReg $1F
        inc     b           ; BC = TBBLUE_REGISTER_ACCESS_P_253B
    ; if already at scanline 224, then wait extra whole frame (for super-fast game loops)
.cantStartAt224:
        in      a,(c)       ; read the raster line LSB
        cp      224
        jr      z,.cantStartAt224
    ; if not yet at scanline 224, wait for it ... wait for it ...
.waitLoop:
        in      a,(c)       ; read the raster line LSB
        cp      224
        jr      nz,.waitLoop
    ; and because the max scanline number is between 260..319 (depends on video mode),
    ; I don't need to read MSB. 256+224 = 480 -> such scanline is not part of any mode.
        ret

ReadNextReg:
    ; reads nextreg in A into A (does modify currently selected NextReg on I/O port)
        push    bc
        ld      bc,TBBLUE_REGISTER_SELECT_P_243B
        out     (c),a
        inc     b       ; bc = TBBLUE_REGISTER_ACCESS_P_253B
        in      a,(c)   ; read desired NextReg state
        pop     bc
        ret

        ALIGN   256
originalPalette:
        BLOCK   256*2, 0
liveColorsBuffer:
        BLOCK   256*2, 0
BresenhamData:
        BLOCK   3*2*256, 0  ; 3x channel (R,G,B), two bytes (deltaD, current "D")

duration:
        DB      52          ; duration of fadeout in frames
    ; minimum valid duration is 7 (to reach 0 by -1 from color value 7)
    ; maximum valid duration is 127 (as 2*duration must fit into 8 bit value)

durationPickTable:
        DB      7, 12, 19, 28, 39, 52, 67, 84, 103, 124 ; durations for keys 1,2,3,...,9,0

    ; reserve space for stack
        BLOCK   1024, $AA   ; $AAAA is debug filler in case of debugging stack
stack:  DW      $AAAA
    SAVENEX OPEN "fadeout.nex", start, stack, 0, 2  ; nexstack-ok
    SAVENEX CORE 3,0,0 : SAVENEX CFG 0
    SAVENEX AUTO : SAVENEX CLOSE
    CSPECTMAP "fadeout.map"
