; "Show 512 colors" example, drawing all possible HW colours in Layer2 256x192 mode
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
; requires ZX Spectrum Next with core3.0+
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.17.1+)
; the source is using currently unreleased (in v1.17.0) "ELSEIF" feature, so
; you need to build sjasmplus from github or wait for next release (hopefully soon)

    OPT reset --zxnext --syntax=abfw
    DEVICE ZXSPECTRUMNEXT

    INCLUDE "constants.i.asm"
KEYBOARD_REPEAT_DELAY   EQU     10
SHOW_CURSOR_DURATION    EQU     250

    ORG $E000

font:   ; font starts here, but there's no graphics for chars 0..31, so using that for data

GfxCursorLineA:     HEX 000000EE EEEEEEEE EE000000 EEEEEEEE
GfxCursorLineB:     HEX 00010101 EEEEEEEE 01010100 EEEEEEEE
GfxCursorLineC:     HEX 0001EEEE EEEEEEEE EEEE0100 EEEEEEEE
GfxCursorLineD:     HEX EE01EEEE EEEEEEEE EEEE01EE EEEEEEEE
GfxCursorLineF:     HEX EEEEEEEE EEEEEEEE EEEEEEEE EEEEEEEE
GfxCursorLines:
    DB  0*16,1*16,2*16,3*16,4*16,4*16,4*16,4*16,3*16,2*16,1*16,0*16,4*16,4*16,4*16,4*16

TxtKeyI:
    DZ  "i - different colour orders"
TxtCursors:
    DZ  "cursors/mouse - move cursor"
UlaRgbLabels    EQU $C040
TxtRgbLabels:
    DZ  "[$..,$..] R:. G:. B:. (9b:$...)"
    ;    012345678901234567890123456789
TxtColorByte1st EQU UlaRgbLabels+2
TxtColorByte2nd EQU UlaRgbLabels+6
TxtColorRed     EQU UlaRgbLabels+12
TxtColorGreen   EQU UlaRgbLabels+16
TxtColorBlue    EQU UlaRgbLabels+20
TxtColor9bit    EQU UlaRgbLabels+27

keyboardDebounce:
        DB      0
selectedPalette:
        DW      0
cursorpos:
        DW      0
showcursor:
        DB      0

        ; include the font data starting at 32nd character
    ASSERT $ <= font + ' '*8
    ORG font + ' '*8
    INCBIN "Envious Bold.ch8"   ; "Envious" font by DamienG https://damieng.com/zx-origins

InitUlaScreen:
        ld      hl,$C000
        ld      de,$C001
        ld      bc,$1800
        ld      (hl),l
        ldir
        ld      (hl),A_BRIGHT|P_BLACK|GREEN
        ld      bc,$300-1
        ldir
    ; print static strings
        ld      de,$C000
        ld      bc,TxtKeyI
        call    PrintUlaString
        ld      de,$C020
        ld      bc,TxtCursors
        call    PrintUlaString
        ld      de,UlaRgbLabels
        ld      bc,TxtRgbLabels
        call    PrintUlaString
    ; mark interactive keys with attributes
        ld      a,A_BRIGHT|P_BLACK|WHITE
        ld      hl,$D800
        ld      (hl),a
        ld      l,$20
        ld      b,13
.cursorMouseAttrLoop:
        ld      (hl),a
        inc     l
        djnz    .cursorMouseAttrLoop
        ld      l,$27
        ld      (hl),A_BRIGHT|P_BLACK|GREEN
        ret

PrintUlaString:
        ld      a,(bc)          ; fetch string character and check it against zero
        inc     bc
        or      a
        ret     z               ; zero char as string terminator
        call    PrintUlaChar
        jr      PrintUlaString
PrintUlaChar:
        add     a,a             ; calculate font address (also truncates char to 0..127)
        ld      l,a
        ld      h,(high font)/4
        add     hl,hl
        add     hl,hl           ; HL = font + char*8
        .8 ldws                 ; 8x LDWS => draws the character
        add     de,-$800+1      ; advance to next pixel position
        ret

PrintHexByteAtDe:
        push    af
        swapnib
        call    PrintHexDigit
        pop     af
        ;  |
        ; fallthrough to PrintHexDigit
        ;  |
        ;  v
PrintHexDigit:
        and     $0F
        cp      10
        sbc     a,$69
        daa                     ; convert 0..15 to '0'..'F' hexa digit
        push    hl
        call    PrintUlaChar    ; print it
        pop     hl
        ret

UpdateCursorReadings:           ; HL = cursor position, preserve B
        push    hl
        ex      de,hl           ; flip position (upper left corner in palette is [31,15])
        ld      hl,$0F1F        ; (bottom right is [0,0])
        or      a
        sbc     hl,de
        add     hl,hl           ; cursor pos *= 2
        ld      e,h
        ld      d,32
        mul     de              ; pos Y *= 32 (memory offset)
        ld      a,(selectedPalette)
        add     a,a
        add     a,a
        ld      h,a             ; H=selectedPalette * 1024, L = (0..31)*2
        add     hl,de
        add     hl,palettesData ; final address of particular color
        ; fetch the color to H = first byte, L = second byte
        ld      d,(hl)
        inc     hl
        ld      e,(hl)
        ex      de,hl
    ; show the color values in ULA texts
        ld      de,TxtColorByte1st
        ld      a,h
        call    PrintHexByteAtDe
        ld      de,TxtColorByte2nd
        ld      a,l
        call    PrintHexByteAtDe
        ld      a,h
        .3 rlca
        and     7
        ld      de,TxtColorRed
        call    PrintHexDigit
        ld      a,h
        .2 rrca
        and     7
        ld      de,TxtColorGreen
        call    PrintHexDigit
        ld      a,h
        rlca
        and     1
        ld      de,TxtColor9bit
        call    PrintHexDigit
        ld      a,h
        rr      l
        rla
        ld      de,TxtColor9bit+1
        push    af
        call    PrintHexByteAtDe
        pop     af
        and     7
        ld      de,TxtColorBlue
        call    PrintHexDigit
        pop     hl              ; restore HL
        ret

InitSprites:
        xor     a
        ; setup sprite palette
        nextreg PALETTE_CONTROL_NR_43,%0'010'000'0  ; first Sprite palette to edit
        nextreg PALETTE_INDEX_NR_40,a
        nextreg PALETTE_VALUE_NR_41,a           ; black
        nextreg PALETTE_VALUE_NR_41,$FF         ; white
        nextreg SPRITE_TRANSPARENCY_I_NR_4B,$EE ; $EE is transparent index
        ; hide all 128 sprites
        nextreg SPRITE_ATTR_SLOT_SEL_NR_34,a    ; set sprite 0 as active
        ld      b,128
.hideAllSprites:
        nextreg SPRITE_ATTR3_INC_NR_78,a
        djnz    .hideAllSprites
        ; upload sprite graphics to first patter (cursor gfx defined by important lines)
        ld      bc,SPRITE_STATUS_SLOT_SELECT_P_303B
        out     (c),a           ; select pattern 0
        ld      bc,$1000|SPRITE_PATTERN_P_5B    ; B = 16 lines, C = patterns port
        ld      de,GfxCursorLines
.uploadPattern:
        ld      a,(de)
        inc     e
        ld      hl,GfxCursorLineA
        add     hl,a            ; source data for one line
        .16 OUTINB              ; upload 16 bytes
        djnz    .uploadPattern
        ; setup cursor sprite attributes (make it also visible, it will be shown/hidden by global flag)
        nextreg SPRITE_ATTR_SLOT_SEL_NR_34,0    ; set sprite 0 as active
        nextreg SPRITE_ATTR0_NR_35,0
        nextreg SPRITE_ATTR1_NR_36,0
        nextreg SPRITE_ATTR2_NR_37,0
        nextreg SPRITE_ATTR3_NR_38,$80          ; visible, pattern 0, 4-byte type
        ret

start:
        di
        nextreg TURBO_CONTROL_NR_07,3                   ; 28MHz
        nextreg LAYER2_RAM_BANK_NR_12,9                 ; Layer 2 from bank 9 (NextZXOS default)
        nextreg SPRITE_CONTROL_NR_15,%0'0'0'000'1'1     ; SLU, no clipping, sprites visible + over border
        nextreg LAYER2_CONTROL_NR_70,%00'00'0000        ; 256x192x8 mode, palette offset +0
        nextreg DISPLAY_CONTROL_NR_69,%1'0'000000       ; Layer 2 ON, ULA shadow off, Timex = 0
        nextreg LAYER2_XOFFSET_MSB_NR_71,0 ,, LAYER2_XOFFSET_NR_16,0 ,, LAYER2_YOFFSET_NR_17,0  ; layer2 X/Y offset = [+0, +0]
        nextreg CLIP_WINDOW_CONTROL_NR_1C,1             ; layer2 clip 256x128 in middle
        nextreg CLIP_LAYER2_NR_18,0 ,, CLIP_LAYER2_NR_18,255 ,, CLIP_LAYER2_NR_18,32 ,, CLIP_LAYER2_NR_18,32+128-1

    ; map 48kiB of Layer2 to $0000..$BFFF address range
        nextreg MMU0_0000_NR_50,2*9+0
        nextreg MMU1_2000_NR_51,2*9+1
        nextreg MMU2_4000_NR_52,2*10+0
        nextreg MMU3_6000_NR_53,2*10+1
        nextreg MMU4_8000_NR_54,2*11+0
        nextreg MMU5_A000_NR_55,2*11+1
        nextreg MMU6_C000_NR_56,2*5     ; classic ULA Bank 5 to $C000 (first 8kiB only)

    ; clear ULA layer (mapped at $C000) and print controls
        call    InitUlaScreen

    ; init sprites used to show the cursor
        call    InitSprites

    ; draw the color tiles, each tile is 8x8 pixels, the whole map is 32x16 (512 colors)
    ; indexes goes from 255 to 0, two times (copper will switch palette in the middle)
        ; clear 32 lines with black (from second palette)
        xor     a
        ld      hl,0
        ld      de,1
        ld      bc,256*32
        ld      (hl),a
        ldir
        ; draw 8px x 8px tiles going from index 255 downward to 0 twice (32x16 tiles)
        ld      b,16                    ; 32x16 tiles (going $FF..$00 twice)
        ; A = 0 (will start drawing from --A => 255)
tilesRowLoop:
        call    drawColorTilesAtHl
        djnz    tilesRowLoop
        ; clear bottom 32 lines with black (from second palette)
        ; A == 0, HL == address to draw ($A000)
        ld      d,h
        ld      e,l
        inc     e
        ld      bc,256*32-1
        ld      (hl),a
        ldir

    ; upload and start the copper code flipping the palettes at required video lines
        nextreg COPPER_CONTROL_LO_NR_61,0
        nextreg COPPER_CONTROL_HI_NR_62,0
        ; cu.WAIT(line=31,h=52 (left border))
        nextreg COPPER_DATA_16B_NR_63,$80|(52<<1)
        nextreg COPPER_DATA_16B_NR_63,31
        ; cu.MOVE(PALETTE_CONTROL_NR_43,%0'000'000'0)   ; select first palette
        nextreg COPPER_DATA_16B_NR_63,PALETTE_CONTROL_NR_43
        nextreg COPPER_DATA_16B_NR_63,%0'001'000'0
        ; cu.MOVE(GLOBAL_TRANSPARENCY_NR_14,$01) to inhibit transparency in upper half
        nextreg COPPER_DATA_16B_NR_63,GLOBAL_TRANSPARENCY_NR_14
        nextreg COPPER_DATA_16B_NR_63,1
        ; cu.WAIT(line=31+64,h=52 (left border))
        nextreg COPPER_DATA_16B_NR_63,$80|(52<<1)
        nextreg COPPER_DATA_16B_NR_63,31+64             ; +64 because 8 row tiles per 8px
        ; cu.MOVE(PALETTE_CONTROL_NR_43,%0'000'010'0)   ; select second palette
        nextreg COPPER_DATA_16B_NR_63,PALETTE_CONTROL_NR_43
        nextreg COPPER_DATA_16B_NR_63,%0'101'010'0
        ; cu.MOVE(GLOBAL_TRANSPARENCY_NR_14,$E3) to inhibit transparency in bottom half
        nextreg COPPER_DATA_16B_NR_63,GLOBAL_TRANSPARENCY_NR_14
        nextreg COPPER_DATA_16B_NR_63,$E3
        ; cu.HALT
        nextreg COPPER_DATA_16B_NR_63,$FF
        nextreg COPPER_DATA_16B_NR_63,$FF
        ; start the copper in VBI-restart mode
        nextreg COPPER_CONTROL_HI_NR_62,%11'00'0000

mainLoop_delayKey:
        ld      a,KEYBOARD_REPEAT_DELAY ; set debounce/repeat delay
        ld      (keyboardDebounce),a    ; disabling key reading for N frames

mainLoop:
    ; wait for video line 180 to make palette upload not interfere with copper operations
        call    WaitForVideoLine180

    ; hide/show sprite cursor based on the showcursor timer
        ld      a,(showcursor)
        sub     1
        adc     a,0
        ld      (showcursor),a          ; countdown to zero
        sbc     a,a                     ; convert non-zero initial timer to zero, zero to $FF
        inc     a                       ; non-zero initial timer = visible sprites, zero = off
        or      %0'0'0'000'1'0          ; SLU, no clipping, spr over border
        nextreg SPRITE_CONTROL_NR_15,a

    ; setup the palettes
    ; - first palette covers "brighter" colors the upper half
    ; - second palette covers "darker" colors for bottom half and regular black
        ld      de,(selectedPalette)
        ld      b,10
        bsla    de,b                    ; DE = selectedPalette * 1024
        add     de,palettesData         ; calculate source address of selected palette
        nextreg PALETTE_CONTROL_NR_43,%0'101'010'0      ; second Layer2 palette to set (+show)
        call    uploadFullPalette
        nextreg PALETTE_CONTROL_NR_43,%0'001'010'0      ; first Layer2 palette to set
        call    uploadFullPalette

    ; read keyboard and react to user controls
        ld      a,(keyboardDebounce)    ; ignore keys for short delay after previous one
        sub     1
        adc     a,0
        ld      (keyboardDebounce),a
        jr      nz,mainLoop
        ; check for 'i' -> color order toggle
        ld      a,~(1<<5)               ; sixth row (yuiop)
        in      a,(ULA_P_FE)
        rra
        rra
        rra
        jr      nc,.keyIpressed
        ld      b,1                     ; mark following checks as "no key pressed"
        ; check cursors and move the cursor
        ld      a,~(1<<3)               ; fourth row (54321)
        in      a,(ULA_P_FE)
        and     $10
        call    z,.key5pressed
        ld      a,~(1<<4)               ; fifth row (67890)
        in      a,(ULA_P_FE)
        rra
        call    nc,.key0pressed
        rra
        rra
        call    nc,.key8pressed
        rra
        call    nc,.key7pressed
        rra
        call    nc,.key6pressed

        djnz    mainLoop_delayKey       ; if some cursor key was pressed, do delay
        jr      mainLoop                ; otherwise regular loop (no key delay)

.keyIpressed:
        ld      a,(selectedPalette)
        xor     1
        ld      (selectedPalette),a
        xor     a
        ld      (showcursor),a          ; hide cursor
        jr      mainLoop_delayKey

.key0pressed:
        push    af
        ld      de,$0000
        jr      .moveCursor             ; just make cursor visible at current position
.key5pressed:
        push    af
        ld      de,$00FF                ; [-1,0] left
        jr      .moveCursor
.key6pressed:
        push    af
        ld      de,$0100                ; [0,+1] down
        jr      .moveCursor
.key7pressed:
        push    af
        ld      de,$FF00                ; [0,-1] down
        jr      .moveCursor
.key8pressed:
        push    af
        ld      de,$0001                ; [+1,0] right
.moveCursor:
        ld      hl,(cursorpos)
        ; X pos update
        ld      a,l
        add     a,e
        jp      p,.posXpositive         ; clamp X pos to 0 as min
        xor     a
.posXpositive:
        cp      32
        adc     a,-1                    ; clamp X pos to 31 as max
        ld      l,a
        ; Y pos update
        ld      a,h
        add     a,d
        jp      p,.posYpositive         ; clamp Y pos to 0 as min
        xor     a
.posYpositive:
        cp      16
        adc     a,-1                    ; clamp Y pos to 15 as max
        ld      h,a
        ld      (cursorpos),hl
        call    UpdateCursorReadings
        ; update sprite position and reset visibility duration
        ld      a,l
        add     a,a
        add     a,a
        add     a,a                     ; 8*pos X (0..248)
        add     a,32-2                  ; add +32 as sprite system X origin (carry is X8)
        nextreg SPRITE_ATTR0_NR_35,a    ; X.LSB
        sbc     a,a
        and     1                       ; extract X8 from carry as 0/1 value
        nextreg SPRITE_ATTR2_NR_37,a
        ld      a,h
        add     a,a
        add     a,a
        add     a,a                     ; 8*pos Y (0..120)
        add     a,32+32-2               ; adjust by sprite system Y origin (and -2)
        nextreg SPRITE_ATTR1_NR_36,a    ; Y.LSB (no worries about Y8)
        ld      a,SHOW_CURSOR_DURATION
        ld      (showcursor),a
        ; mark B as "some key was pressed" (to set delay)
        inc     b
        pop     af
        ret

WaitForVideoLine180:
        ld      bc,TBBLUE_REGISTER_SELECT_P_243B
        ld      a,VIDEO_LINE_LSB_NR_1F
        out     (c),a
        inc     b
.waitForLine:
        in      a,(c)
        cp      180
        jr      nz,.waitForLine
        ret

uploadFullPalette:
        nextreg PALETTE_INDEX_NR_40,0                   ; reset color index
        ld      bc,2                                    ; upload 512 bytes (B=0, C=2)
.dataLoop:
        ld      a,(de)
        inc     de
        nextreg PALETTE_VALUE_9BIT_NR_44,a
        djnz    .dataLoop
        dec     c
        jr      nz,.dataLoop
        ret

drawColorTilesAtHl:
        push    bc
        ld      c,32
.tilesLoop:
        ld      b,8
        dec     a
.pixelLoop:
        ld      (hl),a
        inc     l
        djnz    .pixelLoop
        dec     c
        jr      nz,.tilesLoop
        ; HL is back to original value (256x `inc l`), copy the full line eight times
        ld      d,h
        ld      e,l
        inc     d
        ld      bc,256*8
        ldir
        pop     bc
        ret

    ; after this address there are multiple (two at this moment) 1024 bytes blocks with
    ; different ordering of the colours to set up palettes
palettesData:

    ; color table - 2x 512 bytes of 9bit colors going from "0" to "511" (black to white)
    ; ordered/grouped by sum of channel values, putting raw-blue toward lower indices
    ; the algorithm to generate the table is sort of "marching 8x8x8 cube" per diagonal
    ; plane-cuts ... see the "test_tga.asm" file for some more comments/ascii-art
    ; (the TGA generator was used to prototype the marching and verify the results)
    ;
    ; the colors are not directly equal to index, but grouped together with
    ; same sum of channel values ("wanna be luminance" order, but is not)
    ; but 0 in second palette is black #000, and 255 in first palette is white #777
palettesData_channelSumSorted:

n=0
vb=0
vr=0
vg=0
dir=0   ; 0 -> from B toward R, !0 -> from R toward B (G is always last resort)
    DUP 512
        DB (vr<<5)|(vg<<2)|(vb>>1), vb&1
        OPT push listoff
        IF 0==dir && 0<vb && vr<7       ; going from B toward R
vb=vb-1
vr=vr+1
        ELSEIF 0!=dir && 0<vr && vb<7   ; going from R toward B
vb=vb+1
vr=vr-1
        ELSEIF 0<(vb+vr) && vg<7        ; has room to move toward G and do another R<->B run
vg=vg+1
            IF (vb<vr && 0<vb) || (0==vr)
vb=vb-1
            ELSE
                ASSERT 0<vr
vr=vr-1
            ENDIF
dir=(vb<vr)
        ELSE                            ; the current "plane" was exhausted, move to next one
            IF vr<vb                    ; make sure B is smaller than R (swap)
temp=vr
vr=vb
vb=temp
            ENDIF
temp=vb     ; swap G with B, and ++B
vb=vg+1
vg=temp
            IF 7<vb     ; make sure B is valid (<=7), add excess to R
vb=vb-1
vr=vr+1
            ENDIF
            IF 7<vr     ; make sure R is valid (<=7), add excess to G
vr=vr-1
vg=vg+1
            ENDIF
dir=0
        ENDIF
n=n+1
        OPT pop
    EDUP

    ; color table - 2x 512 bytes of 9bit colors going from "0" to "511" (black to white)
    ; the index is used as 9bit color value
palettesData_9bitIndex:
n=0
    DUP 512
        DB n>>1, n&1
n=n+1
    EDUP

    ; reserve space for stack
        BLOCK   1024, $AA   ; $AAAA is debug filler in case of debugging stack
        BLOCK   10,0        ; avoid non-zero stack data warning of newer sjasmplus (unreleased yet)
stack:  DW      $AAAA
    SAVENEX OPEN "show512.nex", start, stack, 0, 2
    SAVENEX CORE 3,0,0 : SAVENEX CFG 0
    SAVENEX AUTO : SAVENEX CLOSE
    CSPECTMAP "show512.map"

    IFNDEF LAUNCH_EMULATOR : DEFINE LAUNCH_EMULATOR 0 : ENDIF
    IF 0 == __ERRORS__ && 0 == __WARNINGS__ && 1 == LAUNCH_EMULATOR
;         SHELLEXEC "runzeseruse show512.nex"
        SHELLEXEC "runCSpect -brk -map=show512.map show512.nex"
    ENDIF
