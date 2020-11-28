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
GfxCursorLineG:     HEX 01EEEEEE EEEEEEEE EEEEEEEE EEEEEEEE ; 1px dot with shadow for mouse coordinates
GfxCursorLineH:     HEX EE00EEEE EEEEEEEE EEEEEEEE EEEEEEEE
GfxCursorLines:
    DB  0*16,1*16,2*16,3*16,4*16,4*16,4*16,4*16,3*16,2*16,1*16,0*16,4*16,4*16,4*16,4*16
GfxCursor2Lines:
    DB  5*16,6*16,4*16,4*16,4*16,4*16,4*16,4*16,4*16,4*16,4*16,4*16,4*16,4*16,4*16,4*16

TxtKeyI:
    DZ  "i different colour orders"
TxtCursors:
    DZ  "arrows/mouse moves cursor"
TxtBreak:
    DZ  "hold break to exit"
UlaRgbLabels    EQU $C040,10
TxtRgbLabels:
    DZ  "[$__,$__] R   G   B   (9b:$___)"
    ;    012345678901234567890123456789
TxtColorByte1st EQU UlaRgbLabels+2,10
TxtColorByte2nd EQU UlaRgbLabels+6,10
TxtColorRed     EQU UlaRgbLabels+12,10
TxtColorGreen   EQU UlaRgbLabels+16,10
TxtColorBlue    EQU UlaRgbLabels+20,10
TxtColor9bit    EQU UlaRgbLabels+27,10

keyboardDebounce:
        DB      0
selectedPalette:
        DW      0
cursorpos:
        DW      0
showcursor:
        DB      0
lastMouse:
        DW      0
someMouse:
        DB      0

        ; include the font data starting at 32nd character
    ASSERT $ <= font + ' '*8
    ORG font + ' '*8
    INCBIN "Envious Bold.ch8"   ; "Envious" font by DamienG https://damieng.com/zx-origins

BREAK_HOLD_INIT EQU     255-50  ; cca. 1sec
BREAK_ATTR_ADR  EQU     $DAE5,10
EXIT_ATTR_ADR   EQU     $DAED,10
breakHoldCounter:
        DB      BREAK_HOLD_INIT

InitUlaScreen:
        ld      hl,$C000
        ld      de,$C001
        ld      bc,$1800
        ld      (hl),l
        ldir
        ld      (hl),A_BRIGHT|P_BLACK|CYAN
        ld      bc,$300-1
        ldir
    ; print static strings
        ld      de,$C000
        ld      bc,TxtKeyI
        call    PrintUlaString
        ld      de,$C020
        ld      bc,TxtCursors
        call    PrintUlaString
        ld      de,$D0E0
        ld      bc,TxtBreak
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
        call    .colorizeAttrLoop
        ld      l,$26
        ld      (hl),A_BRIGHT|P_BLACK|CYAN
        ld      l,$4A
        ld      (hl),A_BRIGHT|P_BLACK|RED
        ld      l,$4E
        ld      (hl),A_BRIGHT|P_BLACK|GREEN
        ld      l,$52
        ld      (hl),A_BRIGHT|P_BLACK|BLUE
        ld      b,5
        ld      hl,BREAK_ATTR_ADR
.colorizeAttrLoop:
        ld      (hl),a
        inc     l
        djnz    .colorizeAttrLoop
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
        ld      bc,$2000|SPRITE_PATTERN_P_5B    ; B = 32 lines, C = patterns port
        ld      de,GfxCursorLines
.uploadPattern:
        ld      a,(de)
        inc     e
        ld      hl,GfxCursorLineA
        add     hl,a            ; source data for one line
        .16 OUTINB              ; upload 16 bytes
        djnz    .uploadPattern
.resetSprAttributes:
        ; setup cursor sprite attributes (make it also visible, it will be shown/hidden by global flag)
        nextreg SPRITE_ATTR_SLOT_SEL_NR_34,0    ; set sprite 0 as active
        nextreg SPRITE_ATTR0_NR_35,low -16      ; X=496 (512-16)
        nextreg SPRITE_ATTR1_NR_36,0
        nextreg SPRITE_ATTR2_NR_37,1            ; X8=1 (X = 496 outside of screen)
        nextreg SPRITE_ATTR3_INC_NR_78,$80      ; visible, pattern 0, 4-byte type, ++sprite ID
        ; dot-cursor for mouse coordinates
        nextreg SPRITE_ATTR0_NR_35,low -16      ; X=496 (512-16)
        nextreg SPRITE_ATTR1_NR_36,0
        nextreg SPRITE_ATTR2_NR_37,1            ; X8=1 (X = 496 outside of screen)
        nextreg SPRITE_ATTR3_NR_38,$81          ; visible, pattern 1, 4-byte type
        ret

ReadMousePortData:
    ; read raw [x,y] from Kempston mouse port
        ld      bc,KEMPSTON_MOUSE_X_P_FBDF
        in      e,(c)
        ld      bc,KEMPSTON_MOUSE_Y_P_FFDF
        in      a,(c)
        neg
        ld      d,a
        ret

ReadBreakAndMouse:
    ; read "break" key (Caps shift + space)
        ld      a,~(1<<0)               ; first row (VCXZ<CS>)
        in      a,(ULA_P_FE)
        rra
        sbc     hl,hl                   ; hl = 0 when CS is pressed, $FFFF when released
        ex      de,hl
        ld      a,~(1<<7)               ; eight row (BNM<SS><SP>)
        in      a,(ULA_P_FE)
        rra
        sbc     hl,hl                   ; hl = 0 when SP is pressed, $FFFF when released
        ld      a,l
        or      e                       ; A = 0 when break is pressed, $FF when not
        add     a,a
        inc     a                       ; A = 1 pressed, -1 not pressed
        ld      hl,(breakHoldCounter)   ; L = current frame counter for break-holding
        add     a,l
        jr      nz,.dontExitYet
        ld      bc,(25*70000)/256/(20*4+13) ; cca. 25 frames delay (C=N, B=0)
.delayBeforeReset:
        nextreg MMU0_0000_NR_50,$FF     ; map ROM to bottom 16kiB
        nextreg MMU1_2000_NR_51,$FF
        nextreg COPPER_CONTROL_HI_NR_62,0   ; switch off copper (to create "effect")
        nextreg TURBO_CONTROL_NR_07,0       ; switch to 3.5MHz for longer delay loop
        djnz    .delayBeforeReset
        dec     c
        jr      nz,.delayBeforeReset
        nextreg NEXT_RESET_NR_02,1      ; soft reset
        jr      $
.dontExitYet:
        cp      BREAK_HOLD_INIT         ; clamp the value to BREAK_HOLD_INIT as minimum
        adc     a,0
        ld      (breakHoldCounter),a    ; 205..255 value
    ; colorize the break by hold-duration
        ld      hl,EXIT_ATTR_ADR
        sub     BREAK_HOLD_INIT         ; 0..50
        rrca
        rrca
        rrca
        and     7
        ld      b,a                     ; B=0..6
        push    bc
        ld      a,A_BRIGHT|P_RED|YELLOW
        call    nz,InitUlaScreen.colorizeAttrLoop
        pop     bc
        ld      a,6
        sub     b
        ld      b,a
        ld      a,A_BRIGHT|P_BLACK|CYAN
        call    nz,InitUlaScreen.colorizeAttrLoop
    ; read mouse and move mouse cursor
        call    ReadMousePortData
        ld      hl,(lastMouse)
        or      a
        sbc     hl,de
        jr      z,.noMovementDetected
    ; use the coordinates directly (no fancy mouse driver tracking speed vectors/etc in this small example)
        ld      (lastMouse),de
        ld      a,SHOW_CURSOR_DURATION
        ld      (showcursor),a
        ld      (someMouse),a           ; enable buttons forever with any movement
        nextreg SPRITE_ATTR_SLOT_SEL_NR_34,1    ; set sprite 1 as active
        ld      a,32                    ; mouse X 0..255 transformed to 32..287
        add     a,e
        nextreg SPRITE_ATTR0_NR_35,a    ; X.lsb
        rla
        and     1
        nextreg SPRITE_ATTR2_NR_37,a    ; X8 extracted from carry flag
        ld      a,d
        nextreg SPRITE_ATTR1_NR_36,a    ; Y is used directly, doesn't need +32
.noMovementDetected:
        ld      a,(someMouse)
        or      a
        ret     z                       ; skip buttons check if there was no movement ever
        ld      a,(keyboardDebounce)
        or      a
        ret     nz                      ; ignore buttons while press-delay is active
    ; read buttons and move "cursor" square if something is pressed
        ld      bc,KEMPSTON_MOUSE_B_P_FADF
        in      a,(c)
        cpl
        and     7
        ret     z                       ; no button
        ld      a,KEYBOARD_REPEAT_DELAY ; set debounce/repeat delay (also affects mouse buttons)
        ld      (keyboardDebounce),a    ; disabling key reading for N frames
    ; calculate new cursor position
        ld      hl,(lastMouse)
        srl     l
        srl     l
        srl     l                       ; pos X = mouseX/8
        ld      a,h
        sub     32+32                   ; pos Y = (mouseY-64)/8
        ret     c                       ; mouse above palette area
        ret     m                       ; mouse under palette area
        rrca
        rrca
        rrca
        and     15
        ld      h,a
    ; set up new cursor position + update sprite + RGB readings (tailing into mainLoop)
        push    af                      ; needs any value on stack to finish cleanly
        jp      mainLoop.setCursorDirectly

start:
        di
        nextreg TURBO_CONTROL_NR_07,3                   ; 28MHz
        nextreg LAYER2_RAM_BANK_NR_12,9                 ; Layer 2 from bank 9 (NextZXOS default)
        nextreg SPRITE_CONTROL_NR_15,%0'0'0'000'1'1     ; SLU, no clipping, sprites visible + over border
        nextreg TRANSPARENCY_FALLBACK_COL_NR_4A,$E3     ; pink fallback (for debug purposes only)
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

    ; read initial mouse position (to later decide if mouse was even touched)
        call    ReadMousePortData
        ld      (lastMouse),de

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
        nextreg COPPER_DATA_16B_NR_63,$01
        ; cu.WAIT(line=31+32,h=52 (left border)) (1/4 of 32x16 field)
        nextreg COPPER_DATA_16B_NR_63,$80|(52<<1)
        nextreg COPPER_DATA_16B_NR_63,31+32
        ; cu.MOVE(GLOBAL_TRANSPARENCY_NR_14,$FE) to inhibit transparency
        nextreg COPPER_DATA_16B_NR_63,GLOBAL_TRANSPARENCY_NR_14
        nextreg COPPER_DATA_16B_NR_63,$FE
        ; cu.WAIT(line=31+64,h=52 (left border))
        nextreg COPPER_DATA_16B_NR_63,$80|(52<<1)
        nextreg COPPER_DATA_16B_NR_63,31+64             ; +64 because 8 row tiles per 8px
        ; cu.MOVE(PALETTE_CONTROL_NR_43,%0'000'010'0)   ; select second palette
        nextreg COPPER_DATA_16B_NR_63,PALETTE_CONTROL_NR_43
        nextreg COPPER_DATA_16B_NR_63,%0'101'010'0
        ; cu.MOVE(GLOBAL_TRANSPARENCY_NR_14,$01) to inhibit transparency in bottom half
        nextreg COPPER_DATA_16B_NR_63,GLOBAL_TRANSPARENCY_NR_14
        nextreg COPPER_DATA_16B_NR_63,$01
        ; cu.WAIT(line=31+88,h=52 (left border)) (~3/4 of 32x16 field)
        nextreg COPPER_DATA_16B_NR_63,$80|(52<<1)
        nextreg COPPER_DATA_16B_NR_63,31+88
        ; cu.MOVE(GLOBAL_TRANSPARENCY_NR_14,$FE) to inhibit transparency
        nextreg COPPER_DATA_16B_NR_63,GLOBAL_TRANSPARENCY_NR_14
        nextreg COPPER_DATA_16B_NR_63,$FE
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
        ld      a,%0'0'0'000'1          ; SLU, no clipping, spr over border (>>1)
        ccf
        rla                             ; non-zero initial timer = visible sprites, zero = off
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

        ld      a,(selectedPalette)
        add     a,a
        ld      hl,palettesNamesPtrs
        add     hl,a
        ld      c,(hl)
        inc     hl
        ld      b,(hl)                  ; BC = pointer to string with palette name
        ld      de,UlaPalName
        call    PrintUlaString

    ; read "break" key (Caps shift + space), mouse and move cursor if it's changing
        call    ReadBreakAndMouse

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
        inc     a
        cp      TOTAL_PALETTES
        jr      c,.selectedValidPal
        xor     a
.selectedValidPal:
        ld      (selectedPalette),a
        xor     a
        ld      (showcursor),a          ; hide cursor
        call    InitSprites.resetSprAttributes
        call    InitUlaScreen
        jp      mainLoop_delayKey

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
.setCursorDirectly:
        ld      (cursorpos),hl
        call    UpdateCursorReadings
        ; update sprite position and reset visibility duration
        nextreg SPRITE_ATTR_SLOT_SEL_NR_34,0    ; select sprite 0
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

UlaPalName      EQU $D0A0,10
palettesNamesPtrs:
        DW      palette0Name, palette1Name, palette2Name, palette3Name

palette0Name:
        DZ      "Ordered by sum of channel values"
palette1Name:
        DZ      "index 511 to 0 as color itself"
palette2Name:
        DZ      "up: default L2, down: other 256"
palette3Name:
        DZ      "188 colours, \"art\" palette"

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

    OPT push listmc
n=0
vb=0
vr=0
vg=0
dir=0   ; 0 -> from B toward R, !0 -> from R toward B (G is always last resort)
    DUP 512
        DB (vr<<5)|(vg<<2)|(vb>>1), vb&1
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
    EDUP
    OPT pop

    ; color table - 2x 512 bytes of 9bit colors going from "0" to "511" (black to white)
    ; the index is used as 9bit color value
palettesData_9bitIndex:
    OPT push listmc
n=0
    DUP 512
        DB n>>1, n&1
n=n+1
    EDUP
    OPT pop

    ; Layer2 default palette + complement to it (missing colours from default)
    ; index 0..255 is the "missing" colour stuff, 256..511 is Layer2 default palette
palettesData_L2default:
    OPT push listmc
n=0
    DUP 256             ; bottom part has the "missing" colours
        DB n, 1-((n>>1)|n)&1
n=n+1
    EDUP
n=0
    DUP 256             ; upper part has the default palette
        DB n, ((n>>1)|n)&1
n=n+1
    EDUP
    OPT pop

    ; "artsy" ordering of coulours - this one does NOT display all 512 colours!
palettesData_artsy:

    OPT push listmc

    MACRO gen_n idx?, vr?, vg?, vb?
            ORG palettesData_artsy + (idx?)*2
            DB (((vr?)&7)<<5)|(((vg?)&7)<<2)|(((vb?)&7)>>1), (vb?)&1
    ENDM

    MACRO gen_grad_ext idx?, di?, rep?, vr?, vg?, vb?, dr?, dg?, db?
n=idx?
vr=vr?
vg=vg?
vb=vb?
        DUP rep?
            gen_n n, vr, vg, vb
n=n+di?
vr=vr+dr?
vg=vg+dg?
vb=vb+db?
        EDUP
    ENDM

    MACRO gen_grad idx?, rep?, vr?, vg?, vb?, dr?, dg?, db?
        gen_grad_ext idx?, -32, rep?, vr?, vg?, vb?, dr?, dg?, db?
    ENDM

    ; fill 1024B with debug colour
    .32 HEX 2D01 2D01 2D01 2D01 2D01 2D01 2D01 2D01 2D01 2D01 2D01 2D01 2D01 2D01 2D01 2D01
bn=510-32
    gen_grad bn, 8, 7, 7, 7, -1, -1, -1    ; greys
bn=bn-2
    gen_n bn, 7, 7, 7 : gen_grad bn-1*32, 7, 7, 6, 7, -1, -1, -1    ; violet-ish greys
bn=bn-1
    gen_n bn, 7, 7, 7 : gen_grad bn-1*32, 7, 7, 6, 6, -1, -1, -1    ; red-ish greys
bn=bn-1
    gen_n bn, 7, 7, 7 : gen_grad bn-1*32, 7, 7, 7, 6, -1, -1, -1    ; yellow-ish greys
bn=bn-1
    gen_n bn, 7, 7, 7 : gen_grad bn-1*32, 7, 6, 7, 6, -1, -1, -1    ; green-ish greys
bn=bn-1
    gen_n bn, 7, 7, 7 : gen_grad bn-1*32, 7, 6, 7, 7, -1, -1, -1    ; cyan-ish greys
bn=bn-1
    gen_n bn, 7, 7, 7 : gen_grad bn-1*32, 7, 6, 6, 7, -1, -1, -1    ; blue-ish greys

bn=bn-2
    gen_grad bn, 7, 7, 6, 7,  0, -1,  0    : gen_grad bn-7*32, 7, 6, 0, 6, -1,  0, -1     ; violet
bn=bn-1
    gen_grad bn, 7, 7, 6, 6,  0, -1, -1    : gen_grad bn-7*32, 7, 6, 0, 0, -1,  0,  0     ; reds
bn=bn-1
    gen_grad bn, 7, 7, 7, 6,  0,  0, -1    : gen_grad bn-7*32, 7, 6, 6, 0, -1, -1,  0     ; yellows
bn=bn-1
    gen_grad bn, 7, 6, 7, 6, -1,  0, -1    : gen_grad bn-7*32, 7, 0, 6, 0,  0, -1,  0     ; green
bn=bn-1
    gen_grad bn, 7, 6, 7, 7, -1,  0,  0    : gen_grad bn-7*32, 7, 0, 6, 6,  0, -1, -1     ; cyan
bn=bn-1
    gen_grad bn, 7, 6, 6, 7, -1, -1,  0    : gen_grad bn-7*32, 7, 0, 0, 6,  0,  0, -1     ; blues

bn=bn-2
    gen_grad bn, 3, 7, 5, 6,  0, -2, -1    : gen_grad bn-3*32, 3, 6, 0, 3, -1,  0, -1 : gen_n bn-6*32, 3, 0, 1 : gen_n bn-7*32, 2, 0, 1
bn=bn-1
    gen_grad bn, 3, 7, 6, 5,  0, -1, -2    : gen_grad bn-3*32, 3, 6, 3, 0, -1, -1,  0 : gen_n bn-6*32, 3, 1, 0 : gen_n bn-7*32, 2, 1, 0
bn=bn-1
    gen_grad bn, 3, 6, 7, 5, -1,  0, -2    : gen_grad bn-3*32, 3, 3, 6, 0, -1, -1,  0 : gen_n bn-6*32, 1, 3, 0 : gen_n bn-7*32, 1, 2, 0
bn=bn-1
    gen_grad bn, 3, 5, 7, 6, -2,  0, -1    : gen_grad bn-3*32, 3, 0, 6, 3,  0, -1, -1 : gen_n bn-6*32, 0, 3, 1 : gen_n bn-7*32, 0, 2, 1
bn=bn-1
    gen_grad bn, 3, 5, 6, 7, -2, -1,  0    : gen_grad bn-3*32, 3, 0, 3, 6,  0, -1, -1 : gen_n bn-6*32, 0, 1, 3 : gen_n bn-7*32, 0, 1, 2
bn=bn-1
    gen_grad bn, 3, 6, 5, 7, -1, -2,  0    : gen_grad bn-3*32, 3, 3, 0, 6, -1,  0, -1 : gen_n bn-6*32, 1, 0, 3 : gen_n bn-7*32, 1, 0, 2

bn=bn-2
    gen_grad_ext bn, -64, 4, 7, 3, 7, -2, -1, -2 : gen_grad_ext bn-32, -64, 3, 6, 3, 6, -2, -1, -2
bn=bn-1
    gen_grad_ext bn, -64, 4, 7, 3, 3, -2, -1, -1 : gen_grad_ext bn-32, -64, 3, 6, 3, 3, -2, -1, -1
bn=bn-1
    gen_grad_ext bn, -64, 4, 7, 7, 3, -2, -2, -1 : gen_grad_ext bn-32, -64, 3, 6, 6, 3, -2, -2, -1
bn=bn-1
    gen_grad_ext bn, -64, 4, 3, 7, 3, -1, -2, -1 : gen_grad_ext bn-32, -64, 3, 3, 6, 3, -1, -2, -1
bn=bn-1
    gen_grad_ext bn, -64, 4, 3, 7, 7, -1, -2, -2 : gen_grad_ext bn-32, -64, 3, 3, 6, 6, -1, -2, -2
bn=bn-1
    gen_grad_ext bn, -64, 4, 3, 3, 7, -1, -1, -2 : gen_grad_ext bn-32, -64, 3, 3, 3, 6, -1, -1, -2

    ORG palettesData_artsy + 1024       ; restore pointer (from inside pal to after it)
    OPT pop

TOTAL_PALETTES  EQU ($-palettesData)/1024

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
        SHELLEXEC "( sleep 0.1s ; runCSpect -brk -map=show512.map show512.nex ) &"
    ENDIF
