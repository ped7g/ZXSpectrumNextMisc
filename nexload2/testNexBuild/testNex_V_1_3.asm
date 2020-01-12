/*
; patch BMP file with calculated palette (so I can add it to GIMP library of palettes)
    OUTPUT "bluepal.bmp",r
    FPOS 0x36
ii = 0
    REPT    256
bi = ii&7
gi = (ii>>3)&7
ri = (ii>>5)&6
        IF 0 < ri
ri = ri + 1
        ENDIF
blue = bi * 255 / 7
green = gi * 255 / 7
red = ri * 255 / 7
        BYTE blue, green, red, 0
ii = ii+1
    ENDR
    OUTEND
    END ; palette EDIT
*/

; simple test to create V1.3 files with sjasmplus

    OPT --syntax=abf
    DEVICE ZXSPECTRUMNEXT

    ;; ; "Next" picture (tilemap type)
    ; 	nextreg	$6B,10000011b	; 40x32 512 tiles
    ; 	nextreg	$6C,0
    ; 	nextreg	$6E,$40		; 2560  BYTES
    ; 	nextreg	$6F,$4A		; 13824 BYTES

    org $4000
    INCBIN  "NEXT/MAP.BIN"
    org $4A00
    INCBIN  "NEXT/CHR.BIN"

    org $8000
start:
    di
    ld      a,$83
    ld      i,a
    im 2
    ei
    ld      hl,320

.Loop2:
    halt

    dec     hl
    ld      a,l
    nextreg $16,a   ; scroll X LSB
    ld      a,h
    and     1
    nextreg $71,a   ; scroll X MSB
    jr      nz,.notXScrollAt0
    or      l
    jr      nz,.notXScrollAt0
    ld      hl,320
.notXScrollAt0:
    ld      e,31
    inc     d
    ld      a,d
    nextreg $17,a   ; scroll Y
    swapnib
.rainbow_offsets:   ; every cca. 8 scanlines change palette offset for Layer2
    inc     a
;     and     $03     ; do just four of them
    and     $0F     ; do all of them
    or      TARGETMODE     ; add mode info
    nextreg $70,a
    ; T states to wait =228*8*8-(18+5+8+8+8+13) = 14532T, djnz 14/9
    ld      b,227
.waitLoop:
    .10     nop         ; 50T
    djnz    .waitLoop   ; 14/9T
    rlc     b           ; 9T
    dec     e
    jr      nz,.rainbow_offsets
    jr      .Loop2

    ASSERT $ < $8300
    ORG $8300
    BLOCK   257, $84
    ORG $8484
im2Handler:
    di
    ei
    ret

    ORG $9000   ; copper CODE
CopperCode:
    MACRO   CNOP
        DW  0
    ENDM
    MACRO   CWAIT line?
@__w = line? + $8000 + (39<<9)
        DB  high @__w, low @__w
    ENDM
    MACRO   CMOV reg?, val?
        DB  reg?, val?
    ENDM
FRAMEW_MAX EQU 238
FRAMEW_MIN EQU 192+32
framew_line = FRAMEW_MAX
    MACRO   CW_FRAME
        CWAIT framew_line
framew_line = framew_line - 1
        IF framew_line < FRAMEW_MIN
framew_line = FRAMEW_MAX
            CWAIT framew_line
        ENDIF
    ENDM

    OPT listoff

    CMOV    $43, %1'001'000'0       ; select Layer2 first palette, disable auto increment
    CMOV    $40, 200                ; color index 200
    CW_FRAME
    CW_FRAME
    CMOV    $41, %010'010'10
    CW_FRAME
    CW_FRAME
    CW_FRAME
    CMOV    $41, %101'101'11
    CW_FRAME
    CW_FRAME
    CW_FRAME
    CMOV    $41, %010'010'10
    CW_FRAME
    CW_FRAME
    CW_FRAME
    CMOV    $41, 0
    .25 CW_FRAME
    CMOV    $40, 201                ; color index 201
    CMOV    $41, %000'001'01
    CW_FRAME
    CW_FRAME
    CMOV    $41, %000'010'01
    CW_FRAME
    CW_FRAME
    CMOV    $41, %000'010'10
    CW_FRAME
    CW_FRAME
    CMOV    $41, %000'100'10
    CW_FRAME
    CW_FRAME
    CMOV    $41, %001'101'11
    CW_FRAME
    CW_FRAME
    CMOV    $41, %001'110'11
    CW_FRAME
    CW_FRAME
    CMOV    $41, %001'101'11
    CW_FRAME
    CW_FRAME
    CMOV    $41, %000'100'10
    CW_FRAME
    CW_FRAME
    CMOV    $41, %000'010'10
    CW_FRAME
    CW_FRAME
    CMOV    $41, %000'010'01
    CW_FRAME
    CW_FRAME
    CMOV    $41, %000'001'01
    CW_FRAME
    CW_FRAME
    CMOV    $41, 0
    .17 CW_FRAME
    ; second flash, lighter
    CMOV    $40, 200                ; color index 200
    CW_FRAME
    CW_FRAME
    CMOV    $41, %010'010'10
    CW_FRAME
    CW_FRAME
    CMOV    $41, %101'101'11
    CW_FRAME
    CW_FRAME
    CMOV    $41, %111'111'11
    CMOV    $40, 0
    CMOV    $41, %010'010'10
    .6 CW_FRAME
    CMOV    $41, 0
    CMOV    $40, 200                ; color index 200
    CMOV    $41, %101'101'11
    CW_FRAME
    CW_FRAME
    CW_FRAME
    CMOV    $41, %010'010'10
    CW_FRAME
    CW_FRAME
    CW_FRAME
    CMOV    $41, 0
    .25 CW_FRAME
    CMOV    $40, 201                ; color index 201
    CMOV    $41, %000'001'01
    .3 CW_FRAME
    CMOV    $41, %000'010'01
    .3 CW_FRAME
    CMOV    $41, %000'010'10
    .3 CW_FRAME
    CMOV    $41, %000'100'10
    .3 CW_FRAME
    CMOV    $41, %001'101'11
    .3 CW_FRAME
    CMOV    $41, %001'110'11
    .3 CW_FRAME
    CMOV    $41, %010'101'11
    CMOV    $40, 200
    CMOV    $41, %000'010'10
    CMOV    $40, 0
    CMOV    $41, %000'010'10
    .5 CW_FRAME
    CMOV    $41, 0
    CMOV    $40, 200
    CMOV    $41, 0
    CMOV    $40, 201
    CMOV    $41, %001'110'11
    .3 CW_FRAME
    CMOV    $41, %001'101'11
    .3 CW_FRAME
    CMOV    $41, %000'100'10
    .3 CW_FRAME
    CMOV    $41, %000'010'10
    .3 CW_FRAME
    CMOV    $41, %000'010'01
    .3 CW_FRAME
    CMOV    $41, %000'001'01
    .3 CW_FRAME
    CMOV    $41, 0
    .23 CW_FRAME
    
    OPT liston

    ASSERT $ <= $9800
    IF $ < $9800
        BLOCK $9800 - $, 0
    ENDIF

MMpicPal:
    INCBIN  "NEXT/PAL.BIN"
MMpic_6B    EQU     10000011b
MMpic_6C    EQU     0
MMpic_6E    EQU     $40
MMpic_6F    EQU     $4A
    ; just DEBUG to show them in the listing as bytes
    DB      MMpic_6B, MMpic_6C, MMpic_6E, MMpic_6F

    SAVENEX OPEN "test_1.3.nex", start, $8FF0, 20, 3
    SAVENEX CORE 3,0,6
    SAVENEX CFG 0, 0, 0, 0
    SAVENEX CFG3 1, 0, $E000, 2048
    SAVENEX BAR 1, 20, 200, 100
;     SAVENEX BAR 1, 20, 20, 10


    SAVENEX COPPER $$CopperCode, CopperCode & $1FFF
    SAVENEX PALETTE MEM $$MMpicPal, MMpicPal & $1FFF

TARGETMODE  EQU     0       ; 256x192x8
    SAVENEX SCREEN TILE MMpic_6B, MMpic_6C, MMpic_6E, MMpic_6F, 0

; TARGETMODE  EQU     $10     ; 320x256x8
;     SAVENEX SCREEN BMP "bg320x256.bmp", 1, 0

; TARGETMODE  EQU     $20     ; 640x256x4
;     SAVENEX SCREEN BMP "airplane.bmp", 1, 0

    SAVENEX BANK 5, 2, 0, 20
    SAVENEX CLOSE
