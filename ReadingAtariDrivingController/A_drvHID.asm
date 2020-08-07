; Reading Atari "driving controller" example
; moving sprite left/right based on the controller inputs
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
; requires ZX Spectrum Next with core3.1.5+
; The controller reading routine is compatible with original ZX
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.14.4+)

    OPT reset --zxnext --syntax=abfw
    INCLUDE "constants.i.asm"
    DEVICE ZXSPECTRUMNEXT : ORG $C000

; by default the example expects the paddle to be configured as "Kempston 2" joystick
PADDLE_PORT     EQU     KEMPSTON_JOY2_P_37  ; (use KEMPSTON_JOY1_P_1F for "Kempston 1")
; PADDLE_PORT     EQU     KEMPSTON_JOY1_P_1F

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Atari driving controller reading code

; atari drive-paddle readings when turning the knob CW/CCW (in bits up/down of Kempston)
; +1: 00 01 11 10 00 01 ...  (0, 4, 12, 8, 0, 4, ... in decimal by reading Kempston)
; -1: 00 10 11 01 00 10 ...  (0, 8, 12, 4, 0, 8, ...)
; You can also imagine it as moving two-bit window over infinite tape: ..001100110011..
;
; IN+OUT: E = old reading, D = current direction (+1 then is CW, -1 is CCW knob turn)
; modify: AF, C
ReadAtariPaddle:
        in  a,(PADDLE_PORT)
        and $0C     ; preserve only up/down for direction reading
        cp  e
        ret z       ; same value read, no change in direction
        ld  c,e     ; temporary copy of old reading
        ld  e,a     ; preserve new reading in E for next call
        ; check if new bit $04 is equal to old bit $08
        rla
        xor c
        and $08
        jr  nz,.clockwiseTurn
; new bit 2 is equal from old bit 3 -> turn counter-clockwise (--D)
        dec d
        ret
.clockwiseTurn: ; new bit 2 is different from old bit 3 -> turn clockwise (++D)
        inc d
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mundane example code

start:
        di
        nextreg DISPLAY_CONTROL_NR_69,$00   ; Layer 2 off, ULA bank 5, Timex mode 0
        nextreg SPRITE_CONTROL_NR_15,%000'000'11 ; LoRes off, layer priority SLU, sprites visible also over border (and no clipping)

    ; upload the sprite gfx pattern to patterns memory
        ; preconfigure the Next for uploading patterns from slot 0
        ld      bc,SPRITE_STATUS_SLOT_SELECT_P_303B
        xor     a
        out     (c),a       ; select slot 0 for patterns (selects also index 0 for attributes)
        ld      hl,SpritePixelData      ; HL = $C000 (beginning of the sprite pixels)
        ld      bc,SPRITE_PATTERN_P_5B  ; sprite pattern-upload I/O port, B=0 (inner loop counter for 256B)
        ; upload 256 bytes of pattern data (otir increments HL and decrements B until zero)
        otir                            ; B=0 ahead, so otir will repeat 256x ("dec b" wraps 0 to 255)

    ; draw ULA background + setup ULA palette
        call    drawUlaBackground

    ; make the sprite 0 visible with pattern 0 (setting up attributes with nextreg)
        nextreg SPRITE_ATTR_SLOT_SEL_NR_34,0
        nextreg SPRITE_ATTR0_NR_35,128+32-16    ; center of screen as initial posX
        nextreg SPRITE_ATTR1_NR_36,180          ; posY near bottom
        nextreg SPRITE_ATTR2_NR_37,0            ; posX8=0, rotate/mirror/pal_offset=0
        nextreg SPRITE_ATTR3_NR_38,$80          ; visible, pattern 0, 4B sprite type

    ; setup "old" reading value to current one
        call    ReadAtariPaddle                 ; E = old reading value of paddle
        ld      d,128+32-16                     ; let the D be position X of sprite (directly)

    ; main loop - keep reading the paddle data all the time, and adjust sprite position
readingLoop:
        call    ReadAtariPaddle     ; will adjust current D (and preserve readings in E)
        ld      a,d
        nextreg SPRITE_ATTR0_NR_35,a            ; set sprite.X directly to D
    ; multiplex the same sprite over several Y positions just ahead of the video beam
        ld      bc,TBBLUE_REGISTER_SELECT_P_243B
        ld      a,VIDEO_LINE_LSB_NR_1F
        out     (c),a
        inc     b
        in      a,(c)                           ; read the $1F nextreg (LSB videoline)
        and     $E0                             ; multiplex at every 32 pixels
        add     a,32+5
        nextreg SPRITE_ATTR1_NR_36,a            ; new sprite.posY
        jr readingLoop

drawUlaBackground:
        ld      hl,MEM_ZX_SCREEN_4000
        ld      de,MEM_ZX_SCREEN_4000+1
        ld      (hl),l
        ld      bc,$1800
        ldir
        ld      (hl),P_GREEN|YELLOW
        ld      c,10
        ldir
        push    hl
        ld      (hl),P_BLACK|BLACK
        ld      c,12
        ldir
        ld      (hl),P_GREEN|YELLOW
        ld      c,19
        ldir
        pop     hl
        ld      bc,$300-20-12-10
        ldir
        ; create few ink dots here and there
        ld      de,$0005
        ld      bc,$000D    ; C*256 dots
.dotLoop:
        pixelad
        setae
        ld      (hl),a
        add     de,$0D03    ; +3 pixels down +7 pixels right
        ld      a,d         ; keep D within 0..191 for Y
        cp      192
        jr      c,.dotYisOk
        add     de,-192*256
.dotYisOk:
        djnz    .dotLoop
        dec     c
        jr      nz,.dotLoop
        ; modify ULA palette to have paper/ink black as two grey levels
        nextreg PALETTE_CONTROL_NR_43,0         ; all extras off, select ULA palette 0
        nextreg PALETTE_INDEX_NR_40,16+0        ; index: paper black
        nextreg PALETTE_VALUE_9BIT_NR_44,%010'010'01    ; grey %010'010'010 (9 bit)
        nextreg PALETTE_VALUE_9BIT_NR_44,0
        nextreg PALETTE_INDEX_NR_40,0           ; index: ink black
        nextreg PALETTE_VALUE_NR_41,%011'011'01 ; grey %011'011'011
        ret

SpritePixelData:
        HEX e3 e3 e3 e3 e3 e3 e3 25 25 e3 e3 e3 e3 e3 e3 e3
        HEX e3 e3 e3 e3 25 25 25 f5 f0 25 25 25 e3 e3 e3 e3
        HEX e3 e3 e3 e3 25 2b 2b f5 f0 2b 2b 25 e3 e3 e3 e3
        HEX e3 e3 e3 e3 e3 e3 e3 f5 f0 e3 e3 e3 e3 e3 e3 e3
        HEX e3 e3 e3 00 00 e3 e3 f0 f0 e3 e3 00 00 e3 e3 e3
        HEX e3 e3 e3 00 00 25 25 f0 f0 25 25 00 00 e3 e3 e3
        HEX e3 e3 e3 00 00 e3 25 00 00 25 e3 00 00 e3 e3 e3
        HEX e3 e3 e3 e3 e3 e3 cc 00 00 cc e3 e3 e3 e3 e3 e3
        HEX e3 e3 e3 e3 f5 f0 f0 00 00 f0 f0 f0 e3 e3 e3 e3
        HEX e3 e3 e3 e3 f0 f0 f0 f5 cc f0 f0 cc e3 e3 e3 e3
        HEX e3 e3 e3 e3 e3 f0 f0 f5 cc f0 cc e3 e3 e3 e3 e3
        HEX e3 e3 e3 00 00 e3 f0 f0 f0 cc e3 00 00 e3 e3 e3
        HEX e3 e3 e3 00 00 25 25 f0 f0 25 25 00 00 e3 e3 e3
        HEX e3 e3 e3 00 00 e3 e3 f0 f0 e3 e3 00 00 e3 e3 e3
        HEX e3 e3 e3 e3 e3 2b 2b 2b 2b 2b 2b e3 e3 e3 e3 e3
        HEX e3 e3 e3 e3 e3 27 27 27 27 27 27 e3 e3 e3 e3 e3

    SAVENEX OPEN "A_drvHID.nex", start, $FF00, 0, 2 : SAVENEX CORE 3,1,5 : SAVENEX CFG 4
    SAVENEX AUTO : SAVENEX CLOSE
    CSPECTMAP "A_drvHID.map"
