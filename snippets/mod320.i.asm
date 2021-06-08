    MODULE mod320

;--------------------------------------------------------------------------------------------
; Calculates HL = HL % 320, for uin16_t values (0..65535)
; modifies: DE, BC, F
; code length: 18 bytes, about 500T total duration (for SUB_POWER 6)
; (for int16_t input you can do `add hl,32768` to convert -32768..+32767 to 0..65535)

SUB_POWER   EQU     6       ; 6 is max, makes the loop to start with -64*320
                            ; you can use lower number if you know your max HL will be lower

hlMod320:
    ld      de,-320<<SUB_POWER      ; 10T
    ld      bc,$0100+SUB_POWER+1    ; 10T
.loop:
    add     hl,de                   ; 11T
    jr      c,.loop                 ; 12T /  7T
    sbc     hl,de                   ; 15T
    dec     c                       ;  4T
    ret     z                       ;  5T / 11T
    bsra    de,b                    ;  8T
    ; Z80 alternative: sra d : rr e (16T)
    jp      .loop                   ; 10T

;--------------------------------------------------------------------------------------------
; for smaller HL values it may be worth to unroll the routine, like for HL=0..2048
; modifies: DE, F
; (this is 24 bytes, about 112-150T total duration (or even more for HL 2560+))
hlMod320_unrolled:
    ld      de,-320<<2              ; starting from -4*320 (-1280)
.l2:
    add     hl,de
    jr      c,.l2
    sbc     hl,de
    ld      de,-320<<1
    add     hl,de
    jr      c,.power1_hit
    sbc     hl,de
.power1_hit:
    ld      de,-320<<0
    add     hl,de
    ret     c                       ; power0_hit
    sbc     hl,de
    ret

    ENDMODULE
