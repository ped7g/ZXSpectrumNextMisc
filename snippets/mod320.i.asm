    MODULE mod320

;--------------------------------------------------------------------------------------------
;;;;;;;;;;;;;; OBSOLETED by v2 below, unless you are looking for shortest one ;;;;;;;;;;;;;;;
; Calculates HL = HL % 320, for uin16_t values (0..65535)
; modifies: DE, BC, F
; code length: 18 bytes, about 500T total duration (for SUB_POWER 6)
; (for [incomplete] int16_t input you can do `add hl,32640` to get -32640..+32767 as 0..65407)

SUB_POWER   EQU     6       ; 7 is max with `bsrf`, 6 is slightly faster in full 64ki range test
                            ; you can use lower number if you know your usual HL will be lower

hlMod320:
    ld      de,-320<<SUB_POWER      ; 10T
    ld      bc,$0100+SUB_POWER+1    ; 10T
.loop:
    add     hl,de                   ; 11T
    jr      c,.loop                 ; 12T /  7T
    sbc     hl,de                   ; 15T
    dec     c                       ;  4T
    ret     z                       ;  5T / 11T
    bsrf    de,b                    ;  8T ; bsra needs d.7=1 => max power 6, bsrf fills => max power 7
    ; Z80 alternative: sra d : rr e (16T)
    jp      .loop                   ; 10T

;--------------------------------------------------------------------------------------------
; Calculates HL = HL % 320, for uin16_t values (0..65535)
; modifies: DE, B, F
; code length: 26 bytes, about 450T total duration
; (for [incomplete] int16_t input you can do `add hl,32640` to get -32640..+32767 as 0..65407)

hlMod320_v2:
    ld      b,7
    ld      de,-320<<6              ; -320<<7 overflows int16_t and makes the loop fail for large inputs
    ; but with only -320<<6 as starting point, the large values need two extra subtractions
    add     hl,de
    jr      nc,.not_included
    add     hl,de
    jr      nc,.not_included
.loop:
    add     hl,de                   ; check if the particular -320<<n is included in the input value
    jr      c,.was_included
.not_included:
    sbc     hl,de                   ; restore HL
.was_included:
    add     hl,hl                   ; shift input value to left in every loop (instead of shifting -320<<n down)
    djnz    .loop
    ; HL = result_9b << 7; shift it back to return it in HL
    ex      de,hl
    ld      b,7
    bsrl    de,b
    ex      de,hl
    ret

;--------------------------------------------------------------------------------------------
; Calculates A = HL / 320, DE = HL % 320, for uin16_t values (0..65535)
; modifies: HL, B, F
; code length: 30 bytes, about 500T total duration

hlDivMod320:
    ld      a,high(320<<7)-1        ; handle values >= 320<<7 outside of loop (.loop works correctly only for -320<<6 and less)
    cp      h
    jr      nc,.no_power7
    add     hl,-320<<7              ; HL -= 320<<7
.no_power7:
    rla                             ; initial bit of div result
    ld      b,7
    ld      de,-320<<6
.loop:
    add     hl,de                   ; check if the particular -320<<n is included in the input value
    jr      c,.was_included
.not_include:
    sbc     hl,de                   ; restore HL
.was_included:
    rla                             ; TODO: use `adc hl,hl` to store bottom 7 bits of div result? (but it's hassle to extract elegantly)
    add     hl,hl                   ; shift input value to left in every loop (instead of shifting -320<<n down)
    djnz    .loop
    ; HL = result_9b << 7; shift it back to return it in HL, A = HL DIV 320
    ex      de,hl
    ld      b,7
    bsrl    de,b                    ; DE = HL % 320
    ret

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

;--------------------------------------------------------------------------------------------
; for top performance if you can afford 256-byte aligned lookup table (LUT)
; input: DE (depending on the LUT: input is treated either as unsigned or signed integer)
; modifies: HL, AF
; Best case is 33T, worst case 87T, average ~68T
;     DEFINE MOD320LUT_SIGNED ; uncomment for signed input (result is still 0..319)
deMod320_lut:
    ld      h,high mod320lut
    ld      l,d
    ; because (DE%320 == (DE%1280)%320) && (DE%1280 is D=D%5) (hint: 4*320 = 5*256 = 1280)
    ; the value D%5 describes all possible adjustments of E to become result
    ; the lookup table contains 256 control values selected by D%5 value
    ld      d,(hl)
    ;  1 -> D%5 == 0 -> D=0 and E form result
    ;  2 -> D%5 == 4 -> D=0 and E lacks +64 to become result
    ;  64+2, 128+2, 192+2 for D%5 == 1,2,3 -> value to subtract from E
    ;                                         (forms result when CF=0 else lacks +64)
    dec     d
    ret     z               ; D%5 == 0, result is D=0 and original E
    dec     d
    jr      z,.add64toDe    ; D%5 == 4, result is original E + 64
    ld      a,e
    sub     d               ; adjust original E by value from table
    ld      d,0
    ld      e,a             ; DE = result if CF=0
    ret     nc              ; else E belongs to previous 320 range
.add64toDe:
    ld      a,64            ; so here DE needs another +64 to become result
    add     de,a
    ret

    ALIGN 256
mod320lut:
    IFDEF MOD320LUT_SIGNED
        ; signed (manually tested)
        .(128/5)    DB 0+1,64+2,128+2,192+2,0+2 ; mod 5 based values for 0..127
                    DB 0+1,64+2,128+2
                    DB 128+2,192+2,0+2          ; mod 5 based values for -128..-1
        .(128/5)    DB 0+1,64+2,128+2,192+2,0+2
    ELSE
        ; unsigned (mod320.test.i.asm expects unsigned case)
        .(256/5)    DB 0+1,64+2,128+2,192+2,0+2
                    DB 0+1
    ENDIF
    ASSERT 256 == $-mod320lut

    ENDMODULE
