    MODULE mod640

    ALIGN 256
mod640lut:
        .(256/5)    DB  0, 1, 2, 3, 4
                    DB  0
    ASSERT 256 == $-mod640lut

;--------------------------------------------------------------------------------------------
; for top performance if you can afford 256-byte aligned lookup table (LUT)
; input: HL uint16_t (you can gain extra -7T when input is always < $FF80)
; output: DE uint16_t = HL % 640
; modifies: AF
; 15 bytes + 256 bytes table, 74T (13+256 bytes and 67T for input < $FF80)

hlMod640_lut:
    xor     a
    add     hl,hl
    adc     a,h             ; A = HL/128 + HL/32768 ; works as (HL/128) MOD 5 lookup offset
    ; further adjust result for input $FF80..$FFFF
    adc     a,0             ; you can comment this out if you have limited input values < $FF80
    ld      e,l             ; E = preserve bottom 7 bits of input (shifted left)
    ld      l,a
    ld      h,high mod640lut
    ld      d,(hl)          ; D = (HL/128) MOD 5 => DE = (HL % 640)<<1
    srl     d
    rr      e               ; DE = HL % 640
    ; `ld b,1 : bsrl de,b` would be -1T faster, but trashing B register (and using Z80N extension)
    ret

;--------------------------------------------------------------------------------------------
; Calculates HL = HL % 640, for uin16_t values (0..65535)
; modifies: DE, B, F
; code length: 26 bytes, about 400T total duration
; (for [incomplete] int16_t input you can do `add hl,32640` to get -32640..+32767 as 0..65407)

hlMod640:
    ld      b,6
    ld      de,-640<<5              ; -640<<6 overflows int16_t and makes the loop fail for large inputs
    ; but with only -640<<5 as starting point, the large values need two extra subtractions
    add     hl,de
    jr      nc,.not_included
    add     hl,de
    jr      nc,.not_included
.loop:
    add     hl,de                   ; check if the particular -640<<n is included in the input value
    jr      c,.was_included
.not_included:
    sbc     hl,de                   ; restore HL
.was_included:
    add     hl,hl                   ; shift input value to left in every loop (instead of shifting -640<<n down)
    djnz    .loop
    ; HL = result_10b << 6; shift it back to return it in HL
    ex      de,hl
    ld      b,6
    bsrl    de,b
    ex      de,hl
    ret

    ENDMODULE
