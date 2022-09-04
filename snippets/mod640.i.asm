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

    ENDMODULE
