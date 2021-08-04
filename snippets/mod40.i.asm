    MODULE mod40

    ALIGN 256
mod40lut_hi:
    .(256/5) db 0,16,32,8,24
    db 0
    ASSERT 256 == $-mod40lut_hi
mod40lut_low:                       ; low-byte table has extra tail of 32 values when the entry is offset by +32
    .((256+32)/40) db 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39
    db 0,1,2,3,4,5,6,7
    ASSERT 256+32 == $-mod40lut_low ; so in total 288 values are needed to cover all possible look-ups
    ASSERT high(mod40lut_low) == 1+high(mod40lut_hi)    ; tables must follow each other (reason: `inc h`)

;--------------------------------------------------------------------------------------------
; Calculates A = HL % 40, for uin16_t values (0..65535)
; modifies: F, HL
; code length: 10 bytes + 520 bytes in tables, 51T

hlMod40:
    ld      a,l
    ld      l,h
    ld      h,high(mod40lut_hi)
    ld      l,(hl)                  ; result offset from high byte: +0, +16, +32, +8, +24, +0, ...
    inc     h                       ; H = high(mod40lut_low) -> HL address into low lut adjusted by offset
    add     hl,a                    ; HL += original L -> pointing to remainder in the table
    ld      a,(hl)                  ; get the remainder from LUT table directly
    ret

    ENDMODULE
