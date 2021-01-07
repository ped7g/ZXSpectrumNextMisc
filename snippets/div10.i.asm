    MODULE div10

;--------------------------------------------------------------------------------------------
; The two variants of routines below are rather showcase of Z80N instructions, or usable
; to generate tables during init phase, or for performance non-critical parts of code.
; They are lousely inspired by http://z80-heaven.wikidot.com/math#toc25 (classic Z80 code)
; and improved by feedback from Baze.
;
; for performance-critical routines you can hardly beat the LUT table:
;
; eDiv10LUT:
;     ld      d,high Table    ;  7T
;     ld      a,(de)          ;  7T = 14T, 3 bytes + 256 bytes table of results
;     ;...
;     ; ALIGN 256
; Table:
;     ;...

;--------------------------------------------------------------------------------------------
; variant "A": divide E by 10, return 8.8 fixed point result in DE
; modifies: B, DE
; code length: 9 bytes, 40T
; the result accuracy is about 8.5 (bottom 3 bits are inaccurate)

eDiv10A:
    ld      d,%11001'101    ;  7T ; D = 25.625 in 5.3 fixed-point math
    mul     de              ;  8T ; DE = 25.625 * v in 5.11 fixed-point
    ld      b,3             ;  7T
    bsrl    de,b            ;  8T ; DE = 25.625 * v in 8.8 fixed-point
    ret                     ; 10T = 40T (30T without RET)

;--------------------------------------------------------------------------------------------
; variant "B": divide E by 10, return 8.8 fixed point result in DE
; modifies: A, DE
; code length: 14 bytes, 60T
; the result accuracy is near full 8.8 (few values differ by +1 from ideal ones)

eDiv10B:
    ld  a,e     ;  4T ; preserve original E
    ld  d,154   ;  7T
    mul de      ;  8T ; DE = 154 * v    ; D = v * 154/256    = v * 0.6015625
    ld  e,a     ;  4T ; E = v
    ld  a,d     ;  4T ; A = D, to be used as: v * 154/65536  = v * 0.002349853515625
    ld  d,25    ;  7T
    mul de      ;  8T ; DE = 25 * v     ; D = v * 25/256     = v * 0.09765625
    add de,a    ;  8T ; DE = ~25.6 * v  ; D = v * 6554/65536 = v * 0.100006103515625
    ret         ; 10T = 60T (50T without RET)

;--------------------------------------------------------------------------------------------
; variant "C": divide E by 10, return result in D
; modifies: F, DE
; code length: 7 bytes, 33T
; the result is correct for input values 0..127 (for 128 it will return 13 instead of 12)

eDiv10C:        ; does evaluate expression: ((v >> 1) * 52) >> 8
    srl e       ;  8T ; "v >> 1"
    ld  d,52    ;  7T ; 
    mul de      ;  8T ; "* 52" and ">> 8" by using D as result
    ret         ; 10T = 33T (23T without RET)

    ENDMODULE
