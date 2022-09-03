    MODULE div50

/*
; original routine by jcardoso
;
; divide de by 50 (multiply de by 5.12 and divide by 256)
;
; In:
;       de=value 8.8
; Out:
;       hl=result 8.8
; Dirt:
;       bc, a
;
div_8.8_50:
        xor     a
        ld      c, e
        ld      b, d                    ; keep a copy of de
        ld      d, 30                   ; 0.12 in 8.8  = %00011110
        mul     d, e                    ; multiply decinal part by 0.12
        ld      l, d                    ; store d in l, hl =  de >> 8
        ld      h, a                    ;
        ld      d, b                    ; get initial value of de
        ld      e, 30                   ;
        mul     d, e                    ; multiply integer part by 0.12
        add     hl, de                  ; add to hl
        ld      l, h
        ld      h, a                    ; hl >> 8
        ld      e, c                    ; get orignal e value
        ld      d, 5                    ;
        mul     d, e                    ; multiply e by 5
        ld      e, d
        ld      d, a                    ; de >> 8
        add     hl, de
        ld      d, b                    ; get initial value of d
        ld      e, 5
        mul     d, e                    ; multiply d by 5
        add     hl, de                  ; add to hl
        inc     hl                      ; round value
        ret
        ;=4+4+4+7+8 +4+4+4+7+8 +11+4+4+4+7+8 +4+4+11+4+7+8 +11+6+10 = 157T
*/

;--------------------------------------------------------------------------------------------
; (uint8:8)DE = (uint8:8)DE / 49.98932112890923 ; "uint8:8" is fixed point 8:8 type of value
; modifies: AF, HL, BC
; 30 bytes, 4+4+4+7+4+8 +4+4+8 +8+4+4+7+8 +11+4+4+7+8 +8+10 = 130T
; (jcardoso's routine optimised by Ped7g)
; result accuracy: some results are +1, total sum of errors is 8515 for all 64ki input values
; see cpp_sims/div50_v2_sim.cpp and cpp_sims/div50_v2.csv for full list of errors

div50_fp88_v2:
        ld      c,e
        ld      b,d                     ; keep a copy of de
        ld      h,d
        ld      d,31                    ; ~0.12 (0.12109375) in 8.8  = %00011111
        ld      l,d                     ; hl = in.h, 0.12
        mul     de                      ; de = in.l * 0.12 * 256
        ld      a,d
        ex      de,hl
        mul     de                      ; de = in.h * 0.12
        add     de,a
        ex      de,hl                   ; hl = in * 0.12 (max value 7935 (255*31+30))
        ld      e,c
        ld      d,5
        mul     de                      ; de = in.l * 5 (max value 1275)
        add     hl,de                   ; hl = in.l * 5 + in * 0.12 (can't overflow, max: 1275+7935)
        ld      a,h
        ld      e,b
        ld      d,5
        mul     de                      ; de = in.h * 5
        add     de,a                    ; de = (in * 5.12)>>8
        ret     ; DE out, not HL
        ;=4+4+4+7+4+8 +4+4+8 +8+4+4+7+8 +11+4+4+7+8 +8+10 = 130T

;--------------------------------------------------------------------------------------------
; (uint8:8)DE = (uint8:8)DE / 49.95121951219512 ; "uint8:8" is fixed point 8:8 type of value
; modifies: AF, HL, B
; 17 bytes, 72T
; result accuracy: +0 to +2 difference in result, total sum of errors is 41298 for all 64ki inputs
; see cpp_sims/div50_v3_sim.cpp and cpp_sims/div50_v3.csv for full list of errors

div50_fp88_v3:
        ld      h,d
        ld      d,%101'00100            ; 5.125 in fixed point 3:5 form
        ld      l,d
        mul     de
        ex      de,hl
        mul     de
        ld      a,h
        add     de,a                    ; DE = top 16 bits of (DE * 5.125) in fixed point 11:13
        ld      b,5
        bsrl    de,b                    ; shift DE to become (x * 5.125 / 256) in fixed point 8:8
        ret

;--------------------------------------------------------------------------------------------
; (int8:8)DE = (int8:8)DE / 49.95121951219512 ; "int8:8" is fixed point 8:8 type of value
; 25 bytes, modifies AF, B, HL, performance depends on DE sign:
;   DE >= 0: 8+4+7+4+8 +4+4+8 +8+12 +7+8+10 = 92T
;   DE < 0: 8+4+7+4+8 +4+4+8 +8+7 +16 +7+8+10 = 103T
; x * y = r24 ; results from unsigned 16x8 multiply for signed arguments are skewed like this:
; + * + = x*y
; - * + = (x+$10000)*y = x*y + $10000*y
divs50_fp88:
        bit     7,d                     ; check sign of input value for `jr z` branch
        ld      h,d
        ld      l,%101'00100            ; 5.125 in fixed point 3:5 form
        ld      d,l
        mul     de                      ; x0*y -> top byte is added to x1*y
        ld      a,d
        ex      de,hl
        mul     de                      ; x1*y
        add     de,a                    ; DE = top 16 bits of (x * 5.125) in fixed point 11:13
        jr      z,.x_pos
        add     de,-(%101'00100<<8)+32  ; adjust result to be correct for negative x and improve rounding
.x_pos:
        ld      b,5
        bsra    de,b                    ; shift DE to become (x * 5.125 / 256) in fixed point 8:8
        ret

    ENDMODULE
