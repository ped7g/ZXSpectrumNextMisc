;--------------------------------------------------------------------------------------------
; Multiplication routines focusing on z80n `MUL` instruction usage, in the search of optimal
; (size or performance) code.
;
; Following routines always take input and output in registers which are ideal for
; the routine, and destroy anything else what they need, to strip down any
; preservation/setup code, which can be added for particular use-case later.
; (so the input/output registers choice may be weird at some time)
;
; In some routines there's comment "can't overflow" around `add r16,a`, how to reason about it:
; these are around results from multiplying by 8bit value (arg2), ie. $00..$FF
; and they are related to final calculation of first argument top bits with extra 8 bits for result.
; By multiplying with $100 the result would just shift one byte up, and that still fits into +8 extra
; bits for any input, but as the second argument is only uint8, it can't even reach this $100, so
; the result must fit on the upper end doing final addition, QED.

    MODULE mul

;--------------------------------------------------------------------------------------------
; (uint16)DE = (uint8)D * (uint8)E  ; what the HW instruction `mul` does, just a reminder
; 3 bytes, 8+10 = 18T (MUL is two byte long, 8T, RET is 10T)
mul_8_8_16:
    mul     de
    ret

;--------------------------------------------------------------------------------------------
; (uint24)HLE = (uint16)EL * (uint8)A
; 11 bytes, 4+8+4+4+8+4+8+10 = 50T
mul_16_8_24_HLE:
    ld      d,a
    mul     de      ; DE = E*A
    ex      de,hl   ; HL = E*A, E=L
    ld      d,a
    mul     de      ; DE = L*A
    ld      a,d
    add     hl,a    ; HL = E*A + (L*A)>>8
    ret             ; HLE = EL*A

;--------------------------------------------------------------------------------------------
; (uint32)DELC = (uint24)HLE * (uint8)A
; 19 bytes, 4+8+4+4+4+4+4+8+8+4+4+8+8+10 = 82T
mul_24_8_32_DELC:
    ld      d,a
    mul     de      ; de = E*A
    ld      c,e     ; c = low E*A
    ld      e,l
    ld      l,a     ; hl = H,A
    ld      a,d     ; a = (E*A)>>8
    ld      d,l     ; de = A,L
    mul     de
    add     de,a    ; de = L*A + (E*A)>>8 ; can't overflow because summing sub-multiplication "LE*A" result
    ld      a,d
    ex      de,hl   ; hl = L*A + (E*A)>>8, de = H,A
    mul     de
    add     de,a    ; de = H*A + (L*A + (E*A)>>8)>>8 ; can't overflow (summing final "HLE*A")
    ret             ; result = DELC

;--------------------------------------------------------------------------------------------
; (uint40)DEHLB = (uint32)EHLB * (uint8)C + (uint8)D
; SIZE optimised, 17 bytes, 4*(4+4+4+4+4+4+8+8+10)+3*17 = 251T
muladd_32_8_8_40_DEHLB:
    ; do all four segments: EHLB * C = DEHLB with adding initial D as 8bit add-value
    call    .do_two ; do two segments (LB * C)
    ; do remaining two segments (EH * C)
.do_two:
    call    .do_one ; do two segments (call + fallthrough)
.do_one:
    ld      a,d     ; overflow from current result (or initial add-value)
    ld      d,b     ; next 8bits of multiplier (at bottom of current EHLB)
    ld      b,l     ; shift result EHL down to HLB (by 8)
    ld      l,h
    ld      h,e
    ld      e,c     ; arg2
    mul     de      ; DE = arg1_8bit_part * arg2
    add     de,a    ; DE adjusted with overflow from previous sub-multiplication
    ret

;--------------------------------------------------------------------------------------------
; (uint40)DEHLB = (uint32)HLBE * (uint8)C + (uint8)A
;                       ! ^ differs from size-optimised muladd_32_8_8_40_DEHLB !
; performance optimised, 30 bytes, 4*13+8*8+10 = 126T
muladd_32_8_8_40_DEHLB_perf:
    ld      d,c
    mul     de
    add     de,a    ; DE = E * C + A
    ld      a,d
    ld      d,b
    ld      b,e     ; B = result:0:7
    ld      e,c
    mul     de
    add     de,a    ; DE = B * C + ... ; "..." is overflow from lower part of result
    ld      a,d
    ld      d,l
    ld      l,e     ; L = result:8:15
    ld      e,c
    mul     de
    add     de,a    ; DE = L * C + ...
    ld      a,d
    ld      d,h
    ld      h,e     ; H = result:16:23
    ld      e,c
    mul     de
    add     de,a    ; DE = H * C + ...
    ret             ; result = DEHLB

;--------------------------------------------------------------------------------------------
; (uint16)AE = (uint16 HL)x * (uint16 DC)y
; 15 bytes, 4+8+4+4+4+8+4+4+4+8+4+10 = 66T
; also the truncated 16b result is identical to low 16 bits of signed 16x16 multiplication
; so this is also "(uint16)AE = (int16 HL)x * (int16 DC)y" (or "int16" result if there's no overflow)
mul_16_16_16_AE:
    ; ld c,e        ; uncomment to accept "y" argument in DE
    ; HxD xh*yh is not relevant for 16b result at all
    ld      e,l
    mul     de      ; LxD xl*yh
    ld      a,e     ; part of r:8:15
    ld      e,c
    ld      d,h
    mul     de      ; HxC xh*yl
    add     a,e     ; second part of r:8:15
    ld      e,c
    ld      d,l
    mul     de      ; LxC xl*yl (E = r:0:7)
    add     a,d     ; third/last part of r:8:15
    ; uncomment to put result into HL
    ; ld h,a : ld l,e
    ret             ; result = AE
;     DISPLAY "mul_16_16_16_AE code size: ",/A,$-mul_16_16_16_AE

    ENDMODULE