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
mul_8_8_16:
    mul     de
    ret
    ; 8+10 = 18T

;--------------------------------------------------------------------------------------------
; (uint24)HLE = (uint16)EL * (uint8)A
mul_16_8_24_HLE:
    ld      d,a
    mul     de      ; DE = E*A
    ex      de,hl   ; HL = E*A, E=L
    ld      d,a
    mul     de      ; DE = L*A
    ld      a,d
    add     hl,a    ; HL = E*A + (L*A)>>8
    ret             ; HLE = EL*A
    ; 4+8+4+4+8+4+8+10 = 50T

;--------------------------------------------------------------------------------------------
; (uint32)DELC = (uint24)HLE * (uint8)A
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
    ; 4+8+4+4+4+4+4+8+8+4+4+8+8+10 = 82T

    ENDMODULE
