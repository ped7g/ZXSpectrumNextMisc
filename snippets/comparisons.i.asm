; examples of basic arithmetic comparisons for simple types int/uint 8/16 bit
;
; each example consist of two parts:
; - comparison itself (setting flag registers)
; - branching per condition
;
; In the branching part all possible variants are shown, but in your code you need to
; use only the particular condition which is important for your code (probably only
; single line, not all of them).
;
; The branching options are demonstrated by CALL instruction, but you can use any of
; the other conditional intructions JR/JP/RET, except the JR ones not having certain
; tests (pe/po/p/m), for those conditions one is forced to use JP instead of JR.
;
; Also the demonstration works correctly only because each sub-call modifying register
; E with "result value" does NOT modify any flags - if it would modify them,
; the subsequent calls would be branching on invalid state (fragile example).

comparisons:

;; set result markers into E register (helper functions for examples)
.equal:
    ld      e,$00
    ret
.unequal:
    ld      e,$80
    ret
.less:
    ld      e,$FE
    ret
.lessEqual:
    ld      e,$FF
    ret
.greaterEqual:
    ld      e,$01
    ret
.greater:
    ld      e,$02
    ret
.negative:
    ld      e,$2D               ; '-'
    ret
.positive:
    ld      e,$2B               ; '+'
    ret

;; example: uint8 A vs uint8 B
.uint8:
    ; comparison code
    cp      b
    ; equality
    call    z,.equal            ; A == B
    call    nz,.unequal         ; A != B
    ; less/greater+equal (below/above in x86 terminology)
    call    c,.less             ; A < B
    call    nc,.greaterEqual    ; A >= B
    ret

;; example: int8 A vs int8 B
.int8:
    ; comparison code
    cp      b
    ; equality
    call    z,.equal            ; A == B
    call    nz,.unequal         ; A != B
    ; less/greater+equal
    jp      pe,.int8_overflow
.int8_no_overflow:
    call    m,.less             ; A < B  (m = minus)
    call    p,.greaterEqual     ; A >= B (p = plus)
    ret
.int8_overflow:                 ; sign flag is inverted when overflow happened
    call    p,.less             ; A < B
    call    m,.greaterEqual     ; A >= B
    ret

;; example: uint16 HL vs uint16 BC
.uint16:
    ; comparison code
    or      a                   ; clear carry flag for SBC
    sbc     hl,bc               ; destructive to content of HL
    add     hl,bc               ; restore HL (flags: preserves Z, S, P/V, sets same C)
    ; equality
    call    z,.equal            ; HL == BC
    call    nz,.unequal         ; HL != BC
    ; less/greater+equal (below/above in x86 terminology)
    call    c,.less             ; HL < BC
    call    nc,.greaterEqual    ; HL >= BC
    ret

;; example: uint16 HL <= / > uint16 BC (does not preserve HL)
;; - specific case for initial `scf`: changes "<,>=" pair to "<=,>" pair
.uint16_HLgreater:
    ; comparison code
    scf                         ; set carry flag for SBC
    sbc     hl,bc               ; destructive to content of HL
    ; HL restore would require `add hl,bc : inc hl`
    ; less+equal/greater (below/above in x86 terminology)
    call    c,.lessEqual        ; HL <= BC
    call    nc,.greater         ; HL > BC
    ret

;; example: int16 HL vs int16 BC
.int16:
    ; comparison code
    or      a                   ; clear carry flag for SBC
    sbc     hl,bc               ; destructive to content of HL
    add     hl,bc               ; restore HL (flags: preserves Z, S, P/V, sets same C)
    ; equality
    call    z,.equal            ; HL == BC
    call    nz,.unequal         ; HL != BC
    ; less/greater+equal
    jp      pe,.int16_overflow
.int16_no_overflow:
    call    m,.less             ; HL < BC  (m = minus)
    call    p,.greaterEqual     ; HL >= BC (p = plus)
    ret
.int16_overflow:                ; sign flag is inverted when overflow happened
    call    p,.less             ; HL < BC
    call    m,.greaterEqual     ; HL >= BC
    ret

;; example: int16 HL < int16 BC, modifies A, result flag is in zero+carry (like unsigned!)
.int16_v2:
    call    .int16_v2_impl      ; comparison code (implementation)
    ; equality
    call    z,.equal            ; HL == BC
    call    nz,.unequal         ; HL != BC
    ; less/greater+equal
    call    c,.less             ; HL < BC
    call    nc,.greaterEqual    ; HL >= BC
    ret
.int16_v2_impl:
    ld      a,h
    xor     b
    jp      p,.int16_v2_same_sign
.int16_v2_other_sign:           ; sgn HL != sgn BC: one value is negative
    xor     h                   ; A=B, CF=0, SF=BC<0
    ret     m                   ; HL >= 0 > BC, return CF=0, ZF=0
    cp      h
    ret                         ; HL < 0 <= BC, return CF=1, ZF=0
.int16_v2_same_sign:            ; sgn HL == sgn BC: can compare as "unsigned" values
    ld      a,h
    cp      b
    ret     nz
    ld      a,l
    cp      c
    ret

;; example: int16 HL vs uint16 BC (not simple flags-only-result, handling is in code)
.int16_vs_uint16:
    ; conversion to int16 - if BC is above int16 limits, it's clearly "HL < BC" case
    bit     7,b
    call    nz,.less            ; HL < BC (BC is $8000+ value, over the int16 limit)
    ret     nz                  ; abort early (flags PV/SF/CF are NOT set here)
    ; when BC is within int16 limits, then the example `.int16` above can be applied
    jr      .int16

;; example: int16 HL
.is_negative_16:
    bit     7,h
    call    nz,.negative
    call    z,.positive
    ret

;; example: int8 A
.is_negative_8:
    or      a
    call    m,.negative
    call    p,.positive
    ret

;; example: int8 A
.is_zero_8:
    or      a
    call    nz,.unequal
    call    z,.equal
    ret

;; calling all examples (for trying them out in debugger single-stepping over them)
.run:
    ld      e,$EE               ; no result
    ld      a,12                ; first value "A"
    ld      b,34                ; second value "B"
    call    .uint8
    ld      e,$EE               ; no result
    ld      a,23                ; first value "A"
    ld      b,45                ; second value "B"
    call    .int8
    ld      e,$EE               ; no result
    ld      hl,1234             ; first value "HL"
    ld      bc,5678             ; second value "BC"
    call    .uint16
    ld      e,$EE               ; no result
    ld      hl,1234             ; first value "HL"
    ld      bc,1234             ; second value "BC"
    call    .uint16_HLgreater
    ld      e,$EE               ; no result
    ld      hl,2345             ; first value "HL"
    ld      bc,6789             ; second value "BC"
    call    .int16
    ld      e,$EE               ; no result
    ld      hl,3456             ; first value "HL"
    ld      bc,7890             ; second value "BC"
    call    .int16_v2
    ld      e,$EE               ; no result
    ld      hl,-123             ; first value "HL"
    ld      bc,40000            ; second value "BC"
    call    .int16_vs_uint16
    ld      e,$EE
    ld      hl,$7FFF
    call    .is_negative_16
    ld      e,$EE
    ld      a,-123
    call    .is_negative_8
    ld      e,$EE
    ld      a,0
    call    .is_zero_8
    ret
