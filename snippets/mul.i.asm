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
; 11 bytes, 4+4+8+4+8+4+8+10 = 50T
mul_16_8_24_HLE:
    ld      d,a
    ld      h,a
    mul     de      ; DE = E*A
    ex      de,hl   ; HL = E*A, E=L
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

;--------------------------------------------------------------------------------------------
; (uint32)DELC = (uint16 HL)x * (uint16 BC)y
; 29 bytes, 4+4+8+4+4+4+4+8+8+4+4+4+8+11+4+4+8+(12+0|7+4)+4+8+10 = 129/128T (uses af, bc, de, hl)
mul_16_16_32_DELC:
    ld      d,l
    ld      e,c
    mul     de      ; xl*yl
    ld      a,d     ; A = hi(xl*yl)
    ld      d,c
    ld      c,e     ; C = lo(xl*yl)
    ld      e,h
    mul     de      ; yl*xh
    add     de,a    ; DE = xh*yl + hi(xl*yl) (can't overflow)
    ex      de,hl   ; HL = xh*yl + hi(xl*yl), DE = x, B = yh, C = lo(xl*yl)
    ld      a,d     ; A = xh
    ld      d,b
    mul     de      ; yh*xl
    add     hl,de   ; HL = yh*xl + xh*yl + hi(xl*yl), CF=overflow
    ld      d,b
    ld      e,a
    mul     de      ; yh*xh
    jr      nc,.no_cf_to_top    ; resolve carry before `add de,a` which destroys it (core 3.1.5)
    inc     d
.no_cf_to_top:
    ld      a,h
    add     de,a    ; can't overflow
    ret             ; result = DELC

;--------------------------------------------------------------------------------------------
; (uint24)HLE = (uint24 HLE)x * (uint24 BCD)y
; 39 bytes, 182T
mul_24_24_24_HLE:
;__  __  __  x2  x1  x0
;__  __  __  y2  y1  y0 *
;;;;;;;;;;;;;;;;;;;;;;;
;r5  r4  r3  r2  r1  r0
;                 +x0y0
;             +x1y0
;         +x2y0                 ; !1 truncated to 8bit
;             +x0y1
;         +x1y1                 ; !1
;;    +x2y1                     ; !2 truncated completely
;         +x0y2                 ; !1
;;    +x1y2                     ; !2
;;+x2y2                         ; !2
; values preservation: (longest lifecycle) y0[3x] x0[3x] x1[2x] x2[1x] y1[2x] y2[1x] (to discard first)
; x = HLE, y = BCD -> lifecycle: D E L H C B
    ;   A   H  L   B  C   D  E  stack IXL
    ;  **  x2 x1  y2 y1  y0 x0  **     **
    push    de      ; y0,x0
    ld      ixl,d   ; preserve y0
    ld      d,b     ; DE = y2x0, B released
    ld      b,e     ; preserve x0
    mul     de      ; x0y2 = +r2
    ld      a,e
    ;   A   H  L   B  C   D  E  stack IXL
    ;  t2  x2 x1  x0 y1  -- --  y0,x0  y0   ; "t2" is partial r2
    ld      d,b
    ld      e,c
    mul     de      ; x0y1 = +r1
    add     a,d
    ;   A   H  L   B  C   D  E  stack IXL
    ;  t2  x2 x1  x0 y1  -- r1a y0,x0  y0
    ld      d,c     ; C released
    ld      c,e
    ld      e,l
    mul     de      ; x1y1 = +r2
    add     a,e     ; AC = truncated(x*y1 + (x*y2<<8))
    ;   A   H  L   B  C   D  E  stack IXL
    ;  t2  x2 x1  x0 t1  --  -- y0,x0  y0
    ld      d,h
    ld      e,ixl
    ld      h,e
    mul     de      ; x2y0 = +r2
    add     a,e
    ld      d,a     ; x2y0 added to future top byte (currently B)
    ld      e,c
    ex      de,hl
    ;   A   H  L   B  C   D  E  stack IXL
    ;  --  t2 t1  -- --  y0 x1  y0,x0  --
    mul     de      ; x1y0 = +r1
    add     hl,de   ; HL = upper 16 bits of result without x0y0 overflow
    pop     de
    mul     de      ; x0y0 = +r0
    ld      a,d
    add     hl,a    ; HLE = 24b result
    ret
    DISPLAY "mul_24_24_24_HLE code size: ",/A,$-mul_24_24_24_HLE

;--------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------
; signed variants and tips&tricks how to exploit unsigned multiplications for signed values

; general rule 1: for N-bit x N-bit unsigned multiplication, the bottom N-bits of unsigned
; result are identical to bottom N-bits for the same bit patterns interpreted as signed
; values/mul. ie. `mul_16_16_16` works both as signed and unsigned multiplication, as long
; as you don't care about overflow and truncation.
; Same way the `mul de` itself works as 8x8=8 signed multiplication.

; general rule 2: if N-bit x N-bit signed values are miscalculated as unsigned multiplication,
; the negative values are interpreted as value+(1<<N), for example in case of 8bit value it
; is value+$100. This makes the unsigned result to contain in upper bits extra +y, +x or +y+x
; for negative value in x, y or both. Subtracting these from the unsigned result will "fix"
; the result for signed multiplication. See muls_8_8_16 for practical example or grok this:
; (x+$100)*y = x*y+$100*y ; for x negative, the unsigned mul result contains extra "+$100*y"

;--------------------------------------------------------------------------------------------
; (int16)AE = (int8)D * (int8)E
; 15 bytes, 4+8+12+8+12+8+4+10 = 66T (best case 64T)
muls_8_8_16_AE:
    xor     a       ; value to adjust upper byte of result with (starts as zero)
    bit     7,d     ; check sign of D
    jr      z,.d_pos
    sub     e       ; the upper byte will have extra +E
.d_pos:
    bit     7,e     ; check sign of E
    jr      z,.e_pos
    sub     d       ; the upper byte will have extra +D
.e_pos:
    mul     de      ; DE = D*E (unsigned way)
    add     a,d     ; AE = D*E signed result
    ret

;--------------------------------------------------------------------------------------------
; (int16)AL = (int16)DE * (int8)L
; 16 bytes, 4+8+12+0+4+4+8+4+4+8+4+10 = 70T (best case 69T)
; x * y = r16 ; results from unsigned 16x8 multiply for signed arguments are skewed like this:
; + * + = x*y
; - * + = (x+$10000)*y          = x*y + $10000*y
; + * - = x*(y+$100)            = x*y + $100*x
; - * - = (x+$10000)*(y+$100)   = x*y + $10000*y + $100*x + $1000000
; The $10000*y and +$1000000 are truncated from 16b result, so only $100*x (when y<0) is relevant
muls_16_8_16_AL:
    xor     a
    bit     7,l
    jr      z,.y_pos
    sub     e       ; the upper byte will have extra +low(x)
.y_pos:
    ld      h,d     ; HL = x1,y0
    ld      d,l
    mul     de      ; x0*y0
    add     a,d     ; 8..15 bits of partial result adjusted
    ex      de,hl   ; L = bottom 8 bits of result, DE = x1,y0
    mul     de      ; x1*y0
    add     a,e     ; final upper byte of result -> AL = result
    ret

;--------------------------------------------------------------------------------------------
; (int24)HLE = (int16)DE * (int8)A
; 52 bytes, performance depends on arguments sign:
;   ++ 4+4+4+4+10 +8+12 +8+4+8+4+8+10 = 88T
;   -+ 4+4+4+4+10 +8+7 +8+4+4+4+4+8+4+8+10 = 95T
;   +- 4+4+4+4+10 +4+4+8+12 +8+4+8+4+8+4+15+10 = 115T
;   -- 4+4+4+4+10 +4+4+8+7+4+4 +8+4+8+4+8+4+15+10 = 118T
; x * y = r24 ; results from unsigned 16x8 multiply for signed arguments are skewed like this:
; + * + = x*y
; - * + = (x+$10000)*y          = x*y + $10000*y
; + * - = x*(y+$100)            = x*y + $100*x
; - * - = (x+$10000)*(y+$100)   = x*y + $10000*y + $100*x + $1000000
; The +$1000000 is truncated, only $100*x (when y<0) and $10000*y (when x<0) is relevant
muls_16_8_24_HLE:
    ld      h,a
    ld      l,e
    ld      e,a
    or      a
    jp      p,.y_pos
    ld      c,l
    ld      b,d     ; BC = x
    bit     7,d
    jr      z,.x_pos_y_neg
    add     a,b
    ld      b,a     ; BC = x + y<<8
.x_pos_y_neg:
    mul     de      ; x1*y0
    ex      de,hl   ; HL = x1*y0, DE = y0,x0
    mul     de      ; y0*x0
    ld      a,d
    add     hl,a    ; HLE = unsigned result
    or      a
    sbc     hl,bc   ; HLE = signed result (adjusted)
    ret
.y_pos:
    bit     7,d
    jr      z,.x_pos_y_pos
    mul     de      ; x1*y0
    ld      a,d
    sub     h       ; adjust x1*y0 by -y<<8
    ld      d,a
    ex      de,hl   ; HL = adjusted x1*y0, DE = y0,x0
    mul     de      ; y0*x0
    ld      a,d
    add     hl,a    ; HLE = signed result (adjusted)
    ret
.x_pos_y_pos:
    ; identical code with tail of mul_16_8_24_HLE, can be reused if you want both
    mul     de      ; x1*y0
    ex      de,hl   ; HL = x1*y0, DE = y0,x0
    mul     de      ; y0*x0
    ld      a,d
    add     hl,a    ; HLE = unsigned/signed result (no adjustment needed)
    ret

    DISPLAY "muls_16_8_24_HLE code size: ",/A,$-muls_16_8_24_HLE

;--------------------------------------------------------------------------------------------
; (int24)HLE = (int16)DE * (int8)A - more compact variant, but slower
; 30 bytes, 10+8+12+4+10+11+4+4+4+4+4+8+4+8+4+8+4+15+10 = 136T (best case 124T)
muls_16_8_24_HLE_compact:
    ld      hl,0    ; adjust result value = 0
    bit     7,d
    jr      z,.x_pos
    ld      h,a     ; adjust value += ($10000*y)>>8 = y<<8
.x_pos:
    or      a
    jp      p,.y_pos
    add     hl,de   ; adjust value += ($100*x)>>8 = x
.y_pos:
    ld      c,l
    ld      b,h     ; BC = adjust result value, DE = x, A = y
    ld      h,a
    ld      l,e     ; HL = y0,x0
    ld      e,a
    mul     de      ; x1*y0
    ex      de,hl   ; HL = x1*y0, DE = y0,x0
    mul     de      ; y0*x0
    ld      a,d
    add     hl,a    ; HLE = unsigned result
    or      a
    sbc     hl,bc   ; HLE = signed result (adjusted)
    ret

    ENDMODULE
