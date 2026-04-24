    MODULE percentCompleted

;-----------------------------------------------------------------------------------------------------
; A = (DE * 100) / HL       ; DE "value" expressed as a percentage of HL "total" (rounded toward zero)
;
; Input constraints:
;   0 <= HL <= 0x7FFF
;   0 <= DE <= HL           ; DE "value" must not exceed HL "total"
;   If DE > HL, the result is undefined (it may coincidentally be correct in some cases)
;
; Output:
;   A = integer percentage in range 0..100, 100 only when DE == HL (also when HL == 0)
;
; Modifies:
;   HL, DE, BC, F           ; preserves shadow and index regs
;
; 84 bytes, performance somewhat vary per input, average around 900T per call
;
calc:
    push    hl
    ld      l,25
    ld      h,e
    ld      e,l
    mul     de      ; DE = high value * 25 = high 16b of 24b
    ex      de,hl
    mul     de      ; DE = low value * 25 = low 16b of 24b
    ld      a,d
    add     hl,a    ; HLE = value * 25
    ld      d,l
    ld      b,h     ; BDE = value * 25
    ld      c,4     ; step = +4 to match *25
    pop     hl      ; HL = total
    xor     a       ; result = 0 and compare constant for following init code
    call    .upscaleTotal       ; returns to caller with A=100 when "total" is zero
    ex      de,hl   ; BHL = value * 25, DE = upscaled total, C = step, A = result 0
    cp      b       ; check if B is zero, then go into 16b subtraction
    jr      z,.divByTotal16b
.divByTotal24b:     ; BHL has B > 0, needs 24b subtractions until B becomes 0
    add     a,c
    sbc     hl,de   ; CF=0 from `add a,c`
    jr      nc,.divByTotal24b
    djnz    .divByTotal24b
    sub     c       ; adjust result for one surplus +step upon entry to 16b loop
.divByTotal16bHit:
    add     a,c     ; add step to result when subtraction worked
.divByTotal16b:
    sbc     hl,de   ; CF=0 from `add a,c`
    jr      nc,.divByTotal16bHit
    ; subtracted too much, adjust step and remainder
    add     hl,de   ; restore remainder
    add     hl,hl   ; upscale remainder ; should never overflow, because subtraction value is < 0x8000
    srl     c       ; halve step
    jp      nz,.divByTotal16b   ; try again till step == 0
    ret
.upscaleTotal:      ; upscale total (HL) into 0x4000..0x7FFF range or till step is +32 (max)
    cp      h
    jr      z,.upscaleSmallHl   ; HL < 256, safe to do full 3x upscale without checks
    bit     6,h
    ret     nz      ; step =  +4, subtracting total (no upscale possible, already maxed out)
    sla     c
    add     hl,hl
    bit     6,h
    ret     nz      ; step =  +8, subtracting total x 2
    sla     c
    add     hl,hl
    bit     6,h
    ret     nz      ; step = +16, subtracting total x 4
    sla     c
    add     hl,hl
    ret             ; step = +32, subtracting total x 8 ; enough upscaling, 3 x 32 = 96, almost 100
.upscaleSmallHl:    ; (0 == H): no need to test bit 6,h, upscale always 3 times (+32 step)
    sla     c
    sla     c
    sla     c
    cp      l       ; ZF = (0 == HL)
    add     hl,hl   ; preserves ZF
    add     hl,hl
    add     hl,hl   ; step = +32, subtracting total x 8
    ret     nz      ; non-zero total will continue (ZF by `cp l`)
    pop     de      ; discard sub-call return address
    ld      a,100   ; return 100% as end result if total == 0
    ret

    DISPLAY "percentCompleted.calc size: ",/A,$-calc

    ENDMODULE
