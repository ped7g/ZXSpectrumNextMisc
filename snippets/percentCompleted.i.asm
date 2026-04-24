    MODULE percentCompleted

;-------------------------------------------------------------------------------------------------------
; A = (DE * 100) / HL        ; DE "value" expressed as a percentage of HL "total" (rounded toward zero)
;
; Input constraints:
;   0 <= HL <= 0x7FFF
;   0 <= DE <= HL            ; DE "value" must not exceed HL "total"
;   If DE > HL, the result is undefined (it may coincidentally be correct in some cases)
;
; Output:
;   A = integer percentage in range 0..100, 100 only when DE == HL
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
    xor     a       ; result = 0, CF = 0 for .upscaleTotal
    call    .upscaleTotal
    jr      z,.zeroTotalDetected
    srl     h
    rr      l       ; halve HL to get correct upscaled total
    ex      de,hl   ; BHL = value * 25, DE = upscaled total, C = step, A = result 0
    cp      b       ; check if B is zero, then go into 16b subtraction
    jr      z,.divByTotal16b
.divByTotal24b:     ; BHL has B > 0, needs 24b subtractions until B becomes 0
    add     a,c
    sbc     hl,de   ; CF=0 from `add a,c`
    jr      nc,.divByTotal24b
    djnz    .divByTotal24b
.divByTotal16b:
    add     a,c
    sbc     hl,de   ; CF=0 from `add a,c`
    jp      nc,.divByTotal16b
    ; subtracted too much, adjust result, step and remainder
    add     hl,de   ; restore remainder
    sub     c       ; restore result
    add     hl,hl   ; upscale remainder ; should never overflow, because subtraction value is < 0x8000
    sra     c       ; halve step
    jp      nz,.divByTotal16b   ; try again till step == 0
    ret
.zeroTotalDetected:
    ld      a,100   ; 100% in case total is zero
    ret
.upscaleTotal:      ; returns HL x 2, so after return it will be halved back to intended value
    adc     hl,hl   ; ADC to have SF set (CF = 0 from `xor a`)
    ret     z       ; total is zero! Just return 100%
    ret     m       ; step =  +4, subtracting total (HL is x2, needs halving)
    sla     c
    adc     hl,hl   ; ADC to have SF set
    ret     m       ; step =  +8, subtracting total x 2
    sla     c
    adc     hl,hl   ; ADC to have SF set
    ret     m       ; step = +16, subtracting total x 4
    sla     c
    add     hl,hl
    ret             ; step = +32, subtracting total x 8 ; enough upscaling, 3 x 32 = 96, almost 100

    ENDMODULE
