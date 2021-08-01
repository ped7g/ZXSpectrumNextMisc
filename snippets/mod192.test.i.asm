    MODULE mod192

test:
    ld      hl,0        ; value to "mod 192"
    ld      de,0        ; expected result
.loop:
    push    hl
    push    de

    ;call    hlMod192

    ;call    hlMod192_lut

    call    hlMod192_lut_B : ld h,0 : ld l,a    ; HL = A (for rest of test to work)

    pop     de          ; compare result with preserved DE
    or      a
    sbc     hl,de
    pop     hl
    jr      z,.hlIsOk
    ; return signalling error
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret
.hlIsOk:
    inc     de          ; update DE to next result of mod192
    ld      a,-192
    add     a,e
    jr      nc,.deIsOk
    ld      e,a
.deIsOk
    inc     hl          ; update HL for next test
    ld      a,h
    or      l
    jr      nz,.loop    ; keep testing full 0..65535 range
    ret                 ; test finished

    ENDMODULE
