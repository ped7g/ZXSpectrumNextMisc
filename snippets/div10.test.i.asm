    MODULE div10

reportError:
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret

test:
    call    test.EDiv10A
    call    test.EDiv10B
    call    test.EDiv10C
    ret

test.EDiv10A:
    ; only the top 8 bits of 8.5 fixed point results is compared
    xor     a       ; A = expected result for D (H div 10)
    ld      h,a     ; H = source of test value (L = tens-counter)
.outer:
    ld      l,10
.inner:
    ld      e,h
    call    eDiv10A
    cp      d
    call    nz,reportError
    inc     h
    ret     z       ; test finished
    dec     l
    jr      nz,.inner
    inc     a
    jr      .outer

test.EDiv10B:
    ; only the top 8 bits of 8.8 fixed point results is compared
    xor     a
    ld      h,a     ; H = expected result for D (C div 10)
    ld      c,a     ; C = source of test value (B = tens-counter)
.outer:
    ld      b,10
.inner:
    ld      e,c
    call    eDiv10B
    ld      a,d
    cp      h
    call    nz,reportError
    inc     c
    ret     z       ; test finished
    djnz    .inner
    inc     h
    jr      .outer

test.EDiv10C:
    xor     a       ; A = expected result for D (H div 10)
    ld      h,a     ; H = source of test value (L = tens-counter)
.outer:
    ld      l,10
.inner:
    ld      e,h
    call    eDiv10C
    cp      d
    call    nz,reportError
    inc     h
    ret     m       ; test finished when H == 128 (0..127 only is valid range)
    dec     l
    jr      nz,.inner
    inc     a
    jr      .outer

    ENDMODULE
