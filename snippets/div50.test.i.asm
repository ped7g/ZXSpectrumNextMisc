    MODULE div50

reportError:
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret

test:
    call    test.div50_fp88_v2
    call    test.div50_fp88_v3
    call    test.divs50_fp88
    ret

test.div50_fp88_v2: ; partial test, going from largest expected value, multiplying it by 50 and calling DIV
    ld      hl,1310 ; HL = expected result
.loop:
    push    hl
    ld      d,h
    ld      e,50
    ld      h,e
    mul     de      ; DE = high(expected) * 50
    ex      de,hl
    mul     de      ; DE = low(expected) * 50
    ld      h,l
    ld      l,0
    add     hl,de
    ex      de,hl   ; DE = expected * 50
    call    div50_fp88_v2
    pop     hl
    or      a
    sbc     hl,de
    call    nz,reportError
    dec     de
    add     hl,de   ; --expected
    ld      a,l
    or      h
    jr      nz,.loop
    ret             ; test finished

test.div50_fp88_v3: ; partial test, going through few values in table
    ld      hl,div50_fp88_v3
    ld      (.f),hl
    ld      ix,test.v3_data
.loop:
    ld      e,(ix+0)
    ld      d,(ix+1)
.f+*:   call    0   ; self-modify, two tests using this main loop, so each will set up the call
    ld      a,(ix+2)
    cp      e
    call    nz,reportError
    ld      a,(ix+3)
    cp      d
    call    nz,reportError
    ld      de,4
    add     ix,de
    xor     a
    cp      (ix+0)
    jr      nz,.loop
    ret             ; test finished

test.divs50_fp88:
    ; reuse body of previous test, just different data and different call
    ld      hl,divs50_fp88
    ld      (test.div50_fp88_v3.f),hl
    ld      ix,test.divs_data
    jr      test.div50_fp88_v3.loop

DIV50_TEST_V3_DATA  MACRO   value?, expected_error?
                        DW  value?
                        DW  (value?) / 50 + (expected_error?)
                    ENDM
test.v3_data:
    DIV50_TEST_V3_DATA    896, 0
    DIV50_TEST_V3_DATA   1049, 1    ; first +1 precision error
    DIV50_TEST_V3_DATA   3200, 0
    DIV50_TEST_V3_DATA  12864, 0
    DIV50_TEST_V3_DATA  12992, 1
    DIV50_TEST_V3_DATA  21761, 0
    DIV50_TEST_V3_DATA  33344, 1
    DIV50_TEST_V3_DATA  44672, 1
    DIV50_TEST_V3_DATA  64448, 2    ; +2 precision error
    DB      0

test.divs_data:
    DIV50_TEST_V3_DATA    896, 0
    DIV50_TEST_V3_DATA   1049, 1    ; first +1 precision error in positive values
    DIV50_TEST_V3_DATA  12864, 0
    DIV50_TEST_V3_DATA  12992, 1
    DIV50_TEST_V3_DATA  32640, 1
    DIV50_TEST_V3_DATA -32767, 0
    DIV50_TEST_V3_DATA -31936, -1
    DIV50_TEST_V3_DATA -11392, -1
    DIV50_TEST_V3_DATA -11328, 0
    DIV50_TEST_V3_DATA  -1088, 0
    DIV50_TEST_V3_DATA  -1049, -1   ; first -1 precision error in negative values
    DIV50_TEST_V3_DATA    -50, 0
    DIV50_TEST_V3_DATA    -49, 0
    DIV50_TEST_V3_DATA     -1, 0
    DB      0

    ENDMODULE
