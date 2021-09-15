; ZX Spectrum Next - assembly snippets, requires ZX Spectrum Next with core3.1.5+
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.18.2+)
; The Makefile has the full-rebuild instructions
;
; runsnippet.asm: including all snippets into one file, shows addresses for debugging
;  purposes and after keypress it runs some of the snippets

    DEFINE _INCLUDE_COMPARISONS_TESTS_ ; addd rigorous tests: error turns screen red
    DEFINE _INCLUDE_DIV_10_TESTS_ ; addd rigorous tests: error turns screen red
    DEFINE _INCLUDE_MOD_320_TESTS_ ; addd rigorous tests: error turns screen red
    DEFINE _INCLUDE_MOD_192_TESTS_ ; addd rigorous tests: error turns screen red
    DEFINE _INCLUDE_MOD_40_TESTS_ ; addd rigorous tests: error turns screen red
    DEFINE _INCLUDE_BIT_FUN_TESTS_ ; addd rigorous tests: error turns screen red
    DEFINE _INCLUDE_MUL_TESTS_ ; addd partial tests: error turns screen red

    OPT reset --zxnext --syntax=abfw
    DEVICE ZXSPECTRUMNEXT

    INCLUDE "constants.i.asm"

    ;; include various snippets to the $8000 (16ki Bank2)
    ORG     $8000
    INCLUDE "findVLinesCount.i.asm"
    INCLUDE "comparisons.i.asm"
    IFDEF _INCLUDE_COMPARISONS_TESTS_ : INCLUDE "comparisons.test.i.asm" : ENDIF
    INCLUDE "strings5bPacked.i.asm"
    INCLUDE "detectZ80N.i.asm"
    INCLUDE "div10.i.asm"
    IFDEF _INCLUDE_DIV_10_TESTS_ : INCLUDE "div10.test.i.asm" : ENDIF
    INCLUDE "mod320.i.asm"
    IFDEF _INCLUDE_MOD_320_TESTS_ : INCLUDE "mod320.test.i.asm" : ENDIF
    INCLUDE "mod192.i.asm"
    IFDEF _INCLUDE_MOD_192_TESTS_ : INCLUDE "mod192.test.i.asm" : ENDIF
    INCLUDE "mod40.i.asm"
    IFDEF _INCLUDE_MOD_40_TESTS_ : INCLUDE "mod40.test.i.asm" : ENDIF
    INCLUDE "bit_fun.i.asm"
    IFDEF _INCLUDE_BIT_FUN_TESTS_ : INCLUDE "bit_fun.test.i.asm" : ENDIF
    INCLUDE "mul.i.asm"
    IFDEF _INCLUDE_MUL_TESTS_ : INCLUDE "mul.test.i.asm" : ENDIF

    ASSERT $ <= $C000

    ;; main "test" code displaying snippet addresses and waiting for some key to run few
    ORG $C000

tile_fnt EQU $4000  ; font is ZX classic "ch8" file, starting at $4100 with space character
tile_map EQU $4400  ; leave $400 for font (map is $A00 bytes long 80x32 up to $4E00)

test_start:
    ;; init tilemode, copy font data, print texts
    nextreg TURBO_CONTROL_NR_07,3   ; force 28MHz
    nextreg SPRITE_CONTROL_NR_15,0  ; SLU layers, everything default
    nextreg ULA_CONTROL_NR_68,$80 ,, DISPLAY_CONTROL_NR_69,0    ; ULA off, Layer2 off
    ; tilemode 80x32x1bpp, no-attribute, first palette, tilemap-over-ULA, 512tileId
    nextreg TILEMAP_CONTROL_NR_6B,%1110'1011 ,, TILEMAP_DEFAULT_ATTR_NR_6C,0
    nextreg TILEMAP_BASE_ADR_NR_6E,high tile_map ,, TILEMAP_GFX_ADR_NR_6F,high tile_fnt
    nextreg GLOBAL_TRANSPARENCY_NR_14,$E3   ; default "pink" as key-color
    nextreg TRANSPARENCY_FALLBACK_COL_NR_4A,%000'001'00 ; dark green for fallback/border
    ; reset clip window to [0,0]->[159,255]
    nextreg CLIP_WINDOW_CONTROL_NR_1C,$08
    nextreg CLIP_TILEMAP_NR_1B,0 ,, CLIP_TILEMAP_NR_1B,159
    nextreg CLIP_TILEMAP_NR_1B,0 ,, CLIP_TILEMAP_NR_1B,255
    ; reset tilemap scrolling
    nextreg TILEMAP_XOFFSET_MSB_NR_2F,0 ,, TILEMAP_XOFFSET_LSB_NR_30,0 ,, TILEMAP_YOFFSET_NR_31,0
    ; setup first tilemap palette for "text 1bpp" mode with two variants (green/red background)
    nextreg PALETTE_CONTROL_NR_43,%0'011'0000 ,, PALETTE_INDEX_NR_40,0
    nextreg PALETTE_VALUE_NR_41,%000'010'00 ,, PALETTE_VALUE_NR_41,%110'110'11  ; dark green + white
    nextreg PALETTE_VALUE_NR_41,%010'000'00 ,, PALETTE_VALUE_NR_41,%110'110'11  ; dark red + white
.refresh_screen:
    ; copy font data
    ld      hl,font_data
    ld      de,tile_fnt+$100
    ld      bc,8*(128-32) + 1   ; copy full font + one space after the font data
    ldir
    ; fill screen with space char (here: DE=tile_map+1, [tile_map]==' ')
    ld      hl,tile_map
    ld      bc,80*32-1
    ldir
    ; print texts
    ld      hl,test_texts
.texts_loop:
    xor     a
    or      (hl)
    jr      z,.texts_done
    inc     hl
    ld      c,a
    ldi     a,(hl)  ; fake ld a,(hl) : inc hl
    ldi     e,(hl)  ; fake ld e,(hl) : inc hl
    ld      d,80
    mul     de
    add     de,tile_map
    add     de,a
    ldir
    jr      .texts_loop
.texts_done:
    ;; wait for any key
    call    test_wait_for_key
    ;; run through all the reasonable snippets here
.run_selection_of_snippets:

    ; snippet findVLinesCount from findVLinesCount.i.asm
    call    findVLinesCount
    ld      de,test_findVLinesCount.v
    call    test_HL_to_hex_at_de

    ; snippet Comparison Examples from comparisons.i.asm
    call    comparisons.run
    IFDEF _INCLUDE_COMPARISONS_TESTS_ : call comparisons.test : ENDIF

    ; snippet 5-bit packed strings
    ld      de,str5b.example_Packed
    ld      hl,str5b.example_Unpacked
    ld      b,str5b.numOfExampleStrings
.decodeStrings:
    call    str5b.decode
    djnz    .decodeStrings

    ; snippet detectZ80N
    call    detectZ80N
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,a    ; will ruin the screen output if A was not zero

    ; snippets for "E div 10"
    ; variant "A", 9 bytes, 40T, returns result as 8.5 fixed point in DE, modifies B,DE
    ld      e,69
    call    div10.eDiv10A
    ; variant "B", 14 bytes, 60T, returns result as 8.8 fixed point in DE, modifies A,DE
    ld      e,69
    call    div10.eDiv10B
    ; variant "C", 7 bytes, 33T, returns 8bit result in D for E=0..127, modifies F,DE
    ld      e,69
    call    div10.eDiv10C
    ; rigorous tests doing 0..255/0..255/0..127 for all three variants
    IFDEF _INCLUDE_DIV_10_TESTS_ : call div10.test : ENDIF

    ; snippet for "HL mod 320"
    ld      hl,1234         ; expected result for 1234: HL = 274 ($0112)
    call    mod320.hlMod320
    ld      hl,2345         ; expected result for 2345: HL = 105 ($0069)
    call    mod320.hlMod320_unrolled
    ld      de,3456         ; expected result for 3456: DE = 256 ($0100)
    call    mod320.deMod320_lut
    ; rigorous tests doing full HL=0..65535
    IFDEF _INCLUDE_MOD_320_TESTS_ : call mod320.test : ENDIF

    ; snippet for "HL mod 192"
    ld      hl,1234         ; expected result for 1234: HL = 82 ($0052)
    call    mod192.hlMod192
    ld      hl,2345         ; expected result for 2345: HL = 41 ($0029)
    call    mod192.hlMod192_lut
    ld      hl,3456         ; expected result for 3456: A = 0 ($00)
    call    mod192.hlMod192_lut_B
    ; rigorous tests doing full HL=0..65535
    IFDEF _INCLUDE_MOD_192_TESTS_ : call mod192.test : ENDIF

    ; snippet for "HL mod 40"
    ld      hl,1298         ; expected result for 1298: A = 18 ($0012)
    call    mod40.hlMod40
    ; rigorous tests doing full HL=0..65535
    IFDEF _INCLUDE_MOD_40_TESTS_ : call mod40.test : ENDIF

    ; snippet for "bit fun - rotate nibbles right"
    ld      a,$3c
    call    bit_fun.nibrrca_z80n
    ; rigorous tests doing full A=0..255
    IFDEF _INCLUDE_BIT_FUN_TESTS_ : call bit_fun.test : ENDIF

    ; example values for multiplication snippets
.mul_A = $FF11FF
.mul_B = $FF
    ; snippet for "multiplication - 16x8 bits"
    ld      e,high .mul_A
    ld      l,low .mul_A
    ld      a,low .mul_B
    call    mul.mul_16_8_24_HLE             ; HLE = A * EL
        ; subtract expected result from HLE to visually check correctness (HLDE should end zeroed)
    ld      d,0
    add     hl,-((((.mul_A&$FFFF) * .mul_B)>>8)&$FFFF)
    add     de,-(low ((.mul_A&$FFFF) * .mul_B))

    ; snippet for "multiplication - 24x8 bits"
    ld      hl,$FFFF&(.mul_A>>8)
    ld      e,low .mul_A
    ld      a,low .mul_B
    call    mul.mul_24_8_32_DELC            ; DELC = A * HLE
    ld      b,l                             ; result = DEBC
        ; subtract expected result from DEBC to visually check correctness (DEBC should end zeroed)
    add     bc,-((.mul_A * .mul_B)&$FFFF)
    add     de,-(((.mul_A * .mul_B)>>16)&$FFFF)

    ; snippet for "multiplication - 32x8 + 8 bits" ; DEHLB = D + C * EHLB
    ld      d,$0A
    ld      c,$AA
    ld      e,$13
    ld      hl,$579B
    ld      b,$DF
    call    mul.muladd_32_8_8_40_DEHLB      ; $0A + $AA * $13579BDF
    ; expected DEHLB == $0CD82D8220, C preserved as $AA
    add     de,-$0CD8
    add     hl,-$2D82
    add     bc,-$20AA                       ; DE,HL,BC should end zeroed here

    ; snippet for "multiplication - 32x8 + 8 bits", performance variant ; DEHLB = A + C * HLBE
    ld      a,$0A
    ld      c,$AA
    ld      hl,$1357
    ld      b,$9B
    ld      e,$DF
    call    mul.muladd_32_8_8_40_DEHLB_perf ; $0A + $AA * $13579BDF
    ; expected DEHLB == $0CD82D8220, C preserved as $AA
    add     de,-$0CD8
    add     hl,-$2D82
    add     bc,-$20AA                       ; DE,HL,BC should end zeroed here

    ; snippet for "multiplication - 16x16 = 16 bits" ; AE = HL * DC
    ld      hl,$1357
    ld      d,$FD
    ld      c,$B9
    call    mul.mul_16_16_16_AE ; $1357 * $FDB9
    ; expected AE == $F4DF
    ld      d,a
    add     de,-$F4DF                       ; DE should end zeroed here

    ; partial tests multiplying some hand-picked values
    IFDEF _INCLUDE_MUL_TESTS_ : call mul.test : ENDIF

    ;; refresh screen and snippets texts and wait again for key
    jp      .refresh_screen

test_wait_for_key:
    ld      l,$1F           ; wait for press
    call    .read_keys
    xor     l
    ld      l,a             ; wait for release
.read_keys:
    xor     a
    in      a,(ULA_P_FE)
    xor     l
    and     $1F
    jr      z,.read_keys
    ret

test_A_to_hex_at_hl:
    push    af
    swapnib
    call    .nibble
    pop     af
.nibble:
    and     $0F
    cp      10
    sbc     a,$69
    daa
    ld      (hl),a
    inc     hl
    ret

test_HL_to_hex_at_de:
    ex      de,hl
    ld      a,d
    call    test_A_to_hex_at_hl
    ld      a,e
    call    test_A_to_hex_at_hl
    ex      de,hl
    ret

font_data:
    INCBIN "Envious Bold.ch8"   ; "Envious" font by DamienG https://damieng.com/zx-origins
    DB      ' '                 ; add single space char after font data for clear screen

    DEFARRAY test_hex_digits '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
test_txt_hexadr MACRO   imm16?, name?
.adr = imm16?   ; ok
        DB  '$', test_hex_digits[(.adr>>12)&$F], test_hex_digits[(.adr>>8)&$F]
        DB  test_hex_digits[(.adr>>4)&$F], test_hex_digits[.adr&$F], ' '
        DB  name?
    ENDM

test_texts:
test_t0:
    DB  .e-.s, 10, 2
.s: DB  "This is runsnippet.nex including all snippets for debugging."
.e:
test_t1:
    DB  .e-.s, 5, 4
.s: DB  "Enter your debugger and jump to particular snippet address yourself"
.e:
test_t2:
    DB  .e-.s, 4, 5
.s: DB  "or press a key to run some snippets (fixed selection, some output)."
.e:
test_t3:
    DB  .e-.s, 5, 7
.s: DB  "Code snippets:"
.e:

test_findVLinesCount:
    DB  .e-.s,  4,  9
.s: test_txt_hexadr findVLinesCount, "findVLinesCount [$"
.v: DB  "????]"
.e:

test_Comparison:
    DB  .e-.s, 40,  9
.s: test_txt_hexadr comparisons.run, "Comparison examples"
.e:

test_5bDecode:
    DB  .e-.s,  4, 10
.s: test_txt_hexadr str5b.decode, "19B 5b-packed-string decode"
.e:

test_detectZ80N:
    DB  .e-.s, 40, 10
.s: test_txt_hexadr detectZ80N, "Detect Z80N CPU"
.e:

test_div10A:
    DB  .e-.s,  4, 11
.s: test_txt_hexadr div10.eDiv10A, "a) E-Div-10: 9B (FixPt 8.5)"
.e:

test_div10B:
    DB  .e-.s,  4, 12
.s: test_txt_hexadr div10.eDiv10B, "b) E-Div-10: 14B (FixPt 8.8)"
.e:

test_div10C:
    DB  .e-.s,  4, 13
.s: test_txt_hexadr div10.eDiv10C, "c) E-Div-10: 7B (0..127)"
.e:

test_mod320:
    DB  .e-.s, 40, 11
.s: test_txt_hexadr mod320.hlMod320, "a) HL modulo 320"
.e:

test_mod320_unrolled:
    DB  .e-.s, 40, 12
.s: test_txt_hexadr mod320.hlMod320_unrolled, "b) HL modulo 320 unrolled"
.e:

test_mod320_lut:
    DB  .e-.s, 40, 13
.s: test_txt_hexadr mod320.deMod320_lut, "c) DE modulo 320 with LUT"
.e:

test_mod192:
    DB  .e-.s, 40, 14
.s: test_txt_hexadr mod192.hlMod192, "a) HL modulo 192"
.e:

test_mod192_lut:
    DB  .e-.s, 40, 15
.s: test_txt_hexadr mod192.hlMod192_lut, "b) HL modulo 192 with LUT"
.e:

test_mod192_lut_B:
    DB  .e-.s, 40, 16
.s: test_txt_hexadr mod192.hlMod192_lut_B, "c) HL modulo 192 LUT => A"
.e:

test_mod40:
    DB  .e-.s,  4, 14
.s: test_txt_hexadr mod40.hlMod40, "HL modulo 40"
.e:

test_nibrrca_z80n:
    DB  .e-.s,  4, 15
.s: test_txt_hexadr bit_fun.nibrrca_z80n, "RRCA-like but per nibbles"
.e:

test_mul16x8_24:
    DB  .e-.s,  4, 16
.s: test_txt_hexadr mul.mul_16_8_24_HLE, "MUL 16x8 bits (24b result)"
.e:

test_mul24x8_32:
    DB  .e-.s,  4, 17
.s: test_txt_hexadr mul.mul_24_8_32_DELC, "MUL 24x8 bits (32b result)"
.e:

test_mul32x8_40:
    DB  .e-.s,  4, 18
.s: test_txt_hexadr mul.muladd_32_8_8_40_DEHLB, "a) MULADD bits 32x8+8=40, 17B"
.e:

test_mul32x8_40_perf:
    DB  .e-.s,  4, 19
.s: test_txt_hexadr mul.muladd_32_8_8_40_DEHLB_perf, "b) MULADD 2x faster, 30B"
.e:

test_mul16x16_16:
    DB  .e-.s,  4, 20
.s: test_txt_hexadr mul.mul_16_16_16_AE, "MUL 16x16 bits (16b result)"
.e:

; test_texts list terminator
    DB  0

    ASSERT $ < $FF00            ; there should be at least 256B left for stack space

test_stack EQU 0                ; put stack at the very end of Bank0

    SAVENEX OPEN "runsnippet.nex", test_start, test_stack : SAVENEX CFG 7
    SAVENEX BANK 2, 0
    SAVENEX CLOSE
    CSPECTMAP "runsnippet.map"

    IFNDEF LAUNCH_EMULATOR : DEFINE LAUNCH_EMULATOR 0 : ENDIF
    IF 0 == __ERRORS__ && 0 == __WARNINGS__ && 1 == LAUNCH_EMULATOR
;         SHELLEXEC "runzeseruse show512.nex"
        SHELLEXEC "( sleep 0.1s ; runCSpect -brk -map=runsnippet.map runsnippet.nex ) &"
    ENDIF
