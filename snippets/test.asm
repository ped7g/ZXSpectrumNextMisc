; ZX Spectrum Next - assembly snippets, requires ZX Spectrum Next with core3.1.5+
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.17.0+)
; The Makefile has the full-rebuild instructions
;
; test.asm: including all snippets into one file, shows addresses for debugging purposes
;  after keypress it runs some of the snippets

    OPT reset --zxnext --syntax=abfw
    DEVICE ZXSPECTRUMNEXT
    ORG $C000

    INCLUDE "constants.i.asm"

tile_fnt EQU $4000  ; font is ZX classic "ch8" file, starting at $4100 with space character
tile_map EQU $4400  ; leave $400 for font (map is $A00 bytes long 80x32 up to $4E00)

test_start:
    ;; init tilemode, copy font data, print texts
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
    ; setup tilemap palette
    nextreg PALETTE_CONTROL_NR_43,%0'011'0000 ,, PALETTE_INDEX_NR_40,0  ; first tilemap palette
    nextreg PALETTE_VALUE_NR_41,%000'010'00 ,, PALETTE_VALUE_NR_41,%110'110'11  ; white ink on dark green
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
test_wait_for_key_press:
    xor     a
    in      a,(ULA_P_FE)
    cpl
    and     $1F
    jr      z,test_wait_for_key_press
.wait_for_release:
    xor     a
    in      a,(ULA_P_FE)
    cpl
    and     $1F
    jr      nz,.wait_for_release
    ;; run through all the reasonable snippets here
test_run_selection_of_snippets:
    ;; stay in infinite loop for any aftermath in debugger
    jr  $

    DEFARRAY test_hex_digits '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
test_txt_hexadr MACRO   imm16?, name?
.adr = imm16?
        DB  '$', test_hex_digits[(.adr>>12)&$F], test_hex_digits[(.adr>>8)&$F]
        DB  test_hex_digits[(.adr>>4)&$F], test_hex_digits[.adr&$F], ": "
        DB  name?
    ENDM

test_texts:
test_t0:
    DB  .e-.s, 13, 2
.s: DB  "This is test.nex including all snippets for debugging."
.e:
test_t1:
    DB  .e-.s, 5, 4
.s: DB  "Enter your debugger and jump to particular snippet address yourself"
.e:
test_t2:
    DB  .e-.s, 4, 5
.s: DB  "or press a key to run some snippets (hardcoded selection, no output)."
.e:
test_t3:
    DB  .e-.s, 5, 7
.s: DB  "Code snippets:"
.e:
test_s0:
    DB  .e-.s, 4, 8
.s: test_txt_hexadr 0x1234, "TestSnippet"
.e:

font_data:
    INCBIN "Envious Bold.ch8"   ; "Envious" font by DamienG https://damieng.com/zx-origins
    DB      ' '                 ; add single space char after font data for clear screen
    ASSERT $ < $FF00            ; there should be at least 256B left for stack space

test_stack EQU 0                ; put stack at the very end of Bank0

    ;; include various snippets to the $8000 (16ki Bank2)
    ORG     $8000

    SAVENEX OPEN "test.nex", test_start, test_stack : SAVENEX CFG 7
    SAVENEX BANK 2, 0
    SAVENEX CLOSE