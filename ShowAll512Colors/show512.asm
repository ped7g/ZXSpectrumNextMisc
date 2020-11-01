; "Show 512 colors" example, drawing all possible HW colours in Layer2 256x192 mode
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
; requires ZX Spectrum Next with core3.0+
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.17.1+)
; the source is using currently unreleased (in v1.17.0) "ELSEIF" feature, so
; you need to build sjasmplus from github or wait for next release (hopefully soon)

    OPT reset --zxnext --syntax=abfw
    DEVICE ZXSPECTRUMNEXT

    INCLUDE "constants.i.asm"

    ORG $E000
start:
        di
        nextreg TURBO_CONTROL_NR_07,3                   ; 28MHz
        nextreg LAYER2_RAM_BANK_NR_12,9                 ; Layer 2 from bank 9 (NextZXOS default)
        nextreg SPRITE_CONTROL_NR_15,%0'0'0'000'1'1     ; SLU, no clipping, sprites visible + over border
        nextreg LAYER2_CONTROL_NR_70,%00'00'0000        ; 256x192x8 mode, palette offset +0
        nextreg DISPLAY_CONTROL_NR_69,%1'0'000000       ; Layer 2 ON, ULA shadow off, Timex = 0
        nextreg LAYER2_XOFFSET_MSB_NR_71,0 ,, LAYER2_XOFFSET_NR_16,0 ,, LAYER2_YOFFSET_NR_17,0  ; layer2 X/Y offset = [+0, +0]
        nextreg CLIP_WINDOW_CONTROL_NR_1C,1             ; layer2 clip 256x192
        nextreg CLIP_LAYER2_NR_18,0 ,, CLIP_LAYER2_NR_18,255 ,, CLIP_LAYER2_NR_18,0 ,, CLIP_LAYER2_NR_18,191

    ; map 48kiB of Layer2 to $0000..$BFFF address range
        nextreg MMU0_0000_NR_50,2*9+0
        nextreg MMU1_2000_NR_51,2*9+1
        nextreg MMU2_4000_NR_52,2*10+0
        nextreg MMU3_6000_NR_53,2*10+1
        nextreg MMU4_8000_NR_54,2*11+0
        nextreg MMU5_A000_NR_55,2*11+1

    ; draw the color tiles, each tile is 8x8 pixels, the whole map is 32x16 (512 colors)
    ; indexes goes from 255 to 0, two times (copper will switch palette in the middle)
        ; clear 32 lines with black (from second palette)
        xor     a
        ld      hl,0
        ld      de,1
        ld      bc,256*32
        ld      (hl),a
        ldir
        ; draw 8px x 8px tiles going from index 255 downward to 0 twice (32x16 tiles)
        ld      b,16                    ; 32x16 tiles (going $FF..$00 twice)
        ; A = 0 (will start drawing from --A => 255)
tilesRowLoop:
        call    drawColorTilesAtHl
        djnz    tilesRowLoop
        ; clear bottom 32 lines with black (from second palette)
        ; A == 0, HL == address to draw ($A000)
        ld      d,h
        ld      e,l
        inc     e
        ld      bc,256*32-1
        ld      (hl),a
        ldir

    ; setup the palettes
    ; - first palette covers "brighter" colors the upper half
    ; - second palette covers "darker" colors for bottom half and regular black
    ; the colors are not directly equal to index, but grouped together with
    ; same sum of channel values ("wanna be luminance" order, but is not)
    ; but 0 in second palette is black #000, and 255 in first palette is white #777
        ld      hl,palettesData
        nextreg PALETTE_CONTROL_NR_43,%0'101'010'0      ; second Layer2 palette to set (+show)
        call    uploadFullPalette
        nextreg PALETTE_CONTROL_NR_43,%0'001'010'0      ; first Layer2 palette to set
        call    uploadFullPalette

    ; upload and start the copper code flipping the palettes at required video lines
        nextreg COPPER_CONTROL_LO_NR_61,0
        nextreg COPPER_CONTROL_HI_NR_62,0
        ; cu.WAIT(line=31,h=52 (left border))
        nextreg COPPER_DATA_16B_NR_63,$80|(52<<1)
        nextreg COPPER_DATA_16B_NR_63,31
        ; cu.MOVE(PALETTE_CONTROL_NR_43,%0'000'000'0)   ; select first palette
        nextreg COPPER_DATA_16B_NR_63,PALETTE_CONTROL_NR_43
        nextreg COPPER_DATA_16B_NR_63,%0'000'000'0
        ; cu.MOVE(GLOBAL_TRANSPARENCY_NR_14,$01) to inhibit transparency in upper half
        nextreg COPPER_DATA_16B_NR_63,GLOBAL_TRANSPARENCY_NR_14
        nextreg COPPER_DATA_16B_NR_63,1
        ; cu.WAIT(line=31+64,h=52 (left border))
        nextreg COPPER_DATA_16B_NR_63,$80|(52<<1)
        nextreg COPPER_DATA_16B_NR_63,31+64             ; +64 because 8 row tiles per 8px
        ; cu.MOVE(PALETTE_CONTROL_NR_43,%0'000'010'0)   ; select second palette
        nextreg COPPER_DATA_16B_NR_63,PALETTE_CONTROL_NR_43
        nextreg COPPER_DATA_16B_NR_63,%0'000'010'0
        ; cu.MOVE(GLOBAL_TRANSPARENCY_NR_14,$E3) to inhibit transparency in bottom half
        nextreg COPPER_DATA_16B_NR_63,GLOBAL_TRANSPARENCY_NR_14
        nextreg COPPER_DATA_16B_NR_63,$E3
        ; cu.HALT
        nextreg COPPER_DATA_16B_NR_63,$FF
        nextreg COPPER_DATA_16B_NR_63,$FF
        ; start the copper in VBI-restart mode
        nextreg COPPER_CONTROL_HI_NR_62,%11'00'0000

    ; infinite loop (nothing more to do, the copper code is flipping palettes)
        jr      $

uploadFullPalette:
        nextreg PALETTE_INDEX_NR_40,0                   ; reset color index
        ld      bc,2                                    ; upload 512 bytes (B=0, C=2)
.dataLoop:
        ld      a,(hl)
        inc     hl
        nextreg PALETTE_VALUE_9BIT_NR_44,a
        djnz    .dataLoop
        dec     c
        jr      nz,.dataLoop
        ret

drawColorTilesAtHl:
        push    bc
        ld      c,32
.tilesLoop:
        ld      b,8
        dec     a
.pixelLoop:
        ld      (hl),a
        inc     l
        djnz    .pixelLoop
        dec     c
        jr      nz,.tilesLoop
        ; HL is back to original value (256x `inc l`), copy the full line eight times
        ld      d,h
        ld      e,l
        inc     d
        ld      bc,256*8
        ldir
        pop     bc
        ret

    ; color table - 2x 512 bytes of 9bit colors going from "0" to "511" (black to white)
    ; ordered/grouped by sum of channel values, putting raw-blue toward lower indices
    ; the algorithm to generate the table is sort of "marching 8x8x8 cube" per diagonal
    ; plane-cuts ... see the "test_tga.asm" file for some more comments/ascii-art
    ; (the TGA generator was used to prototype the marching and verify the results)
palettesData:

n=0
vb=0
vr=0
vg=0
dir=0   ; 0 -> from B toward R, !0 -> from R toward B (G is always last resort)
    DUP 512
        DB (vr<<5)|(vg<<2)|(vb>>1), vb&1
        OPT push listoff
        IF 0==dir && 0<vb && vr<7       ; going from B toward R
vb=vb-1
vr=vr+1
        ELSEIF 0!=dir && 0<vr && vb<7   ; going from R toward B
vb=vb+1
vr=vr-1
        ELSEIF 0<(vb+vr) && vg<7        ; has room to move toward G and do another R<->B run
vg=vg+1
            IF (vb<vr && 0<vb) || (0==vr)
vb=vb-1
            ELSE
                ASSERT 0<vr
vr=vr-1
            ENDIF
dir=(vb<vr)
        ELSE                            ; the current "plane" was exhausted, move to next one
            IF vr<vb                    ; make sure B is smaller than R (swap)
temp=vr
vr=vb
vb=temp
            ENDIF
temp=vb     ; swap G with B, and ++B
vb=vg+1
vg=temp
            IF 7<vb     ; make sure B is valid (<=7), add excess to R
vb=vb-1
vr=vr+1
            ENDIF
            IF 7<vr     ; make sure R is valid (<=7), add excess to G
vr=vr-1
vg=vg+1
            ENDIF
dir=0
        ENDIF
n=n+1
        OPT pop
    EDUP

    ; reserve space for stack
        BLOCK   1024, $AA   ; $AAAA is debug filler in case of debugging stack
        BLOCK   10,0        ; avoid non-zero stack data warning of newer sjasmplus (unreleased yet)
stack:  DW      $AAAA
    SAVENEX OPEN "show512.nex", start, stack, 0, 2
    SAVENEX CORE 3,0,0 : SAVENEX CFG 0
    SAVENEX AUTO : SAVENEX CLOSE
    CSPECTMAP "show512.map"

    IFNDEF LAUNCH_EMULATOR : DEFINE LAUNCH_EMULATOR 0 : ENDIF
    IF 0 == __ERRORS__ && 0 == __WARNINGS__ && 1 == LAUNCH_EMULATOR
;         SHELLEXEC "runzeseruse show512.nex"
        SHELLEXEC "runCSpect -brk -map=show512.map show512.nex"
    ENDIF
