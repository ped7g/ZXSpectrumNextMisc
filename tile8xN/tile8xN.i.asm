; ZX Spectrum Next - tilemodes 8xN, requires ZX Spectrum Next with core3.1.5+
; © Peter Helcmanovsky 2021, license: https://opensource.org/licenses/MIT
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.18.2+)
; The Makefile has the full-rebuild instructions
;
; tile8xN.i.asm: routines to set up copper to bend HW into displaying 8xN tiles.
;
; The regular 80x32 tile-mode with 8x8 tiles is used as base, with copper code
; scrolling every text-row slightly up to produce effect of 8xN tile mode.
; The VRAM Bank 5 is used for virtual tile-map from $4000 up to font data.
;
; v0.9 - 2021-08-30             - initial version, missing more docs and example code
;
; ## Dependencies:
; displayedge runtime library (displayedge_rt.i.asm)
;
; ## configurable define values
; VIDEO_FONT_ADR                - Bank 5 address for font data (default $7000)
; DBG_COPPER_REINIT_PERFORMANCE - adds code changing transparency fallback $4A colour (timing)
;
; ## List of functions:
; ChangeRowHeight               - change pixel height of single row (default 6)
;      - does modify code of GetModeData and CopperReinit to operate with new size
; InitVideo                     - set up tilemode 80x42 (except copper), ...
;      - also shifts scanline "0" 33 lines up with nextreg $64
; CopperNeedsReinit             - ZF=0 when copper needs reinit (mode change detected)
; SetCopperIsInitialized        - clears the "needs reinit" flag
; GetModeData                   - gets config data by dspedge.MODE_* value
; CopperReinit                  - generate copper code (when map-config changes)
; ClearScreen                   - sets whole virtual map ($4000..font) to ' ' in color 0
; SetFullTilemapPalette         - (internal) setup tilemode palette
; CalcTileAddress               - memory address of particular single character
; CalcLineAddress               - memory address of particular row (in virtual map)
; WriteSpace                    - fills "rectangle" in virtual map with ' ' in color 0
; Print                         - print C-string
; PrintChar                     - print single character
; AdvancePos                    - advance BC coordinates to next position in virtual map

    ; switch sjasmplus to correct syntax variant
    OPT push reset --zxnext --syntax=abfw

    ; DEFINE DBG_COPPER_REINIT_PERFORMANCE

    ; default values for configuration-defines
    IFNDEF VIDEO_FONT_ADR
        DEFINE VIDEO_FONT_ADR   $7000       ; must fit into $4000..$7FFF region
        ; this also limits the tile-map region, which is always $4000 to VIDEO_FONT_ADR
        ; make sure all tiles fit into the Bank5 region (i.e. $7000 max for 128 tiles)
    ENDIF

    MODULE tile8xN

;;----------------------------------------------------------------------------------------
;; Structure to define new "display map" data (what is displayed where), for CopperReinit.
;; Use array of these with the last one having skipScanlines set to -1 to work
;; as list-terminator item (or DB -1 after the list has same effect).
;; Account for the current "fully visible text rows" from GetModeData, creating layout
;; which fits on screen (if the layout is higher, the generator should survive it, but it
;; isn't recommended situation, the layout should be valid for current video mode).
;; You must skip the reported invisible lines yourself by adding them to "skipScanlines".
;; The xOffset must be valid 0..79 value only, otherwise illegal values will be sent to
;; NextReg $2F (will work on core 3.0 as expected, but may stop working in newer cores).

        STRUCT SDisplayMap
skipScanlines       BYTE    0   ; number of scanlines to skip (with tilemode off), -1 to end list
rows                BYTE    0   ; number of rows (1..N) (row height N: 8px 32, 7px 37, 6px 43, 5px 52, 4px 64)
tilemapY            BYTE    0   ; map 0..101 (but some region is font data)
    ; so if 8kiB reserved for map ($4000..$5FFF) then Y: 0..50 (font at $6000)
    ; if 12kiB reserved for map ($4000..$6FFF) then Y: 0..75 range (font at $7000)
    ; the particular line address in map is: $4000 + tilemapY * 160
xOffset             BYTE    0   ; tile number to start line at (wraps around) 0..79 only!
        ENDS

;;----------------------------------------------------------------------------------------
;; possible "colours" for PrintChar and similar (palette slots):
;; 0 - white on black                1 - black on white (inverse 0)
;; 2 - bright white on black         3 - black on bright white (inverse 2)
;; 4 - light blue on black           5 - light green on black
;; 6 - light cyan on black           7 - light yellow on black
;; 8 - white on blue (+8 sel)        9 - bright white on red (cursor)
;; A - bright white on blue (+8 sel) B - white on dark grey (non-inverse menu/status/etc)
;; C - light blue on blue (+8 sel)   D - light green on blue (+8 sel)
;; E - light cyan on blue (+8 sel)   F - light yellow on blue (+8 sel)

;;----------------------------------------------------------------------------------------
FONT_ADR            EQU     VIDEO_FONT_ADR          ; convert DEFINE to regular symbol
VIRTUAL_ROWS        EQU     (FONT_ADR - $4000)/160  ; 51 for font=$6000, 76 for $7000
HORIZONTAL_COMPARE  EQU     39  ; after the right border is finished in all modes

; array to parse displayedge config data into:
DisplayMarginsArr:  DS      dspedge.S_MARGINS * dspedge.MODE_COUNT  ; 4 * 9 = 36 bytes

RowHeightConfigTab: ; pairs: total fully-visible rows, -(off screen pixels)
                    DB      256/4, 0-4              ; 4px: 64 rows visible
                    DB      256/5, 1-5              ; 5px: 51 rows visible (+1 pixel of 52nd)
                    DB      256/6, 4-6              ; 6px: 42 rows visible (+4 pixels of 43th)
                    DB      256/7, 4-7              ; 7px: 36 rows visible (+4 pixels of 37th)
                    DB      256/8, 0-8              ; 8px: 32 rows visible
; ^ this table can be deleted, if you don't plan to use ChangeRowHeight

;;----------------------------------------------------------------------------------------
;; Reconfigure the library's pixel height of row, supported heights: 4,5,6 (default),7,8
;; Use this before calling GetModeData and CopperReinit, or if you are reconfiguring the
;; library on the fly, you will need to adjust your SDisplayMap array (shrinking/expanding
;; the sub-windows to fit the total number of rows you get by calling GetModeData) and
;; reinit the copper code. Check RowHeightConfigTab above for expected amount of rows.

ChangeRowHeight:    ; this routine can be deleted, if you plan to use only default 6px height
        ; Input:
        ;       A = pixel height of row (4 to 8, not validated, so don't even try...)
        ; Output:
        ;       Library functions are reconfigured to work with the new height of row
        ; Uses:
        ;       AF, HL
                ld      (CopperReinit.rowSz),a
                ld      (GetModeData.rowSz),a       ; patch with pixel height first (4..8 expected/supported)
                ld      hl,RowHeightConfigTab-4*2
                add     a,a
                add     hl,a                        ; HL = rowConfigTab[height]
                ld      a,(hl)                      ; fully-visible rows
                inc     hl
                ld      (GetModeData.rowSz+1),a     ; patch total rows
                ld      a,(hl)                      ; -(off screen pixels) of one more row after fully visible ones
                ld      (GetModeData.offScreen),a   ; patch off-screen pixels
                ret

;;----------------------------------------------------------------------------------------
;; Init all video related settings to default state for 80x42 tilemode (has to be called
;; at least once before the app will enter main loop).
;; In main loop the CopperReinit is enough to call, when the need does arise - reported
;; by CopperNeedsReinit or by change in layout done by app itself.
;;
;; Settings:
;;   ULA disabled (so the "border" and areas between text will become transparency fallback)
;;   Shifts scanline "0" 33 lines up (just one line above the 640x256 area)
;;   4bit tilemode 80x32 with attribute bytes, tiles base to tile8xN.FONT_ADR
;;   resets tilemode clip window (to full 640x256), sets palette
;;   clears tilemap (from $4000 to tile8xN.FONT_ADR)

InitVideo:
            ; make sure the need of copper init is signalled
                ld      a,dspedge.MODE_COUNT
                ld      (CopperNeedsReinit.CurrentMode),a
            ; init next registers
                call    SetNextRegs
                db      $64,33                  ; VIDEO_LINE_OFFSET_NR_64
                ; set other NextRegs to default settings of 80x42 tilemode component
                db      $6B,%11000001           ; Tilemap control
                    ;= +enable +80col -noAttr -palNum -textMode .r -512tile +forceTmOverUla
                ; $6E (TILEMAP_BASE_ADR_NR_6E) is not set up, because copper code does it
                ; The "tilemapY" value in config is like 0..101, into map starting @ $4000
                ; But the tiles def (font) resides after map, cutting down available space
                ; ("tilemapY" can be 0..76 for font at $7000 and 0..51 for font at $6000)
                db      $6F,high FONT_ADR       ; Tiles base offset
                db      $4C,$0f                 ; Transparency colour (last item)
                db      $68,%10000000           ; Disable ULA output
                    ;*; +disableUla-blending-r-r-r-r-r-stencil
                db      $1C,$08                 ; reset tilemap clip window index to 0
                db      $1B,0                   ; reset clip window to 640x256
                db      $1B,159
                db      $1B,0
                db      $1B,255
                db      0                       ; init list terminator

        IFDEF DBG_COPPER_REINIT_PERFORMANCE
                ; fill copper with soft-reset to verify the filler works correctly
                    nextreg $61,0
                    nextreg $62,0               ; stop coppper + write index = 0
                    ld      bc,1024/256
.debugFillCopper:
                    nextreg $63,$02
                    nextreg $63,$01             ; soft-reset request
                    djnz    .debugFillCopper
                    dec     c
                    jr      nz,.debugFillCopper
        ENDIF

                call    SetFullTilemapPalette   ; setup tilemode palette
                ;  |
                ; fallthrough into ClearScreen to clear the $4000..FONT_ADR region
                ;  |
                ;  v

;;----------------------------------------------------------------------------------------
;; Clear screen (write spaces in colour 0 everywhere in $4000..tile8xN.FONT_ADR region)

ClearScreen:
        ; Uses:
        ;       BC, HL
                push    de
                ld      bc,FONT_ADR-$4002
                ld      de,$4002
                ld      hl,$4001
                ldd     (hl),0          ; fake [hl--] = 0
                ld      (hl),' '
                ldir
                pop     de
                ret

;;----------------------------------------------------------------------------------------
;; Set up nextreg-value pairs, the data should follow the `call SetNextRegs` instruction
;; (the data ends with zero terminator)

SetNextRegs:
                pop     hl                      ; read values from return address
                ld      bc,$243B + $0100        ; TBBLUE_REGISTER_SELECT_P_243B with ++B
                xor     a
.set_loop:
                outi                            ; adjust BC to $243B and out *HL++ (reg)
                inc     b                       ; bc = TBBLUE_REGISTER_ACCESS_P_253B
                outinb                          ; out *HL++ (value)
                cp      (hl)
                jr      nz,.set_loop            ; loop until zero-terminator is reached
                ; return back [almost] beyond the data (jumps at the zero-terminator)
                jp      (hl)

;;----------------------------------------------------------------------------------------
;; Detect current video mode, and check if the copper code has to be reprogrammed
;; The copper code itself is technically compatible with any video mode without any
;; change (since the use of videoline offset $64 register), but the user may have
;; different margins for different modes, so it may be worth to adjust the display-map
;; configuration and init the copper.

CopperNeedsReinit:
        ; Output:
        ;       A = current mode 0..dspedge.MODE_COUNT-1
        ;       B = copper-code mode (0..dspedge.MODE_COUNT) (dspedge.MODE_COUNT = no copper yet)
        ;       ZF=0 => copper needs reprogramming (ZF=1 copper code is valid)
        ; Uses:
        ;       side effect: selects NextReg $11 or $03 on I/O port

                call    dspedge.DetectMode
                ;; compare with previously stored value (by copper code generator)
.CurrentMode+1: ld      b,dspedge.MODE_COUNT    ; last copper code programmed for mode X (self-modify-code)
                cp      b               ; set ZF (ZF=1 => copper code is still valid)
                ret

;;----------------------------------------------------------------------------------------
;; Clears the "needs reinit" flag (when code knows it will reinitialize layout)

SetCopperIsInitialized:
        ; Input:
        ;       A = mode number for which copper is initialized (like in dspedge.DetectMode)
                ld      (CopperNeedsReinit.CurrentMode),a
                ret

;;----------------------------------------------------------------------------------------
;; Gets "invisible display margin" data configured by user (with `.displayedge` utility)

GetModeData:
        ; Input:
        ;       A = mode number (like in dspedge.DetectMode)
        ;       DE = displayedge runtime library array with parsed values from cfg file
        ; Output:
        ;       HLDE = L/R/T/B user defined margins (sanitized to 0..31 even if not found in cfg file)
        ;       B = fully visible text rows (in configured row height, 6px default)
        ;       C = configured pixels per row (6)
        ;       A = remaining visible scanlines after last full row (0..5)

                call    dspedge.GetMargins      ; returns BCDE = L/R/T/B (255 for undefined)
            ; sanitize margins returned by displayedge to 0..31 range and redirect L/R from BC to HL
                ld      a,b
                call    dspedge.SanitizeMarginValue
                ld      h,a
                ld      a,c
                call    dspedge.SanitizeMarginValue
                ld      l,a
                ld      a,d
                call    dspedge.SanitizeMarginValue
                ld      d,a
                ld      a,e
                call    dspedge.SanitizeMarginValue
                ld      e,a
            ; calculate fully visible rows, adjusted by margin data (with 6px row -> 258px = 43 rows)
.rowSz+1:       ld      bc,(42<<8)+6    ; B = fully visible rows init (-1 already done), C = pixels per row
.offScreen+1:   ld      a,-2            ; 640x256 tile-mode has 2px invisible (42.66 rows)
                sub d
                sub e                   ; A = -(total invisible scanlines)
.calcVisibleRows:
                add     a,c
                ret     c               ; B = full Npx rows, A = extra scanlines (0..N-1)
                djnz    .calcVisibleRows
            ; can't reach this point ever

;;----------------------------------------------------------------------------------------
;; Re-programs the Copper for the new display-map configuration

CopperReinit:
        ; Input:
        ;       IX = pointer to SDisplayMap array (terminating item has `skipScanlines == -1`)
        ;           The array can't contain just the terminating block,
        ;           there must be at least one row visible
        ; Output:
        ;       Copper is reprogrammed and started (in %11 mode, infinite restart at [0,0] offset by $64)
        ; Uses:
        ;       AF, BC, DE, HL, IX
        ;       side effect: selects NextReg $63 on I/O port

        IFDEF DBG_COPPER_REINIT_PERFORMANCE
                    nextreg $4A,$02                 ; TRANSPARENCY_FALLBACK_COL_NR_4A = blue (debug)
                    ld      bc,4
.debugDelay:
                    nop : djnz .debugDelay
                    dec     c
                    jr      nz,.debugDelay
                    nextreg $4A,$80                 ; TRANSPARENCY_FALLBACK_COL_NR_4A = red (debug)
        ENDIF
                ; set up Copper control to "stop" + index 0
                nextreg $62,0           ; COPPER_CONTROL_HI_NR_62 - STOP first (should not matter)
                nextreg $61,0           ; COPPER_CONTROL_LO_NR_61
                ; set COPPER_DATA for write by OUT (c)
                ld      bc,$243B        ; TBBLUE_REGISTER_SELECT_P_243B
                ld      a,$63           ; COPPER_DATA_16B_NR_63
                out     (c),a           ; select copper data register
                inc     b               ; BC = TBBLUE_REGISTER_ACCESS_P_253B
                ld      hl,$8000        ; copper WAIT scanline 0, h=0
                out     (c),h           ; WAIT for beginning of line to give generator
                out     (c),l           ; at least half of scanline head-start
                ld      h,high($8000 | (HORIZONTAL_COMPARE<<9)) ; copper WAIT scanline 0, H=39
                ;; IX = SDisplayMap array, HL = WAIT_line, BC = $253B I/O port
                out     (c),h           ; initial WAIT
                out     (c),l
                ; start copper before the full code is generated to maximize chance
                ; to catch "this" frame, if the generator was called early after interrupt
                nextreg $62,%11'000'000 ; COPPER_CONTROL_HI_NR_62 ; restart copper from 0 at [0,0] every frame
                call    .DisplayMapLoopEntry
                ; add tilemap OFF after last display map (WAIT is already inserted)
                ld      de,$6B          ; E = TILEMAP_CONTROL_NR_6B, D = 0
                out     (c),e
                out     (c),d           ; switch OFF tilemap
                ; add COPPER_HALT instruction to wait for another restart at [0,0]
                ld      a,$FF
                out     (c),a
                out     (c),a
        IFDEF DBG_COPPER_REINIT_PERFORMANCE
                    nextreg $4A,$08                 ; TRANSPARENCY_FALLBACK_COL_NR_4A = green (debug)
        ENDIF
                ret

.DisplayMapLoop:
                ld      de,SDisplayMap
                add     ix,de           ; ++displayMapPtr
.DisplayMapLoopEntry:
                ;; create "skip scanlines" in copper code (switch OFF + ON tilemode)
                ld      a,(ix + SDisplayMap.skipScanlines)
                or      a
                jr      z,.noSkipScanline
                add     a,l
                ld      l,a             ; Wline += skipLines
                ret     c               ; 256 <= scanline, gone offscreen or hit terminator item
                ld      de,$6B          ; E = TILEMAP_CONTROL_NR_6B, D = 0
                out     (c),e
                out     (c),d           ; switch OFF tilemap
                out     (c),h           ; WAIT
                out     (c),l
.noSkipScanline:
                ;; IX = SDisplayMap array, HL = W-line, BC = $253B I/O port
                ld      de,$6B00 + %11000001    ; TILEMAP_CONTROL_NR_6B = %11000001 (ON)
                out     (c),d
                out     (c),e
                ;; read config: xOffset -> setup X offset
                ld      e,(ix + SDisplayMap.xOffset)
                ld      d,8
                mul     de              ; DE = x offset 0..639 (from valid 0..79 input)
                ld      a,$2F
                out     (c),a           ; TILEMAP_XOFFSET_MSB_NR_2F = high xOffset*8
                out     (c),d
                inc     a
                out     (c),a           ; TILEMAP_XOFFSET_LSB_NR_30 = low xOffset*8
                out     (c),e
                ;; read config: tilemapY, rows
                ld      e,(ix + SDisplayMap.tilemapY)
                ld      d,8
                mul     de              ; DE = tilemapY*8
                ; force setup of tilemap base address ahead of first row
                scf
                ex      af,af'          ; CF in F'
                ; rows counter in regular A
                ld      a,(ix + SDisplayMap.rows)
.RowNpxLoop:
                ;; IX = SDisplayMap, HL = W-line, BC = $253B I/O port, DE = tilemapY*8
                ; A = rows counter, F' = CF when base address must be set
                ex      af,af'
                jp      nc,.skipBaseAddressSetup
                ; set up base address of tilemap = (tilemapY/32 * (high 32*160))
                push    de              ; D = tilemapY/32 (because DE = tilemapY*8)
                ld      e,high (32*160)
                mul     de              ; DE = base address of tile map
                ld      d,$6E           ; TILEMAP_BASE_ADR_NR_6E
                out     (c),d
                out     (c),e
                pop     de
                inc     d               ; adjust D for next base address setup
.skipBaseAddressSetup:
                ; set y offset = tilemapY*8 - scanline
                ld      a,$31           ; TILEMAP_YOFFSET_NR_31 = yOfs
                out     (c),a
                ld      a,e
                sub     l               ; A = low(tilemapY*8 - scanline)
                out     (c),a
.rowSz+1:       ld      a,6             ; scanline += N (configured row pixel height)
                add     hl,a
                out     (c),h           ; WAIT
                out     (c),l
                bit     0,h
                ret     nz              ; 256 <= scanline, gone offscreen
                ; ++tilemapY and set CF in "F'" when base address needs update
                ld      a,8
                add     a,e
                ld      e,a
                ex      af,af'          ; A = rows counter (and preserve CF indicator)
                dec     a
                jp      nz,.RowNpxLoop
                jp      .DisplayMapLoop

;;----------------------------------------------------------------------------------------
;; Palette control
; palette slots:
; 0 - white on black                1 - black on white (inverse 0)
; 2 - bright white on black         3 - black on bright white (inverse 2)
; 4 - light blue on black           5 - light green on black
; 6 - light cyan on black           7 - light yellow on black
; 8 - white on blue (+8 sel)        9 - bright white on red (cursor)
; A - bright white on blue (+8 sel) B - white on dark grey (non-inverse menu/status/etc)
; C - light blue on blue (+8 sel)   D - light green on blue (+8 sel)
; E - light cyan on blue (+8 sel)   F - light yellow on blue (+8 sel)

PalBaseData:
                db  %000'000'00,0       ; black (paper)
                db  %001'001'00,1       ; dark grey (25% ink)
                db  %011'011'01,1       ; light grey (75% ink)
                db  %101'101'10,1       ; white (full ink)
                db  %001'001'11,0       ; blue
                db  %110'001'00,1       ; red
                db  %110'001'11,0       ; magenda
                db  %000'101'01,0       ; dark green
                db  %000'110'11,0       ; cyan
                db  %111'110'00,1       ; yellow
                db  %001'011'10,1       ; light blue
                db  %101'011'00,1       ; brown
                db  %101'110'00,1       ; light green
                db  %011'111'11,1       ; light cyan
                db  %111'111'01,1       ; light yellow
                db  %111'000'11,1       ; transparent E3
PalSlotData:
        ; slot 1: black on white (inverse slot 0)
                db  %101'101'10,1       ; white (paper)
                db  %011'011'01,1       ; light grey (25% ink)
                db  %001'001'00,1       ; dark grey (75% ink)
                db  %000'000'00,0       ; black (full ink)
        ; slot 2: bright white on black
                db  %000'000'00,0       ; black (paper)
                db  %010'010'01,0       ; dark grey (25% ink)
                db  %101'101'10,1       ; light grey (75% ink)
                db  %111'111'11,1       ; bright white (full ink)
        ; slot 3: black on bright white (inverse slot 2)
                db  %111'111'11,1       ; bright white (paper)
                db  %101'101'10,1       ; light grey (25% ink)
                db  %010'010'01,0       ; dark grey (75% ink)
                db  %000'000'00,0       ; black (full ink)
        ; slot 4: light blue on black
                db  %000'000'00,0       ; black (paper)
                db  %000'000'00,1       ; (25% ink)
                db  %000'010'10,0       ; (75% ink)
                db  %001'011'10,1       ; light blue (full ink)
        ; slot 5: light green on black
                db  %000'000'00,0       ; black (paper)
                db  %001'010'00,0       ; (25% ink)
                db  %100'101'00,0       ; (75% ink)
                db  %101'110'00,1       ; light green (full ink)
        ; slot 6: bright cyan on black
                db  %000'000'00,0       ; black (paper)
                db  %000'010'01,0       ; (25% ink)
                db  %001'101'10,1       ; (75% ink)
                db  %011'111'11,1       ; light cyan (full ink)
        ; slot 7: bright yellow on black
                db  %000'000'00,0       ; black (paper)
                db  %010'010'00,0       ; (25% ink)
                db  %101'101'00,1       ; (75% ink)
                db  %111'111'01,1       ; light yellow (full ink)
        ; slot 8: white on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %001'011'01,1       ; dark grey (25% ink)
                db  %011'011'01,1       ; light grey (75% ink)
                db  %101'101'10,1       ; white (full ink)
        ; slot 9: bright white on red (cursor)
                db  %111'000'00,0       ; red (paper)
                db  %111'010'01,0       ; (25% ink)
                db  %111'101'10,1       ; (75% ink)
                db  %111'111'11,1       ; white (full ink)
        ; slot A: bright white on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %010'010'01,1       ; dark grey (25% ink)
                db  %101'111'11,1       ; light grey (75% ink)
                db  %111'111'11,1       ; bright white (full ink)
        ; slot B: white on dark grey (non-inverse menu/status/etc)
                db  %010'010'01,0       ; dark grey (paper)
                db  %011'011'01,1       ; dark grey (33% ink)
                db  %101'101'10,1       ; light grey (75% ink)
                db  %111'111'11,1       ; bright white (full ink)
        ; slot C: light blue on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %000'010'01,1       ; (25% ink)
                db  %000'010'10,1       ; (75% ink)
                db  %001'011'10,1       ; light blue (full ink)
        ; slot D: light green on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %001'010'01,0       ; (25% ink)
                db  %100'101'00,1       ; (75% ink)
                db  %101'110'00,1       ; light green (full ink)
        ; slot E: light cyan on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %000'010'10,0       ; (25% ink)
                db  %001'101'10,1       ; (75% ink)
                db  %011'111'11,1       ; light cyan (full ink)
        ; slot F: light yellow on blue (+8 sel)
                db  %000'010'01,1       ; blue (paper)
                db  %010'010'01,0       ; (25% ink)
                db  %101'101'00,1       ; (75% ink)
                db  %111'111'01,1       ; light yellow (full ink)

;;----------------------------------------------------------------------------------------
;; Setup palette of tilemode

SetFullTilemapPalette:
        ; Uses:
        ;       A, BC, HL, NextRegs [$40, $44]
                nextreg $43,%00110000   ; Set tilemap palette0
                nextreg $40,0           ; reset index
                ld      c,16            ; do 16x16 identical palettes first
.SlotLoop:      ld      b,32
                ld      hl,PalBaseData
.ColourLoop:    ld      a,(hl)
                inc     hl
                nextreg $44,a
                djnz    .ColourLoop
                dec     c
                jr      nz,.SlotLoop
                ; patch each slot (1..F) with specialized letter colours
                ld      hl,PalSlotData
                ld      a,16
.SlotPatchLoop: ld      b,8
                ld      c,a             ; preserve index in C
                nextreg $40,a           ; set index for slot
.ColourLoop2:   ld      a,(hl)
                inc     hl
                nextreg $44,a
                djnz    .ColourLoop2
                ld      a,c
                add     a,16
                jr      nz,.SlotPatchLoop
                ret

;;----------------------------------------------------------------------------------------
;; Calculate address of coordinate [x=C 0..79,y=B 0..101] into the tilemap = HL

CalcTileAddress:
        ; Input:
        ;       B = Y coord (0-101)
        ;       C = X coord (0-79)
        ; Output:
        ;       HL = Tile address (base is $4000)
                push    de
                ld      h,$20
                ld      l,c         ; HL = tilemap base address $4000/2 + X coord
                ld      d,80
                ld      e,b
                mul     de
                add     hl,de
                add     hl,hl
                pop     de
                ret

;;----------------------------------------------------------------------------------------
;; Calculate address of line B (0..101) into the tilemap = HL

CalcLineAddress:
        ; Input:
        ;       B = Y coord (0-101)
        ; Output:
        ;       HL = Tile address (base is $4000)
                push    de
                ld      hl,$4000
                ld      d,160
                ld      e,b
                mul     de
                add     hl,de
                pop     de
                ret

;;----------------------------------------------------------------------------------------
;; Draw a rectangular area of spaces

WriteSpace:
        ; Input
        ;       B = Y coord (0-101) of start
        ;       C = X coord (0-79) of start
        ;       D = height (1-102)
        ;       E = width (1-80)
        ;       A = colour (0-15)
        ; Uses:
        ;       HL, DE, A
                push    bc
                call    CalcTileAddress     ; HL = start corner
                swapnib
                ld      c,a                 ; C = colour
                ld      a,80
                sub     e
                add     a,a                 ; A = 160 - 2*width = deltaHL
.row            ld      b,e                 ; reset width counter
.col            ld      (hl),' '            ; Write space
                inc     l                   ; only L: even -> odd value, can't overflow
                ld      (hl),c              ; Write colour
                inc     hl
                djnz    .col
                add     hl,a                ; HL = next row
                dec     d
                jr      nz,.row
                pop     bc
                ret

;;----------------------------------------------------------------------------------------
;; Low-level printing

Print:
        ; Input
        ;       B = Y coord (0-101)
        ;       C = X coord (0-79)
        ;       DE = string (make sure the string will fit into virtual map!)
        ;       A = colour (0-15)
        ; Output:
        ;       DE = points after string
        ; Uses:
        ;       BC, HL, DE, A

                call    CalcTileAddress
                swapnib
                ld      c,a
                jr      .loopEntry
.l1             ld      (hl),a      ; Write out character
                inc     l           ; only L: even -> odd value, can't overflow to H
                ldi     (hl),c      ; fake [HL++] = C ; Write out attribute
.loopEntry      ldi     a,(de)      ; fake A = [DE++] ; Read next string character
                or      a
                jr      nz,.l1
                ret

PrintChar:
        ; Input:
        ;       B = Y coord (0-101)
        ;       C = X coord (0-79)
        ;       D = Colour (0-15)
        ;       E = character
        ; Output:
        ;       HL = Tilemap address of following position
        ; Uses:
        ;       A
                call    CalcTileAddress
                ld      (hl),e
                inc     l           ; only L: even -> odd value, can't overflow to H
                ld      a,d
                swapnib
                ldi     (hl),a      ; fake [HL++] = A ; Write out attribute
                ret

AdvancePos:
        ; Advances position to next position on screen. This will wrap to next line.
        ; When at the last row of virtual map (depends on tile8xN.FONT_ADR), wraps to Y=0
        ; Input:
        ;       B = Y coord (0-?) (0..50 font=$6000, 0..75 font=$7000)
        ;       C = X coord (0-79)
        ; Output:
        ;       BC = next position XY.
        ; Uses:
        ;       A
                inc     c           ; ++X
                ld      a,-80
                add     a,c
                ret     nc          ; C is 0..79
                ld      c,a         ; C = 0 (if it was 80)
                inc     b           ; ++Y
                ld      a,-VIRTUAL_ROWS
                add     a,b
                ret     nc          ; B is 0..(VIRTUAL_ROWS-1)
                ld      b,a         ; B = 0 (if it was VIRTUAL_ROWS)
                ret

    ENDMODULE

    OPT pop     ; restore original configuration of sjasmplus syntax

; end of file: tile8xN.i.asm
