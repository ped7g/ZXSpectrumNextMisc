;-------------------------------
; .DISPLAYEDGE
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
; project repository: https://github.com/ped7g/ZXSpectrumNextMisc (report issues here)
;
; Displays green rectangle in tilemap 640x256x4 mode at the user defined edge and let
; the user further adjust per-pixel which pixels are well visible on his display. The
; final configuration can be then stored in /sys/env.cfg file, to be readable
; by any other application. The config is for full width-pixel only (320x256 res)!
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.17.0+)
; For "testing" snapshot for emulators use option: -DTESTING (or uncomment define below)
;
; Reads /sys/env.cfg by default, and allows user to adjust the configuration for
; current video mode (system F3 key to switch between 50/60 Hz => user can define both).
;
; There is displayedge_rt.i.asm source file with "runtime" functions to read
; the configuration file back and have simple API to detect the current mode and
; read back the user defined values.
;
; command line options:
; TODO add cfg filename (maybe "--clone" which will copy current global env.cfg values)
; TODO maybe CLI edit mode to write values for particular mode without interactive part
;
; Changelist:
; v2.2  23/01/2021 P7G    Refactored "runtime" part to make it smaller
; v2.1  23/01/2021 P7G    Fix: $07 and $2F nextregs were not restoring original value
; v2.0  30/10/2020 P7G    New control scheme using arrows and "active corner"
;                           slightly adjusted UI look
; v1.4  28/10/2020 P7G    More robust Z80N CPU check, fix one case of "\n" EOL (to "\r\n")
;                           font replaced with 8x8 Topan font (same as Odin is using)
; v1.3  30/01/2020 P7G    Check for Z80N CPU at start, better O/P keys handler
; v1.2  28/01/2020 P7G    Incorporating the feedback from discord:
;                           keywords prefix "edge_", CRLF eols preferred, calc bak filename
;                           the default cfg filename is now /sys/env.cfg
; v1    25/01/2020 P7G    First fully working version, before public test
; v0    16/01/2020 P7G    Initial version (unfinished)
;
;-------------------------------
; Syntax rules: See the example+test file "test.cfg" in the project git repo.
;-------------------------------

;-------------------------------
    device zxspectrum48
    OPT reset --zxnext --syntax=abfw
;-------------------------------

;     DEFINE TESTING
    DEFINE DISP_ADDRESS     $2000
    DEFINE SP_ADDRESS       $3D00
    IFNDEF TESTING
        DEFINE ORG_ADDRESS      $2000
    ELSE
        OPT --zxnext=cspect
        DEFINE ORG_ADDRESS      $8003
        DEFINE TEST_CODE_PAGE   223         ; using the last page of 2MiB RAM (in emulator)
    ENDIF

    DEFINE TILE_MAP_ADR     $4000           ; 80*32 = 2560 (10*256)
    DEFINE TILE_GFX_ADR     $4A00           ; 128*32 = 4096
                                            ; = 6656 $1A00 -> fits into ULA classic VRAM

    DEFINE CFG_FILENAME     dspedge.defaultCfgFileName

    STRUCT S_MARGINS        ; pixels of margin 0..31 (-1 = undefined margin)
L           BYTE    -1      ; left
R           BYTE    -1      ; right
T           BYTE    -1      ; top
B           BYTE    -1      ; bottom
    ENDS

    STRUCT S_UI_DEFINITIONS
labelDot    WORD    0       ; address where to write display dot
cellAdr     WORD    0       ; address of big table cell on screen at [1,0] char inside
nextMode    BYTE    0
keyword     WORD    0       ; address of keyword used in the CFG file
    ENDS

    STRUCT S_MODE_EDGES
        ; current margins values (must be first four bytes of the structure)
cur         S_MARGINS
        ; original values (from file)
orig        S_MARGINS
        ; UI related config
ui          S_UI_DEFINITIONS
        ; internal flags and intermediate values
modified    BYTE    0       ; if this mode was modified in some way
leftT       BYTE    0       ; full tiles left
rightT      BYTE    0       ; full tiles right
midT        BYTE    0       ; amount of semi top/bottom tiles (w/o left/right corner tiles)
        ; bit masks to redraw gfx tile with Green/Background color (columns/rows in bits)
        ; preserve the order of offsets, they are processed in "++DE" way
maskLeftG   BYTE    0
maskLeftB   BYTE    0
maskRightG  BYTE    0
maskRightB  BYTE    0
maskTopG    BYTE    0
maskTopB    BYTE    0
maskBottomG BYTE    0
maskBottomB BYTE    0
    ENDS

    STRUCT S_STATE
timingIsUnlocked    BYTE    0
corner              BYTE    0   ; 0 left+top, 1 right+bottom
lastCtrlKey         BYTE    0   ; save/reload/quit/hz/timing when waiting for confirm
debounceKey         BYTE    0
modified            BYTE    0   ; set if any of modes (even inactive) is modified
noFileFound         BYTE    0
esxErrorNo          BYTE    1
frameTicker         BYTE    0   ; just 8bit counter for blinking animation
argsPtr             WORD    0
    ENDS

    STRUCT S_PRESERVE
        ; WORDs used, first byte is register number, second byte is preserved value
turbo_07            WORD    TURBO_CONTROL_NR_07
spr_ctrl_15         WORD    SPRITE_CONTROL_NR_15
transp_fallback_4A  WORD    TRANSPARENCY_FALLBACK_COL_NR_4A
tile_transp_4C      WORD    TILEMAP_TRANSPARENCY_I_NR_4C
ula_ctrl_68         WORD    ULA_CONTROL_NR_68
display_ctrl_69     WORD    DISPLAY_CONTROL_NR_69
tile_ctrl_6B        WORD    TILEMAP_CONTROL_NR_6B
tile_def_attr_6C    WORD    TILEMAP_DEFAULT_ATTR_NR_6C
tile_map_adr_6E     WORD    TILEMAP_BASE_ADR_NR_6E
tile_gfx_adr_6F     WORD    TILEMAP_GFX_ADR_NR_6F
tile_xofs_msb_2F    WORD    TILEMAP_XOFFSET_MSB_NR_2F
tile_xofs_lsb_30    WORD    TILEMAP_XOFFSET_LSB_NR_30
tile_yofs_31        WORD    TILEMAP_YOFFSET_NR_31
pal_ctrl_43         WORD    PALETTE_CONTROL_NR_43
pal_idx_40          WORD    PALETTE_INDEX_NR_40
mmu2_52             WORD    MMU2_4000_NR_52
mmu3_53             WORD    MMU3_6000_NR_53
    ; not preserving tilemode clip window coordinates, and tilemode palette
    ; (intentionally, not expecting anyone to need it, or not being able to fix it)
    ; (and subtle sub-states like half-written 9bit color to $44 -> will be lost too)
    ENDS

KEY_DEBOUNCE_WAIT   EQU     8

; stripe blocks: top/left, top/right, left, right, ... (dynamically generated by code)
CHAR_HOT_F          EQU     1
CHAR_HOT_Q          EQU     2
CHAR_HOT_R          EQU     3
CHAR_HOT_S          EQU     4
CHAR_HOT_T          EQU     5
CHAR_HOT_Y          EQU     6
CHAR_FILL_TL        EQU     14
CHAR_FILL_TR        EQU     15
CHAR_FILL_L         EQU     16
CHAR_FILL_R         EQU     17
CHAR_FILL_BL        EQU     18
CHAR_FILL_BR        EQU     19
CHAR_FILL_T         EQU     20
CHAR_FILL_B         EQU     21
    ASSERT 1+CHAR_FILL_TL == CHAR_FILL_TR && 1+CHAR_FILL_L == CHAR_FILL_R && 1+CHAR_FILL_BL == CHAR_FILL_BR ; required
; stripe block: full (all) (part of the font data)
CHAR_FILL_ALL       EQU     22
CHAR_CORNER_TL      EQU     23
CHAR_CORNER_BR      EQU     24
CHAR_DOT_RED        EQU     25
CHAR_DOT_YELLOW     EQU     26
CHAR_DOT_GREEN      EQU     27
CHAR_ARROW_L        EQU     28
CHAR_ARROW_T        EQU     29
CHAR_ARROW_R        EQU     30
CHAR_ARROW_B        EQU     31

;; some further constants, mostly machine/API related
    INCLUDE "constants.i.asm"

;-----------------------------------------------------------------------------
;-- ESX DOS functions
M_DOSVERSION                    equ $88
M_GETSETDRV                     equ $89     ; get current drive (or use A='*'/'$' for current/system drive!)
M_GETHANDLE                     equ $8D     ; get file handle of current dot command
M_GETERR                        equ $93
F_OPEN                          equ $9A
F_CLOSE                         equ $9B
F_READ                          equ $9D
F_WRITE                         equ $9E
F_SEEK                          equ $9F
F_FGETPOS                       equ $A0
F_UNLINK                        equ $AD
F_RENAME                        equ $B0
FA_READ                         equ $01

;; helper macros

ESXDOS      MACRO service? : push hl : pop ix : rst $08 : db service? : ENDM    ; copies HL into IX
NEXTREG2A   MACRO nextreg? : ld a,nextreg? : call readNextReg2A : ENDM
CSP_BREAK   MACRO : IFDEF TESTING : break : ENDIF : ENDM

    ;; reserved space for values (but not initialized, i.e. not part of the binary)

    ; actually in DISPLAYEDGE tool these re-use the same memory area where font was stored

    ; (turned out the DS/BLOCK does overwrite device memory always, so I'm reserving space
    ; here ahead of the real machine code is produced, the real code will later overwrite
    ; the memory as desired)

    ORG tilemapFont_char22
EsxErrorBuffer:     DS      34
ReadMarginsArray:   DS      dspedge.S_MARGINS * dspedge.MODE_COUNT
    ; parsing/writing buffers has to be 256B-aligned
    ALIGN   256
ParsingBuffer:      DS      256
WritingBuffer:      DS      256
WritingBuffer2:     DS      256
BackupFilename:     DS      256

lastReserved:   ASSERT  lastReserved < $3D00 && last < $3D00

;;-------------------------------
;; Start of the machine code itself
;;-------------------------------

        ORG     ORG_ADDRESS
__bin_b DISP    DISP_ADDRESS

start:
        ld      (state.argsPtr),hl  ; preserve pointer to arguments
    ;; detect running environment (bail out if it's not Z80N)
        xor     a
        inc     a               ; A=1, Fc=0
        mirror  a : nop : nop   ; $01 -> $80 on Z80N CPU, maybe "inc h" on Z80
                                ; something else on Z380 and similar (but not mirror A)
        cp      $80
        jr      z,.z80n_opcode_detected
        ; report wrong HW: A=0, Fc=1 (enforce it with regular instruction, no assumptions)
        xor     a
        ld      hl,.ErrTxt_NotZ80N
        scf
        ret
.ErrTxt_NotZ80N:
        DC      "Z80N CPU required"

.z80n_opcode_detected:

    ;; close the file handle of the dot command itself
        rst $08 : db M_GETHANDLE
        rst $08 : db F_CLOSE

    ;; preserve reasonable amount of current machine state (for Quit functionality)
        ; the tilemode clip window and palette will be NOT restored, sorry
        ld      hl,preserved
        ld      b,S_PRESERVE/2
.preserveStateLoop:
        ld      a,(hl)
        inc     hl
        call    readNextReg2A
        ld      (hl),a
        inc     hl
        djnz    .preserveStateLoop
        ; patch the turbo_07 and tile_xofs_msb_2F values to clear reserved bits
        ld      a,(preserved.turbo_07+1)
        and     3
        ld      (preserved.turbo_07+1),a
        ld      a,(preserved.tile_xofs_msb_2F+1)
        and     3
        ld      (preserved.tile_xofs_msb_2F+1),a

    ;; parse the arguments on command line
        ;ld      hl,(state.argsPtr)
        ; no options implemented yet

    ;; autodetect Kempston joystick 1/2 (only one of them will be read if both are valid)
        ld      bc,$37
        call    AutoDetectKempston
        ld      bc,$1F
        call    AutoDetectKempston  ; Kempston 1 check as second to have higher priority

    ;; page-in the bank5 explicitly (just to be sure it's Bank 5 there)
        nextreg MMU2_4000_NR_52,5*2
        nextreg MMU3_6000_NR_53,5*2+1

    ;; copy the font data to Bank 5 VRAM (= release memory for buffer to parse .cfg file)
        ; clear first 22 characters which are not part of the font
        ld      hl,TILE_GFX_ADR
        ld      de,TILE_GFX_ADR+1
        ld      bc,CHAR_FILL_ALL*32-1
        ld      (hl),l
        ldir
        ; copy the font data for chars CHAR_FILL_ALL..127
        ld      hl,tilemapFont_char22
        ld      bc,(128-CHAR_FILL_ALL)*32
        ldir

    ;; create hot-key capital letters (on green background): chars 1,2,3,4,5,6 = FQRSTY
        call    CreateHotKeyCapitals

    ;; set Tilemode 80x32 (640x256x4) and other state of machine
        nextreg TURBO_CONTROL_NR_07,3               ; 28Mhz mode
        nextreg SPRITE_CONTROL_NR_15,%000'100'00    ; layer priority: USL
        nextreg TRANSPARENCY_FALLBACK_COL_NR_4A,0   ; black transparency fallback color
        nextreg TILEMAP_TRANSPARENCY_I_NR_4C,$0F
        nextreg ULA_CONTROL_NR_68,$80               ; disable ULA layer
        nextreg DISPLAY_CONTROL_NR_69,0             ; layer2 off, bank 5, timex=0
        nextreg TILEMAP_CONTROL_NR_6B,%1110'0011    ; 80x32x1, 4bpp, pal0, 512tile-mode, force tile-over-ULA
        nextreg TILEMAP_DEFAULT_ATTR_NR_6C,$00      ; no pal offset, no mirror/rot, 0 bit8
        nextreg TILEMAP_BASE_ADR_NR_6E,high TILE_MAP_ADR
        nextreg TILEMAP_GFX_ADR_NR_6F,high TILE_GFX_ADR
        ; reset tile-clipping write-index, and reset clip window
        nextreg CLIP_WINDOW_CONTROL_NR_1C,%0000'1000
        nextreg CLIP_TILEMAP_NR_1B,0
        nextreg CLIP_TILEMAP_NR_1B,159
        nextreg CLIP_TILEMAP_NR_1B,0
        nextreg CLIP_TILEMAP_NR_1B,255
        ; reset tilemode [x,y] offset
        nextreg TILEMAP_XOFFSET_MSB_NR_2F,0
        nextreg TILEMAP_XOFFSET_LSB_NR_30,0
        nextreg TILEMAP_YOFFSET_NR_31,0
        ; set tilemap palette
        nextreg PALETTE_CONTROL_NR_43,%0'011'0000   ; tilemap pal0
        nextreg PALETTE_INDEX_NR_40,0
        ld      hl,tilemapPalette
        ld      b,tilemapPalette_SZ
.setPalLoop:
        ld      a,(hl)
        inc     hl
        nextreg PALETTE_VALUE_9BIT_NR_44,a
        djnz    .setPalLoop

    ;; read the current cfg file and parse the values
        call    ParseCfgFile

    ;; enter the interactive loop (make sure the screen will get full refresh)

MainLoop:
        ld      a,(state.frameTicker)
        inc     a
        ld      (state.frameTicker),a
    ; check if whole video mode did change
        call    DidVideoModeChange
        jr      z,.noModeChange
    ; calculate EdgesData address for new mode
        ASSERT  S_MODE_EDGES * dspedge.MODE_COUNT < 256  ; guardian in case the struct gets too big
        ld      e,a
        ld      d,S_MODE_EDGES
        mul     de
        add     de,EdgesData
        push    de
        pop     ix
    ; sanitize current margin values (if new or invalid from file)
        call    SanitizeCurMarginValues
    ; redraw full screen
        ; redraw full screen and default texts + table
        call    RedrawMainMap
        ; redraw the edge area itself
        call    RedrawEdge
        ; mark current mode as active
        ld      hl,(ix+S_MODE_EDGES.ui.labelDot)    ; fake
        ld      (hl),CHAR_DOT_RED
        ; mark 50/60Hz label as active
        ld      a,(DidVideoModeChange.oM)
        ld      hl,Label50HzAdr-1
        and     4
        jr      z,.modeIs50Hz
        ld      hl,Label60HzAdr-1
.modeIs50Hz:
        ld      (hl),CHAR_DOT_RED
        ; show if the timing is locked
        ld      a,(DidVideoModeChange.oM)
        and     ~4
        ld      (state.timingIsUnlocked),a  ; non zero value for anything except HDMI50
        jr      z,.hdmiIsLockedAnyWay   ; HDMI vs VGA can be modified only in config mode
        NEXTREG2A   MACHINE_TYPE_NR_03
        and     $08                     ; check if user lock bit is set
        jr      z,.displayTimingIsUnlocked
        xor     a
        ld      (state.timingIsUnlocked),a  ; clear unlocked flag
.hdmiIsLockedAnyWay:
        ld      hl,LockedTxt
        ld      de,(ix+S_MODE_EDGES.ui.labelDot)    ; fake
        add     de,80
        call    DrawHlStringAtDe
.displayTimingIsUnlocked:
        call    RedrawAllTableData

.noModeChange:
        call    RedrawUiControls
        ei
        halt
        call    HandleControls
        jr      MainLoop

;-------------------------------
HandleControls:
    ; count down debounce wait
        ld      a,(state.debounceKey)
        sub     1
        adc     a,0
        ld      (state.debounceKey),a
        ret     nz
        ASSERT 1+state.corner == state.lastCtrlKey  ; make sure they follow each other
        ld      de,(state.corner)   ; E = corner, D = lastCtrlKey, A = 0
    ; check if the confirmation prompt is being displayed, check for Y/other key then
        or      d
        jr      nz,.ConfirmationPromptHandler
    ; check for Kempston joystick (if it was detected)
        call    AdjustCornersByKempston
    ; check for regular control keys
        ; <space> to flip active corner
        ld      a,~(1<<7)           ; eighth row BNM<SS><SPC>
        in      a,(254)
        rra
        jr      nc,.cornerSelector
        ; 5678 for cursor keys - not checking <CS> (caps shift)
        ld      a,~(1<<3)           ; fourth row 54321
        in      a,(254)
        and     $10                 ; keep only key_5
        swapnib
        ld      b,a                 ; in bit 0 (key_5)
        ld      a,~(1<<4)           ; fifth row 67890
        in      a,(254)
        rra
        and     $0E                 ; keep only keys 678 in bits 3:1
        or      b                   ; combined state "6785" (arrows v^><)
        xor     $0F
        jr      nz,.cornerAdjusting ; some cursor key is pressed
        ; Q/R (and read T)
        ld      a,~(1<<2)           ; third row QWERT
        in      a,(254)
        ld      b,1
        rra
        jr      nc,.menuControlPressed  ; Q pressed
        inc     b
        rra
        rra
        rra
        jr      nc,.menuControlPressed  ; R pressed
        ld      c,a ; preserve "T" key for later
        ld      a,~(1<<1)           ; second row ASDFG
        in      a,(254)
        inc     b
        rra
        rra
        jr      nc,.menuControlPressed  ; S pressed
        inc     b
        rra
        rra
        jr      nc,.menuControlPressed  ; F pressed
        ; ignore "T" key presses when video mode timing is locked
        ld      a,(state.timingIsUnlocked)
        or      a
        ret     z
        inc     b
        rr      c
        ret     c
        ; T pressed
.menuControlPressed:
    ; B = key control; order of controls is QRSFT (B=1,2,3,4,5)
        ld      a,(state.modified)
        or      a
        ld      a,b
        jr      nz,.needsAlwaysConfirm  ; file modified -> everything needs confirmation
        cp      3
        ; quit and reload will happen instantly when nothing is modified
        jr      c,.YwasPressed
.needsAlwaysConfirm:
        ld      (state.lastCtrlKey),a
        ;  |
        ; fallthrough to .setDebounce
        ;  |
.setDebounce:
        ld      a,KEY_DEBOUNCE_WAIT
        ld      (state.debounceKey),a   ; set debounce wait before reading keyboard again
        ret

.cornerSelector:
        ld      a,e
        xor     1
        ld      (state.corner),a
        jr      .setDebounce

.cornerAdjusting:               ; A = keys 6785 (arrows: v^><)
        ; edges are LRTB ; corner 0 will adjust L+T (--,++), corner 1 will adjust R+B (++,--)
        push    ix
        pop     hl              ; address of left edge (for corner 0)
        dec     e
        jr      nz,.cornerZero
        inc     hl              ; address of right edge (for corner 1)
        cpl                     ; invert keys
.cornerZero:
        ; adjust horizontal and then vertical edge
        call    AdjustOneEdge   ; adjust horizontal edge
        call    AdjustOneEdge   ; adjust vertical edge
        call    SanitizeCurMarginValues
        call    RedrawEdge
        jr      .setDebounce

.ConfirmationPromptHandler:
        ld      b,a     ; which control is waiting for confirm
    ; wait for any key press
        xor     a       ; all rows of keys
        in      a,(254)
        cpl             ; pressed key "0" -> "1"
        and     $1F
        ret     z
    ; some key was pressed, check if it was "Y"
        call    .setDebounce
        xor     a
        ld      (state.lastCtrlKey),a   ; clears last ctrl (will be handled here)
        ld      a,~(1<<5)
        in      a,(254)
        and     $10     ; key Y
        ret     nz                      ; something else pressed, just redraw UI
.YwasPressed:
        djnz    .notQuitPending
    ; Quit confirmed
        ; do classic ULA CLS (destroys the tile map+font)
        ; (in other modes the result is hopefully acceptable any way)
        ld      hl,$4000
        ld      de,$4001
        ld      bc,32*24*8
        ld      (hl),l
        ldir
        ld      (hl),$38                ; white paper + black ink
        ld      bc,32*24-1
        ldir
        ; restore the machine state (reasonably enough)
        ld      hl,preserved
        ld      b,S_PRESERVE/2
.restoreStateLoop:
        ld      a,(hl)
        inc     hl
        ld      (.nr),a                 ; self-modify `nextreg $nn,a` instruction
        ld      a,(hl)
        inc     hl
.nr=$+2:nextreg $00,a
        djnz    .restoreStateLoop
    ;; return to NextZXOS with "no error"
    ; - CF=0 when exiting (CF=1 A=esx_err, CF=1, A=0, HL="dc" custom error string)
        pop     bc          ; remove return address to main loop (to return to NextZXOS)
        xor     a
        ret

.notQuitPending:
        djnz    .notReloadPending
    ; Reload confirmed
        ld      a,CHAR_DOT_YELLOW       ; mark it yellow while processing
        ld      (LabelQuitAdr + 1*80 - 1),a
        .3 halt
        call    ParseCfgFile
        jr      .setDebounce            ; when called without confirm step, needs this

.notReloadPending:
        djnz    .notSavePending
    ; Save confirmed
        ld      a,CHAR_DOT_YELLOW       ; mark it yellow while processing
        ld      (LabelQuitAdr + 2*80 - 1),a
        .3 halt
        jp      SaveCfgFile

.notSavePending:
        djnz    .notFreqPending
    ; Frequency confirmed
        ld      a,CHAR_DOT_YELLOW       ; mark it yellow while processing
        ld      (LabelQuitAdr + 3*80 - 1),a
        .3 halt
        ; flip the 50/60Hz bit
        NEXTREG2A PERIPHERAL_1_NR_05
        xor     %00'00'0'1'0'0
        nextreg PERIPHERAL_1_NR_05,a
        ret

.notFreqPending:
    ; Timing change confirmed
        ld      a,CHAR_DOT_YELLOW       ; mark it yellow while processing
        ld      (LabelQuitAdr + 4*80 - 1),a
        .3 halt
        NEXTREG2A MACHINE_TYPE_NR_03
        and     $0F
        or      (ix + S_MODE_EDGES.ui.nextMode)
        nextreg MACHINE_TYPE_NR_03,a
        ret

;-------------------------------
saveModeKeyword:
.h_5            DZ      'edge_hdmi_50'
.z4_5           DZ      'edge_zx48_50'
.z1_5           DZ      'edge_zx128_50'
.z3_5           DZ      'edge_zx128p3_50'
.h_6            DZ      'edge_hdmi_60'
.z4_6           DZ      'edge_zx48_60'
.z1_6           DZ      'edge_zx128_60'
.z3_6           DZ      'edge_zx128p3_60'
.p              DZ      'edge_pentagon'

EdgesData:
.hdmi_50    S_MODE_EDGES    {{},{},{ $4000+ 8*80+23, $4000+ 7*80+35, $00, saveModeKeyword.h_5  }}
.z48_50     S_MODE_EDGES    {{},{},{ $4000+12*80+23, $4000+11*80+35, $A0, saveModeKeyword.z4_5 }}
.z128_50    S_MODE_EDGES    {{},{},{ $4000+16*80+23, $4000+15*80+35, $B0, saveModeKeyword.z1_5 }}
.z128p3_50  S_MODE_EDGES    {{},{},{ $4000+20*80+23, $4000+19*80+35, $C0, saveModeKeyword.z3_5 }}
.hdmi_60    S_MODE_EDGES    {{},{},{ $4000+ 8*80+23, $4000+ 7*80+54, $00, saveModeKeyword.h_6  }}
.z48_60     S_MODE_EDGES    {{},{},{ $4000+12*80+23, $4000+11*80+54, $A0, saveModeKeyword.z4_6 }}
.z128_60    S_MODE_EDGES    {{},{},{ $4000+16*80+23, $4000+15*80+54, $B0, saveModeKeyword.z1_6 }}
.z128p3_60  S_MODE_EDGES    {{},{},{ $4000+20*80+23, $4000+19*80+54, $C0, saveModeKeyword.z3_6 }}
.pentagon   S_MODE_EDGES    {{},{},{ $4000+24*80+23, $4000+23*80+35, $90, saveModeKeyword.p    }}

state:      S_STATE     {0, 0, 0}

preserved:  S_PRESERVE

backupFilenameExtension:
        DB      ".bak",0
backupFilenameExtensionSZ: EQU $ - backupFilenameExtension

;-------------------------------
readNextReg2A:
        push    bc
        ld      bc,TBBLUE_REGISTER_SELECT_P_243B
        out     (c),a
        inc     b
        in      a,(c)
        pop     bc
        ret

;-------------------------------
; "runtime" functions are in separate asm file so they can be easily included
; in other projects
    DEFINE USE_TO_READ_NEXT_REG @readNextReg2A

    INCLUDE "displayedge_rt.i.asm"

;-------------------------------
DidVideoModeChange:
; returns ZF=1 when no change happened, ZF=0 when changed
        call    dspedge.DetectMode  ; A = 0..8 current mode number
.oM=$+1:cp      dspedge.MODE_COUNT  ; self-modify storage of previous mode
        ld      (.oM),a             ; reset "old Mode" value
        ret

;-------------------------------
SanitizeCurMarginValues:
        push    ix
        pop     hl
        ; the current margin values must be first four bytes in the structure
        ld      b,4
.sanitizeLoop:
        ld      a,(hl)
        call    dspedge.SanitizeMarginValue
        ; 32..127 will become 31, 128..255 will become 0
        ld      (hl),a
        inc     hl
        djnz    .sanitizeLoop
        ret

;-------------------------------
AutoDetectKempston:             ; BC = Kempston port (ie. B = 0, C = $1F or $37)
        xor     a
.detection:
        in      l,(c)
        or      l
        djnz    .detection      ; keep reading the port about 7.5k T states
        or      a               ; check if stable zero was read for full duration
        ret     nz              ; treat non-zero as "no joystick" (strict test)
        ; (b/c false negative does lot less damage than false positive (random edits))
        ; set up the joystick port to be read for controls too
        ld      a,c
        ld      (AdjustCornersByKempston.p),a
        ret

;-------------------------------
AdjustCornersByKempston:
.p=$+1: ld      bc,0            ; detected port number (0, $1F or $37)
        inc     c
        dec     c
        ret     z               ; I/O port is zero, don't read it
        in      a,(c)
        ret     z               ; no input from joystick, just continue with keyboard
        ld      hl,HandleControls.cornerSelector    ; fire1 button -> switch corners
        bit     4,a
        jr      nz,.pressedFire
        cpl                     ; invert direction controls
        ld      hl,HandleControls.cornerAdjusting   ; handle kempston inputs as cursor keys
.pressedFire:
        ex      (sp),hl
        ret

;-------------------------------
AdjustOneEdge:
        rra
        jr      nc,.noDec
        dec     (hl)
.noDec:
        rra
        jr      nc,.noInc
        inc     (hl)
.noInc:
        inc     hl              ; and adjust edge address +2
        inc     hl
        ret

;-------------------------------
ParseCfgFile:
    ; reset save-error number to stop displaying any dangling error message
        ld      a,1
        ld      (state.esxErrorNo),a
    ; call the Cfg parser from runtime library
        ld      hl,CFG_FILENAME
        ld      de,ReadMarginsArray
        ld      bc,ParsingBuffer
        push    ix
        call    dspedge.ParseCfgFile
        pop     ix
        sbc     a                       ; A = 0 when ok, $FF when error reported (+keeps CF)
        ld      (state.noFileFound),a
        ret     c                       ; parsing failed, don't do anything more
    ; reset internal editor structures
        ld      hl,ReadMarginsArray
        ld      de,EdgesData
        ld      bc,(dspedge.MODE_COUNT<<8) + $FF
.copyParsedDataToEditStructs:
        .4 ldi                          ; current values
        add     hl,-4
        .4 ldi                          ; original values (copy of the same)
        add     de,S_MODE_EDGES - S_MODE_EDGES.ui   ; advance to next mode data
        djnz    .copyParsedDataToEditStructs
    ; force full screen redraw the hard way (by pretending the video mode did change)
        ld      a,dspedge.MODE_COUNT
        ld      (DidVideoModeChange.oM),a
        ret

;-------------------------------
SaveCfgFile:
        push    ix
        call    .internal
        ;Fc=1 - error in writing file
        jr      c,.esxErrorHappened
        ; reset "original" values
        ld      a,dspedge.MODE_COUNT
        ld      hl,EdgesData+S_MODE_EDGES.cur
        ld      de,EdgesData+S_MODE_EDGES.orig
.resetOrigDataLoop:
        ld      bc,S_MARGINS
        ldir
        add     hl,S_MODE_EDGES-S_MARGINS
        add     de,S_MODE_EDGES-S_MARGINS
        dec     a
        jr      nz,.resetOrigDataLoop
        ; reset esxErrorNo
        ld      a,1                     ; 1 == esx_eok
.esxErrorHappened:
        ld      (state.esxErrorNo),a    ; remember esxdos error number
        ; force full refresh (by faking mode change)
        ld      a,dspedge.MODE_COUNT
        ld      (DidVideoModeChange.oM),a
        pop     ix
        ret

.internal:
    ; init all internals first
        ld      a,$FF
        ld      (dspedge.ParseCfgFile.Fhandle),a    ; invalid file handle for read file
        ld      (dspedge.ParseCfgFile.oldSP),sp
    ; mark all modes which require save - reusing/merging into "modified" flag
        ld      hl,EdgesData + S_MODE_EDGES.orig.L
        ld      b,dspedge.MODE_COUNT
.setToSaveFlagLoop:
        ; check if original values are != 255 (were read from file) -> then mark as "modified"
        ld a,(hl) : inc hl : and (hl) : inc hl : and (hl) : inc hl : and (hl)
        ; A == orig.L & orig.R & orig.T & orig.B, A == 255 iff all of them are 255
        inc     a                       ; all 255 -> 0 (don't mark it, wasn't in file)
        add     hl,S_MODE_EDGES.modified - S_MODE_EDGES.orig.B
        or      (hl)            ; modified ; mix with real "modified" value
        ld      (hl),a
        add     hl,S_MODE_EDGES - S_MODE_EDGES.modified + S_MODE_EDGES.orig.L ; next mode
        djnz    .setToSaveFlagLoop
    ; if old CFG file exists, backup it, then open it for reading
        ld      a,(state.noFileFound)   ; $FF when file was not found
        rra
        jr      c,.skipRenameAndOpen
        ; create backup filename from original
        ld      de,BackupFilename
        ld      hl,CFG_FILENAME
        ld      bc,124<<8|$FF           ; B = max chars to copy from original filename (C=junk food for LDI)
.copyFilenameLoop:
        ldi
        ld      a,(hl)
        or      a
        jr      z,.EndOfFilenameFound
        djnz    .copyFilenameLoop
.EndOfFilenameFound:                    ; file is copied (either as whole or truncated to max chars
        ; add ".bak" to original name
        ld      hl,backupFilenameExtension
        ld      bc,backupFilenameExtensionSZ
        ldir
        ; delete old backup first (if it exists)
        ld      a,'$'
        ld      hl,BackupFilename
        ESXDOS  F_UNLINK                ; don't even check for the error here
        ; rename the current CFG file to backup file
        ld      a,'$'
        ld      hl,CFG_FILENAME
        ld      de,BackupFilename
        ESXDOS  F_RENAME
        jr      c,.skipRenameAndOpen    ; in case renaming of old file fails, try at least save
        ; fopen the backup file for reading the original data
        ld      a,'$'
        ld      hl,BackupFilename
        ld      b,$01           ; read-only
        ESXDOS  F_OPEN
        jr      c,.skipRenameAndOpen    ; in case old file fopen fails, try at least save
        ld      (dspedge.ParseCfgFile.Fhandle),a
        ; read initial 256B of buffer
        ld      hl,ParsingBuffer+$80
        call    dspedge.ParseCfgFile.readBuffer ; BTW does preserve carry flag for `rl c` below
        ld      l,b
        call    dspedge.ParseCfgFile.readBuffer ; BTW does preserve carry flag for `rl c` below
.skipRenameAndOpen:
        rl      c               ; remember Fc in C
    ; open new cfg file for write
        push    bc
        ld      a,'$'
        ld      hl,CFG_FILENAME
        ld      b,$02+$0C       ; +write, +create_or_truncate
        ESXDOS  F_OPEN
        pop     bc
        ret     c               ; error opening file for write, just exit
        ld      (.Whandle),a    ; write-file handle
        ld      de,WritingBuffer
        rr      c
        jr      nc,.ParseOldFile
        call    .writeNewFileComments   ; add few initial comments to brand new file
        jr      .oldFileClosed      ; read file is not open
    ; parse the old file, copy unknown content to new file (comments, etc?), override data
.ParseOldFile:
        ld      hl,ParsingBuffer
        ld      b,l
        ld      c,l                 ; BC=0 (not in comment mode, B=0 as constant)
.ParseOldFileLoop:
        push    bc
        push    de
        ; when inside comment, skip keyword checking
        rlc     c
        jr      nz,.noKeywordYet
        ; not in comment, check if the mode keyword is in file
        call    dspedge.matchKeyword ; ZF=1 no match, ZF=0 match, HL points after, A=0..8 match number
        jr      z,.noKeywordYet
    ;   if keyword is detected (doesn't even check for "=", word boundary is enough)
        ; (b/c I can't easily rewind old file if "=" is missing, so just ignoring...)
        ; calculate address of mode structure
        ld      e,a
        ld      d,S_MODE_EDGES
        mul     de
        add     de,EdgesData
        ld      ix,de               ; fake IX = mode structure
        pop     de                  ; DE = write buffer again
        push    hl                  ; preserve parsing buffer pointer
        push    ModifiedByTxt       ; tail comment "; modified by..." (contains EOL!)
        call    SaveModeData
        ; dismiss rest of line
        pop     hl                  ; HL = ParsingBuffer (points just after the keyword)
.dismissLineLoop:
        call    dspedge.ParseCfgFile.getCh
        or      a
        jr      z,.continueAfterKeyword
        cp      10
        jr      z,.dismissEolFound
        cp      13
        jr      nz,.dismissLineLoop
.dismissEolFound:
        ; dismiss any further EOL char (eats whole CRLF, but also empty lines)
        ld      a,(hl)              ; peekChar
        cp      10
        jr      z,.dismissLineLoop
        cp      13
        jr      z,.dismissLineLoop
        pop     bc
        jr      .ParseOldFileLoop   ; try next line (may start with keyword)
.noKeywordYet:
    ; copy anything per char. if ";" is found, switch to Eol comment mode (no more keyword check)
        call    dspedge.ParseCfgFile.getCh
        pop     de
.continueAfterKeyword:
        pop     bc
        or      a
        jr      z,.OldFileEof
        push    bc
        call    writeCh
        pop     bc
        cp      10
        jr      z,.eolFound
        cp      13
        jr      z,.eolFound
        cp      ';'
        jr      nz,.ParseOldFileLoop
        ld      c,h                 ; enter comment mode
        jr      .ParseOldFileLoop
.eolFound:
        ld      c,b                 ; leave comment mode
        jr      .ParseOldFileLoop
.OldFileEof:
        ; fclose old-file
        ld      a,(dspedge.ParseCfgFile.Fhandle)
        rst     $08 : DB F_CLOSE
.oldFileClosed:
    ; foreach mode filter !saved -> save mode on new line
        ; check if there is already newline ahead
        dec     e
        ld      a,(de)
        inc     e
        cp      10
        jr      z,.hasEolAfterOldFile
        ld      a,13                ; EOL "\r\n" to make mode data start at new line
        call    writeCh
        ld      a,10
        call    writeCh
.hasEolAfterOldFile:
        ld      b,dspedge.MODE_COUNT
        ld      ix,EdgesData
.writeRemainingModesLoop:
        push    bc
        push    ix
        push    ByTxt
        call    SaveModeData
        pop     ix
        ld      bc,S_MODE_EDGES
        add     ix,bc
        pop     bc
        djnz    .writeRemainingModesLoop
    ; dump remaining WritingBuffer
        xor     a
        cp      e
        jr      z,.emptyWritingBuffer
        ld      b,a
        ld      c,e
        ld      de,WritingBuffer
        call    writeCh.BcBytes
.emptyWritingBuffer:
    ; fclose write file
.Whandle=$+1:   ld      a,$FF       ; self-modify storage for write-handle
        rst     $08 : DB F_CLOSE
        ret

.writeNewFileComments:
        ld      hl,NewFileCommentsTxt
        jr      putS

writeCh:
    ; store character in A into WritingBuffer
        ld      (de),a
        inc     e
        ret     nz
    ; WritingBuffer is full, dump it into the file
        ld      bc,$100
.BcBytes:
        push    af                  ; preserve the A, BC, DE
        push    de
        push    hl
        ex      de,hl               ; HL = WritingBuffer
        ld      a,(SaveCfgFile.Whandle)
        ESXDOS  F_WRITE
        jp      c,dspedge.ParseCfgFile.esxError
        pop     hl
        pop     de
        pop     af
        ret

putS:
        ld      a,(hl)
        inc     hl
        or      a
        ret     z
        call    writeCh
        jr      putS

printfFourEdgeValuesToWB2:
        push    de
        ld      de,WritingBuffer2
        push    de
        call    .printfTwoValuesAndCommas
        call    .printfTwoValuesAndCommas
        ex      de,hl
        dec     hl          ; remove last comma
        ld      (hl),0
        pop     hl
        pop     de
        ret
.printfTwoValuesAndCommas:
        call    .printfValueAndComma
.printfValueAndComma:
        ld      a,(hl)
        inc     hl
        call    DrawAdecAtDe
        ld      a,','
        ld      (de),a
        inc     de
        ret

SaveModeData:
        ; check if it was already saved
        ld      a,(ix+S_MODE_EDGES.modified)
        or      a
        jr      z,.alreadySaved
        ; if not, save it now
        ld      (ix+S_MODE_EDGES.modified),0    ; clear modified flag
        ld      hl,(ix+S_MODE_EDGES.ui.keyword) ; fake HL = keyword pointer
        push    ix
        call    putS
        ld      a,'='
        call    writeCh
        ; store values as well
        pop     hl                  ; mode struct, at +0 offset are the L,R,T,B to save
        ASSERT S_MODE_EDGES.cur.L == 0
        call    printfFourEdgeValuesToWB2       ; HL = WritingBuffer2
        call    putS
        ; write the tail comment
        pop     hl
        ex      (sp),hl
        jr      putS
.alreadySaved:
        ; release the tail-comment from stack
        pop     hl
        ex      (sp),hl
        ret

ModifiedByTxt:
        DB      " ; modified"
ByTxt:
        DB      " ; by .displayedge", 13, 10, 0

NewFileCommentsTxt:
        DB      "; Visible display edge config", 13, 10
        DB      "; by .displayedge, recognized", 13, 10
        DB      "; var names (lowercase!):", 13, 10
        DB      "; edge_hdmi, edge_zx48,", 13, 10
        DB      "; edge_zx128, edge_zx128p3", 13, 10
        DB      "; with _50 / _60 suffix for Hz", 13, 10
        DB      "; edge_pentagon", 13, 10
        DB      "; Values: left,right,top,bottom", 13, 10
        DB      13, 10, 0

;-------------------------------
CreateHotKeyCapitals:
        ld      de,TILE_GFX_ADR+32*CHAR_HOT_F
        ld      hl,TILE_GFX_ADR+32*'F'
        ld      bc,$2066    ; B = 32, C = $66 (green:green)
        call    .copyAndTintLoop
        ld      b,$20*4     ; do four chars: Q, R, S, T
        ld      hl,TILE_GFX_ADR+32*'Q'
        call    .copyAndTintLoop
        ld      b,$20
        ld      hl,TILE_GFX_ADR+32*'Y'
        ;  |
        ; fallthrough to .copyAndTintLoop
        ;  |
.copyAndTintLoop:
        ld      a,(hl)
        or      c
        ld      (de),a
        inc     hl
        inc     de
        djnz    .copyAndTintLoop
        ret

;-------------------------------
RedrawUiControls:
    ; refresh the global "modified" flag based on modified flags of all modes
        ld      hl,EdgesData + S_MODE_EDGES.modified
        ld      b,dspedge.MODE_COUNT
        xor     a
.refreshModified:
        or      (hl)
        add     hl,S_MODE_EDGES
        djnz    .refreshModified
        ld      (state.modified),a
    ; update file status flag
        ld      hl,FileStatusModTxt
        or      a
        jr      nz,.drawFileStatus
        ld      hl,FileStatusNewTxt
        ld      a,(state.noFileFound)
        or      a
        jr      nz,.drawFileStatus
        ld      hl,FileStatusOkTxt
.drawFileStatus:
        ld      de,FileStatusAdr
        call    DrawHlStringAtDe
    ; add "Timing" menu item, when mode is unlocked
        ld      b,4
        ld      a,(state.timingIsUnlocked)
        or      a
        jr      z,.TimingIsLocked
        ld      hl,TimingLegendTxt
        call    DrawStringWithAddressData
        ld      b,5
.TimingIsLocked:
        ; B = number of main menu items (quit/save/...) (4 or 5, the +1 is timing)
    ; draw the green dots/confirm message depending on the internal status of controls
        ld      hl,LabelQuitAdr-1
        ld      a,(state.lastCtrlKey)
        ld      c,a
        or      a
        ld      a,' '   ; CHAR_DOT_GREEN ;TODO maybe simplify since dots are gone
        ld      de,ConfirmLegendTxtOff  ; zero CtrlKey, hide confirmation prompt
        jr      z,.MainKeysAvailable
        ld      a,' '
        ld      de,ConfirmLegendTxtOn   ; non-zero CtrlKey, requesting confirmation
.MainKeysAvailable:
        ld      (hl),a
        add     hl,80
        djnz    .MainKeysAvailable
        ; display/hide confirmation prompt
        ex      de,hl
        call    DrawStringsWithAddressData
        ; mark the active Ctrl when confirm prompt is displayed
        ld      e,c
        ld      d,80
        dec     e
        jp      m,.noConfirmPrompt
        mul     de
        add     de,LabelQuitAdr-1
        ex      de,hl
        ld      (hl),CHAR_DOT_RED
.noConfirmPrompt:
    ; draw the blinking corner type after "SPACE:" label and into table-cell
        ; erase all positions with space char and set HL to left corner
        ld      a,' '
        ld      hl,(ix+S_MODE_EDGES.ui.cellAdr)     ; fake
        add     hl,2*80+16
        ld      (hl),a
        add     hl,-(2*80+17)
        ld      (hl),a
        ld      (LegendCornerAdr),a
        ; make it blink per frameTicker value
        ld      a,(state.frameTicker)
        and     $30
        jp      z,RedrawAllTableData.RedrawCell     ; keep it blank for short while
        ; draw the corner char at desired places
        ld      de,(state.corner)       ; E = active corner 0/1
        ld      a,CHAR_CORNER_TL
        ASSERT 1+CHAR_CORNER_TL == CHAR_CORNER_BR
        add     a,e                     ; A = char with corner marker
        ld      d,2*80+17
        mul     de
        add     hl,de                   ; address of corner inside the table-cell
        ld      (hl),a                  ; draw the one inside table-cell
        ld      (LegendCornerAdr),a     ; draw the one after SPACE: label
    ; redraw active table item
        jp      RedrawAllTableData.RedrawCell

;-------------------------------
RedrawEdge:
    ;; reset dynamic tiles gfx, chars numbers:
    ; TOP:    CHAR_FILL_TL CHAR_FILL_T CHAR_FILL_TR
    ; MIDDLE: CHAR_FILL_L      ' '     CHAR_FILL_R    **CHAR_FILL_ALL**
    ; BOTTOM: CHAR_FILL_BL CHAR_FILL_B CHAR_FILL_BR

    ; copy fully invisible tile ($16) to all others as base of drawing
        ld      hl,TILE_GFX_ADR + (CHAR_FILL_ALL+1) * 32 - 1    ; end of FILL_ALL char
        ld      de,TILE_GFX_ADR + CHAR_FILL_ALL * 32 - 1  ; copy downward to other CHAR_FILL tiles
        ld      bc,8 * 32
        lddr
    ; calculate bit-masks for left/right/top/bottom sides to make drawing tiles easy
        ; read the masks for all sides from tables (too lazy to calculate them in code)
        ; two masks for each side (first creates green frame, second background fill)

        MACRO LdiTwoValuesFromTableByAnd table?, value?, and_mask?
            ld      hl,table?
            ld      a,value?
            and     and_mask?
            add     hl,a
            ldi
            ldi
        ENDM

        push    ix
        pop     de
        add     de,S_MODE_EDGES.maskLeftG   ; DE = ix+S_MODE_EDGES.maskLeftG
        LdiTwoValuesFromTableByAnd  RedrawTileMasksLeft,    (ix+S_MODE_EDGES.cur.L),    3
        LdiTwoValuesFromTableByAnd  RedrawTileMasksRight,   (ix+S_MODE_EDGES.cur.R),    3
        LdiTwoValuesFromTableByAnd  RedrawTileMasksTop,     (ix+S_MODE_EDGES.cur.T),    7
        LdiTwoValuesFromTableByAnd  RedrawTileMasksBottom,  (ix+S_MODE_EDGES.cur.B),    7

    ; now further patch the actual tiles gfx with these bit-masks which pixel to draw
        ; two masks to cover vertical/horizontal pixels 0 = keep stripes, 1 = draw pixel
        ; horizontal mask is 4b copied twice: %0011'0011 => will draw right 4 pixels, keep 4 left
    ; green filling to create "frame" gfx
        ld      hl,TILE_GFX_ADR + CHAR_FILL_TL * 32
        ld      a,$66           ; draw green full-width pixel (2x1)
        ld      d,(ix+S_MODE_EDGES.maskTopG)
        ld      e,(ix+S_MODE_EDGES.maskLeftG)
        call    RedrawTile
        ld      e,(ix+S_MODE_EDGES.maskRightG)
        call    RedrawTile
        ld      d,$FF           ; all rows
        ld      e,(ix+S_MODE_EDGES.maskLeftG)
        call    RedrawTile
        ld      e,(ix+S_MODE_EDGES.maskRightG)
        call    RedrawTile
        ld      d,(ix+S_MODE_EDGES.maskBottomG)
        ld      e,(ix+S_MODE_EDGES.maskLeftG)
        call    RedrawTile
        ld      e,(ix+S_MODE_EDGES.maskRightG)
        call    RedrawTile
        ld      e,$FF           ; all columns
        ld      d,(ix+S_MODE_EDGES.maskTopG)
        call    RedrawTile
        ld      d,(ix+S_MODE_EDGES.maskBottomG)
        call    RedrawTile
    ; background filling to finalize the "frame" gfx
        ld      hl,TILE_GFX_ADR + CHAR_FILL_TL * 32
        xor     a               ; draw background
        ld      d,(ix+S_MODE_EDGES.maskTopB)
        ld      e,(ix+S_MODE_EDGES.maskLeftB)
        call    RedrawTile
        ld      e,(ix+S_MODE_EDGES.maskRightB)
        call    RedrawTile
        ld      d,$FF
        ld      e,(ix+S_MODE_EDGES.maskLeftB)
        call    RedrawTile
        ld      e,(ix+S_MODE_EDGES.maskRightB)
        call    RedrawTile
        ld      d,(ix+S_MODE_EDGES.maskBottomB)
        ld      e,(ix+S_MODE_EDGES.maskLeftB)
        call    RedrawTile
        ld      e,(ix+S_MODE_EDGES.maskRightB)
        call    RedrawTile
        ld      e,$FF           ; all columns
        ld      d,(ix+S_MODE_EDGES.maskTopB)
        call    RedrawTile
        ld      d,(ix+S_MODE_EDGES.maskBottomB)
        call    RedrawTile

    ;; now redraw the tile map in the border area

    ; calculate left/middle/right tiles numbers for side-rows
        ld      a,(ix+S_MODE_EDGES.cur.L)
        srl     a
        srl     a
        ld      (ix+S_MODE_EDGES.leftT),a       ; pixels/4
        ld      b,a
        ld      a,(ix+S_MODE_EDGES.cur.R)
        srl     a
        srl     a
        ld      (ix+S_MODE_EDGES.rightT),a      ; pixels/4
        add     a,b
        neg
        add     a,80-2
        ld      (ix+S_MODE_EDGES.midT),a        ; middleT = 80 - leftT - rightT - 2
    ; fill the full-tile chars at top and the top edge of green frame
        ld      c,CHAR_FILL_ALL
        ld      hl,$4000
        ld      a,(ix+S_MODE_EDGES.cur.T)
        call    FillFullBorderRows
        ; HL = address to draw top semi-edge
        ld      de,(CHAR_FILL_TL<<8)|CHAR_FILL_T
        call    FillDetailedRow
    ; fill remaining border top rows with clearing rows (drawing space in middle)
.clearingTopRow:
        bit     0,h     ; 3*80 = 240, 4*80 = 320 -> after four lines the H=$41
        jr      nz,.noClearingTopRow
        ld      de,(CHAR_FILL_L<<8)|' '
        call    FillDetailedRow
        jr      .clearingTopRow
.noClearingTopRow:
    ; fill 24 rows clearing sides, but skipping 64 chars in middle (64x24 = PAPER area)
        ld      de,(' '<<8)|24  ; D = space char, E = counter
.drawSideRowsLoop:
        ld      b,(ix+S_MODE_EDGES.leftT)
        call    FillBCharsWithC
        ld      (hl),CHAR_FILL_L
        inc     hl
.clearLeftSide:
        bit     3,l             ; left border ends when +8 is set in address
        jr      nz,.clearLeftSideDone
        ld      (hl),d
        inc     hl
        jr      .clearLeftSide
.clearLeftSideDone:
        add     hl,64           ; skip the middle part
        ld      a,8-1
        sub     (ix+S_MODE_EDGES.rightT)
        jr      z,.clearRightSideDone
.clearRightSide:
        ld      (hl),d
        inc     hl
        dec     a
        jr      nz,.clearRightSide
.clearRightSideDone:
        ld      (hl),CHAR_FILL_R
        inc     hl
        ld      b,(ix+S_MODE_EDGES.rightT)
        call    FillBCharsWithC
        dec     e
        jr      nz,.drawSideRowsLoop
    ; draw the clearing bottom part
        ld      a,23
        sub     (ix+S_MODE_EDGES.cur.B)     ; CF=1 for 24..31 pixels (no clearing row)
        jr      .clearingBottomRowEntry
.clearingBottomRow:
        ld      de,(CHAR_FILL_L<<8)|' '
        call    FillDetailedRow
        sub     8               ; one more full line to clear?
.clearingBottomRowEntry:
        jr      nc,.clearingBottomRow
    ; draw the bottom part of frame, fill remaining bottom and exit
        ld      de,(CHAR_FILL_BL<<8)|CHAR_FILL_B
        call    FillDetailedRow
        ld      a,(ix+S_MODE_EDGES.cur.B)
        ;  |
        ; fallthrough to FillFullBorderRows
        ;  |
FillFullBorderRows:
    ; HL = start address, A = edge pixels, C = CHAR_FILL_ALL
        and     ~7              ; whole full-rows, pre-multiplied by 8
        ld      e,a
        ld      d,80/8
        mul     de              ; whole full-rows * 80 = 0..240 (3*80 = 240) (fits 8b)
        ld      b,e
        ;  |
        ; fallthrough to FillBCharsWithC
        ;  |
FillBCharsWithC:
        ; check if called with B==0
        inc     b
        dec     b
        ret     z
.fillChars:
        ld      (hl),c
        inc     hl
        djnz    .fillChars
        ret

FillDetailedRow:
    ; HL = address, D = left char (+1 right char), E = middle char, C = CHAR_FILL_ALL
        ld      b,(ix+S_MODE_EDGES.leftT)
        call    FillBCharsWithC
        ld      (hl),d
        inc     hl
        ld      c,e
        ld      b,(ix+S_MODE_EDGES.midT)
        call    FillBCharsWithC
        inc     d
        ld      (hl),d
        inc     hl
        ld      c,CHAR_FILL_ALL
        ld      b,(ix+S_MODE_EDGES.rightT)
        jr      FillBCharsWithC

RedrawTile:
    ; A = color to draw (two 4bpp pixels together), HL = tile address
    ; D = vertical mask, E = horizontal mask (0 = keep, 1 = draw) (masks are AND-ed)
    ; horizontal mask is only 4b, but copied twice
        ld      b,8
.RowsLoop:
        rlc     d
        jr      nc,.keepFullRowOfStripes
        DUP     4           ; 4x2 = 8 pixels per row
            rlc     e
            jr      nc,$+2+1    ; skip draw
            ld      (hl),a      ; draw two pixels at time (4bpp)
            inc     hl
        EDUP
        djnz    .RowsLoop
        ret
.keepFullRowOfStripes:
        .4  inc     hl  ; HL += 4 (skip full row)
        djnz    .RowsLoop
        ret

RedrawTileMasksLeft:
        DB      $FF, $77, $33, $11,  $00
RedrawTileMasksRight:
        DB      $FF, $EE, $CC, $88,  $00
RedrawTileMasksTop:
        DB      $FF, $7F, $3F, $1F,  $0F, $07, $03, $01,  $00
RedrawTileMasksBottom:
        DB      $FF, $FE, $FC, $F8,  $F0, $E0, $C0, $80,  $00

;-------------------------------
RedrawMainMap:
        ; clear full map first with space character
        ld      hl,$4000
        ld      de,$4001
        ld      bc,80*32-1
        ld      (hl),' '
        ldir
        ; draw table grid 51x23 ... do the horizontal lines first, position it at [21,4]
        ld      hl,$4000 + 4*80 + 21
        call    DrawTableGridHorizontalLine
        ld      hl,$4000 + 6*80 + 21
        ld      c,6
.tableGridHorizontalLoop:
        call    DrawTableGridHorizontalLine
        add     hl,80-51 + 3*80
        dec     c
        jr      nz,.tableGridHorizontalLoop
        ld      hl,$4000 + 4*80 + 21
        call    DrawTableGridVerticalLine
        ld      hl,$4000 + 4*80 + 21 + 12
        call    DrawTableGridVerticalLine
        ld      hl,$4000 + 4*80 + 21 + 12 + 19
        call    DrawTableGridVerticalLine
        ld      hl,$4000 + 4*80 + 21 + 12 + 19*2
        call    DrawTableGridVerticalLine
    ; draw filename or esxdos error text
        ld      hl,CFG_FILENAME
        ld      a,(state.esxErrorNo)    ; last esxErrorNo
        cp      1
        jr      z,.drawFileName
        ; translate esxErrorNo to some text
        ld      b,1             ; return error message to 32-byte buffer at DE
        ld      de,EsxErrorBuffer
        push    ix
        ESXDOS  M_GETERR
        pop     ix
        ld      hl,EsxErrorBufferWithLabel
        ld      a,CHAR_DOT_RED
        ld      (hl),a
.drawFileName:
        ld      de,FileNameAdr
        call    DrawHlStringAtDe
        ; draw fixed legend text
        ld      hl,FixedLegendText
        ;  |
        ; fallthrough to DrawStringsWithAddressData
        ;  |
DrawStringsWithAddressData:
        call    DrawStringWithAddressData
        jr      nz,DrawStringsWithAddressData
        ret

; HL = address of data: DW adr, DC text (with bit 7 set on last char)
; returns HL after the string, ZF=1 when "adr" was not $4xxx, ZF=0 otherwise
DrawStringWithAddressData:
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        bit     6,d
        ret     z       ; DE was not $4xxx address, exit with ZF=1
DrawHlStringAtDe:
        ld      a,(hl)
        and     $7F
        ld      (de),a
        inc     de
        bit     7,(hl)
        inc     hl
        jr      z,DrawHlStringAtDe
        ret

DrawTableGridHorizontalLine:
        ld      a,'='
        ld      b,51
.loop:
        ld      (hl),a
        inc     hl
        xor     '=' ^ '-'
        djnz    .loop
        ret

DrawTableGridVerticalLine:
        ld      b,23
.loop:
        ld      a,' '
        cp      (hl)
        ld      a,'+'
        jr      nz,.doThePlusChar
        ld      a,'|'
.doThePlusChar:
        ld      (hl),a
        add     hl,80
        djnz    .loop
        ret

DrawAdecAtDe:
    ; DE = target address, A = decimal value 0..99 (or "--" is printed)
        cp      100
        jr      nc,.invalidValue    ; 100..255 shows as "--"
        ex      de,hl
        ld      (hl),' '
        sub     10
        jr      c,.tensDone
        ld      (hl),'0'
.tensLoop:
        inc     (hl)
        sub     10
        jr      nc,.tensLoop
.tensDone:
        inc     hl
        add     a,10+'0'
        ld      (hl),a
        inc     hl
        ex      de,hl
        ret
.invalidValue:
        ld      a,'-'
        ld      (de),a
        inc     de
        ld      (de),a
        inc     de
        ret

;-------------------------------
; redraw all table cells
RedrawAllTableData:
        push    ix      ; preserve the active mode pointer
        ld      ix,EdgesData
        ld      b,dspedge.MODE_COUNT
.loopModes:
        push    bc
        call    .RedrawCell
        ld      bc,S_MODE_EDGES
        add     ix,bc
        pop     bc
        djnz    .loopModes
        pop     ix
        ret
.drawAdecAtDeAndRememberChange:
        jr      z,DrawAdecAtDe
        ld      b,1
        ld      (ix+S_MODE_EDGES.modified),b
        jr      DrawAdecAtDe
.RedrawCell:
        ld      (ix+S_MODE_EDGES.modified),0
        ld      b,2
        ld      de,(ix+S_MODE_EDGES.ui.cellAdr)     ; fake
        add     de,7
        ld      a,(ix+S_MODE_EDGES.cur.T)
        cp      (ix+S_MODE_EDGES.orig.T)
        call    .drawAdecAtDeAndRememberChange
        add     de,80-2-7
        ld      a,(ix+S_MODE_EDGES.cur.L)
        cp      (ix+S_MODE_EDGES.orig.L)
        call    .drawAdecAtDeAndRememberChange
        add     de,-2+14
        ld      a,(ix+S_MODE_EDGES.cur.R)
        cp      (ix+S_MODE_EDGES.orig.R)
        call    .drawAdecAtDeAndRememberChange
        add     de,80-2-14+7
        ld      a,(ix+S_MODE_EDGES.cur.B)
        cp      (ix+S_MODE_EDGES.orig.B)
        call    .drawAdecAtDeAndRememberChange
        ; if no value is modified, draw either empty spaces (clear) or "not in cfg"
        add     de,-80-2-7+3
        ld      hl,NotInCfgTxt
        ld      a,(ix+S_MODE_EDGES.cur.L)
        inc     a
        jr      z,.notInCfg
        ld      hl,ClearModeStatusTxt
.notInCfg:
        djnz    .notModified
        ; if any value differs, draw "*modified*" message
        ld      hl,ModeModifiedTxt
.notModified:
        jp      DrawHlStringAtDe

;-------------------------------
; UI data

Label50HzAdr:   EQU     $4000 + 5*80 + 21 + 20
Label60HzAdr:   EQU     $4000 + 5*80 + 21 + 39
LabelQuitAdr:   EQU     $4000 + 15*80 + 10
LegendCornerAdr:EQU     $4000 + 9*80 + 16
FileStatusAdr:  EQU     $4000 + 26*80 + 19-4
FileNameAdr:    EQU     $4000 + 27*80 + 9

FixedLegendText:
        DW      Label50HzAdr
        DC      "50 Hz"
        DW      Label60HzAdr
        DC      "60 Hz"
        DW      $4000 + 8*80 + 24
        DC      "HDMI"
        DW      $4000 + 12*80 + 24
        DC      "ZX48"
        DW      $4000 + 16*80 + 24
        DC      "ZX128"
        DW      $4000 + 20*80 + 24
        DC      "ZX128+3"
        DW      $4000 + 24*80 + 24
        DC      "Pentagon"
        DW      $4000 + 4*80 + 14
        DB      "Make ",CHAR_DOT_GREEN,"green",CHAR_DOT_GREEN
        DC      "       "
        DW      $4000 + 5*80 + 14
        DC      "edge visible"
        DW      $4000 + 7*80 + 10
        DC      "controls:"
        DW      $4000 + 9*80 + 9
        DC      "SPACE:"
        DW      $4000 + 11*80 + 10
        DC      CHAR_ARROW_T,"   edit"
        DW      $4000 + 12*80 + 9
        DC      CHAR_ARROW_L,CHAR_ARROW_B,CHAR_ARROW_R," corner"
        DW      LabelQuitAdr
        DC      CHAR_HOT_Q,"uit"
        DW      $4000 + 16*80 + 10
        DC      CHAR_HOT_R,"eload"
        DW      $4000 + 17*80 + 10
        DC      CHAR_HOT_S,"ave"
        DW      $4000 + 18*80 + 10
        DC      CHAR_HOT_F," 50/60Hz"
        DW      $4000 + 26*80 + 9
        DC      "file ["
        DW      $4000 + 26*80 + 19
        DC      "]"
        DW      0

TimingLegendTxt:
        DW      $4000 + 19*80 + 10
        DC      CHAR_HOT_T,"iming"
ConfirmLegendTxtOn:
        DW      $4000 + 21*80 + 10
        DB      "press ",CHAR_DOT_RED,CHAR_HOT_Y,CHAR_DOT_RED|128
        DW      $4000 + 22*80 + 10
        DC      "to confirm"
        DW      0
ConfirmLegendTxtOff:
        DW      $4000 + 21*80 + 10
        DC      "         "
        DW      $4000 + 22*80 + 10
        DC      "          "
        DW      0
LockedTxt:
        DC      "(locked)"

; mode status texts (inside table cells)
NotInCfgTxt:
        DC      "not in cfg"
ClearModeStatusTxt:
        DC      "          "
ModeModifiedTxt:
        DB      CHAR_DOT_YELLOW, "modified", CHAR_DOT_YELLOW|128

; file status texts: "*new","*mod","*ok "  (yellow dot for new+mod, green for ok)
FileStatusNewTxt:
        DC      CHAR_DOT_YELLOW,"new"
FileStatusModTxt:
        DC      CHAR_DOT_YELLOW,"mod"
FileStatusOkTxt:
        DC      CHAR_DOT_GREEN,"ok "

;-------------------------------
; PALETTE data for tilemode (full 9bit colors)
tilemapPalette:
                db  %101'101'11,0       ; 0 white-blueish (paper)
                db  %100'100'10,1       ; 1 light grey (25% ink)
                db  %010'010'01,1       ; 2 dark grey (75% ink)
                db  %000'000'00,0       ; 3 black (full ink)
                db  %110'001'00,1       ; 4 red
                db  %111'110'00,1       ; 5 yellow
                db  %000'100'00,0       ; 6 green
                db  %000'000'00,0       ; 7 black (copy)
tilemapPalette_SZ:  EQU $ - tilemapPalette

EsxErrorBufferWithLabel:
        DB      CHAR_DOT_RED, "Save failed: "
    ; the "tilemapFont_char22" address should follow, reusing font area for esxdos error text

;-------------------------------
; FONT data for tilemode (32B per char, almost 4kiB of data)
; the special characters are from 8x6 mode font, characters are from Topan font

        ; starts at character 32 - 4 dir_arrows - 3 color dots - 2 corners - 1 filler = 22
tilemapFont_char22:
tilemapFont:    EQU     tilemapFont_char22 - CHAR_FILL_ALL*32  ; 22 chars skipped

    ; includes full 8x6 font first, reserving space (Topan will be written over later)
    OPT push listoff
        INCLUDE "tilemap_font_8x6.i.asm"
;         INCBIN "ned_gfx_font.bin"
    OPT pop

last:       ; after last machine code byte which should be part of the binary
    ENDT        ;; end of DISP

    ; overwrite font with topan.fnt from Odin project
    ; converting it from 1bpp to 4bpp at assembly time
topan_1bpp:
    INCBIN "topan.fnt"      ; source 1bpp data (outside of DOT command binary)
    ; move '=' 1 pixel up
    ORG topan_1bpp+'='*8 : .7 DB {b $+1 }
    ; move '|' 1 pixel right
    ORG topan_1bpp+'|'*8 : .8 DB {b $ } >> 1
    ; convert 1bpp to 4bpp data and overwrite the 8x6 font in dot command area
    ORG topan_1bpp-(128-' ')*32
    OPT push listoff
topan_rptr=topan_1bpp+' '*8
        DUP (128-' ')*8
topan_pixels={b topan_rptr}
topan_rptr=topan_rptr+1
topan_pxmask=$80
            DUP 4
                DB ((!!(topan_pixels&topan_pxmask))&$30) | ((!!(topan_pixels&(topan_pxmask>>1)))&$03)
topan_pxmask=topan_pxmask>>2
            EDUP
        EDUP
    OPT pop

diagBinSz   EQU     last-start
diagBinPcHi EQU     (100*diagBinSz)/8192
diagBinPcLo EQU     ((100*diagBinSz)%8192)*10/8192
    DISPLAY "Binary size: ",/D,diagBinSz," (",/D,diagBinPcHi,".",/D,diagBinPcLo,"% of dot command 8kiB)"

    IFNDEF TESTING
        SAVEBIN "DISPLAYEDGE",start,last-start
    ELSE
testStart
        ; inject "jp testStart" at $8000 for easy re-run from BASIC (if not overwritten)
        ld      a,$C3
        ld      (ORG_ADDRESS-3),a
        ld      hl,testStart
        ld      (ORG_ADDRESS-2),hl
        ; move the code into 0x2000..3FFF area, faking dot command environment
        nextreg MMU1_2000_NR_51,TEST_CODE_PAGE
        ; copy the machine code into the area
        ld      hl,__bin_b
        ld      de,$2000
        ld      bc,last-start
        ldir
        ; setup fake argument and launch loader
        ld      hl,testFakeArgumentsLine
;         CSP_BREAK
        call    $2000       ; call to test the quit function
        CSP_BREAK
        ret

testFakeArgumentsLine   DZ  " nothing yet ..."

        SAVESNA "DISPLAYEDGE.SNA",testStart

        IFDEF LAUNCH_EMULATOR : IF 0 == __ERRORS__ && 0 == __WARNINGS__
            SHELLEXEC "( sleep 0.1s ; runCSpect -brk DISPLAYEDGE.SNA ) &"
        ENDIF : ENDIF
    ENDIF
