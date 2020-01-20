;-------------------------------
; .DISPLAYEDGE
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
;
; Displays green rectangle in tilemap 640x256x4 mode at the user defined edge and let
; the user further adjust per-pixel which pixels are well visible on his display. The
; final configuration can be then stored in /sys/displayedge.cfg file, to be readable
; by any other application. The config is for full width-pixel only (320x256 res)!
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.14.0+)
; For "testing" snapshot for emulators use option: -DTESTING (or uncomment define below)
;
; (TODO verify paths and if the documented filenames work)
; Reads /sys/displayedge.cfg by default, and allows user to adjust the configuration for
; current video mode (system F3 key to switch between 50/60 Hz => user can define both).
;
; There will be another set of asm source files in https://github.com/ped7g/ZXSpectrumNextMisc
; repository (hopefully it will become part of main distribution over time as well) to
; read the configuration file back and have simple API to detect the current mode and
; read back the user defined values. (TODO)
;
; command line options:
; TODO add cfg filename, and maybe CLI edit mode to write values for particular mode
; TODO without interactive part
;
; Changelist:
; v1    16/01/2020 P7G    Initial version
;
;-------------------------------
; See example of CFG file in the file "test.cfg" in the project git repo. Syntax rules:
;; full comment line starts with semicolon, only ASCII chars allowed, max line length 250
;; otherwise (non-ASCII byte, longer line) parser may break and skip rest of file
;; the recognized video-mode names: "hdmi", "zx48", "zx128", "zx128p3", "pentagon" (lowercase!)
;; the video-mode name is then appended by Hz: "_50", "_60" (pentagon is without Hz part)
;; the value is four decimal integers split by comma/space: left right top bottom of display
;; the values are number of pixels (in 320x256 resolution) not visible to user
;-------------------------------
/* screen layout design
- tilemode 80x32 (640x256x4) without attribute byte = 2560 bytes map
- the rectangle around (invisible part is solid "//" pattern, green frame is 8 dynamically
drawn chars gfx, inner part is "space" char)
- there is arrow pointing to particular edge of screen
 -- O/P to turn arrow counterclockwise/clockwise
 -- J (-)/K (+) to remove/add margin (H/L to remove/add per 8px)
 -- some button to flip 50/60Hz (although F3 will work too)
- near H/J/K/L controls there is current margin in pixels (decimal value)
- at bottom there is filename of cfg file (being edited)
- status of the file (new, no change, edited-needs save), buttons to reload/save
- on right there is table for all modes, emphasing current display mode and selected value

+-=-=-=-=-=-+-=-=-=-=-=--=-=-=-+-=-=-=-=-=--=-=-=-+
|           |       50 Hz      |       60 Hz      |
+-=-=-=-=-=-+-=-=-=-=-=--=-=-=-+-=-=-=-=-=--=-=-=-+
|           |        99        | v    > 99 <      |  Controls:  !
| HDMI      | 99 *modified* 99 | 99 *current*  99 |
| (locked)  |        99        | ^      99        | O > right P ! "< left"/"^ top"/"> right"/"v bot."
+-=-=-=-=-=-+-=-=-=-=-=--=-=-=-+-=-=-=-=-=--=-=-=-+
|           |        99        |        99        |     99      !
| ZX48      | 99 *modified* 99 | 99 *current*  99 | -8 -1 +1 +8 !
|           |        99        |        99        |  H  J  K  L !
+-=-=-=-=-=-+-=-=-=-=-=--=-=-=-+-=-=-=-=-=--=-=-=-+
|           |        99        |        99        | *S*ave      ! green dots before first tap
| ZX128     | 99 *modified* 99 | 99 *current*  99 | *R*eload    ! red dots after first tap (to confirm)
|           |        99        |        99        | *Q*uit      !
+-=-=-=-=-=-+-=-=-=-=-=--=-=-=-+-=-=-=-=-=--=-=-=-+ *T*iming    !
|           |        --        |        99        |
| ZX128+3   | -- not in cfg -- | 99 *current*  99 | press       !
|           |        --        |        99        | S/R/Q/T     !
+-=-=-=-=-=-+-=-=-=-=-=--=-=-=-+-=-=-=-=-=--=-=-=-+ twice to    !
|           |        99        |                  | confirm     !
| pentagon  | 99 *modified* 99 |                  |
|           |        99        |                  |
+-=-=-=-=-=-+-=-=-=-=-=--=-=-=-+-=-=-=-=-=--=-=-=-+ file [*new] !
$/sys/displayedge.cfg

video mode table is 51x23 +1 for filename
screen estate is 64x24 (without the extra +-32px on sides, just regular 256x192)
file status: "*new","*mod","*ok "  (yellow dot for new+mod, green for ok)
asciiart: swap left/right side, the table is on right side, controls on left

*/
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

;     DEFINE  CFG_FILENAME    dspedge.defaultCfgFileName
    DEFINE  CFG_FILENAME    debugCfgName        ;FIXME DEBUG

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
edge                BYTE    0   ; 0 left, 1 top, 2 right, 3 bottom (to align with chars)
lastCtrlKey         BYTE    0   ; save/reload/quit/hz/timing when waiting for confirm
debounceKey         BYTE    0
modified            BYTE    0   ; set if any of modes (even inactive) is modified
noFileFound         BYTE    0
    ENDS

KEY_DEBOUNCE_WAIT   EQU     8

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
M_GETSETDRV                     equ $89     ; get current drive (or use A='*'/'$' for current/system drive!)
M_GETHANDLE                     equ $8D     ; get file handle of current dot command
M_GETERR                        equ $93
F_OPEN                          equ $9A
F_CLOSE                         equ $9B
F_READ                          equ $9D
F_SEEK                          equ $9F
F_FGETPOS                       equ $A0
F_RENAME                        equ $B0
FA_READ                         equ $01

;; helper macros

ESXDOS      MACRO service? : push hl : pop ix : rst $08 : db service? : ENDM    ; copies HL into IX
NEXTREG2A   MACRO nextreg? : ld a,nextreg? : call readNextReg2A : ENDM
CSP_BREAK   MACRO : IFDEF TESTING : break : ENDIF : ENDM

;;-------------------------------
;; Start of the machine code itself
;;-------------------------------

        ORG     ORG_ADDRESS
__bin_b DISP    DISP_ADDRESS

start:
    ;; close the file handle of the dot command itself
        push    hl
        rst $08 : db M_GETHANDLE
        rst $08 : db F_CLOSE
        pop     hl

    ;; parse the arguments on command line (HL = arguments)
        ; no options implemented yet

    ;; page-in the bank5 explicitly (just to be sure it's Bank 5 there)
        nextreg MMU2_4000_NR_52,5*2
        nextreg MMU3_6000_NR_53,5*2+1

    ;; copy the font data to Bank 5 VRAM (= release memory for buffer to parse .cfg file)
        ; clear first 24 characters which are not part of the font
        ld      hl,TILE_GFX_ADR
        ld      de,TILE_GFX_ADR+1
        ld      bc,24*32-1
        ld      (hl),l
        ldir
        ; copy the font data for chars 24..127
        ld      hl,tilemapFont_char24
        ld      bc,(128-24)*32
        ldir

    ;; set Tilemode 80x32 (640x256x4)
        ; enter it in a way to make it possible to restore the original mode completely
        ; i.e. read old_$69 and do $69=0 (layer2 off, bank 5 ULA, no timex mode)
        ; preserve also the $6x tilemap registers and set my tilemap mode
        ; preserve tilemap clip window, reset it to full res
        ; preserve layer priorities, set ula (tiles) on top to be sure
        ; disable ULA pixels? (is it part of tile $6x?)
        ; preserve also transparency[global,tiles], transparency fallback colour

        ;FIXME all the preservations

        ; set up the tilemode and machine state
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
    ; FIXME all
        ; when "save" is requested, parse the old cfg file and overwrite/add new data:
        ; - probably rename old to backup, open for read, open for write new cfg, copy
        ; - all comment lines, write modified lines instead of old values where needed
        ; - add new modes after the other block, close the files
        ; - (delete old backup before first step)

MainLoop:
    ; check if whole video mode did change
        call    DidVideoModeChange
        jr      z,.noModeChange
    ; calculate EdgesData address for new mode
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
        ld      hl,(ix+S_MODE_EDGES.ui.cellAdr)     ; fake
        add     hl,2*80+16
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
    ; check if the confirmation prompt is being displayed, check for Y/other key then
        ld      a,(state.lastCtrlKey)
        or      a
        jr      nz,.ConfirmationPromptHandler
    ; check for regular control keys
        ; O,P select edge to edit (CCWS/CWS)
        ld      a,~(1<<5)           ; sixth row YUIOP
        in      a,(254)
        cpl
        and     3
        jr      nz,.edgeSelector    ; A=%10 for "O", %01 for "P" (or %11 for both)
        ; HJKL to adjust edge size
        ld      a,~(1<<6)           ; seventh row HJKLenter
        in      a,(254)
        cpl
        and     $1E
        jr      nz,.edgeAdjusting
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

.edgeSelector:
        and     2       ; 2 for O/O+P, 0 for P
        inc     a       ; 3 for O/O+P, 1 for P (technically 3 is like -1)
        ld      hl,state.edge
        add     a,(hl)
        and     3
        ld      (hl),a
        jr      .setDebounce

.edgeAdjusting:
        ld      de,EdgeAdjustConstantsTable-2
.findAdjustConstant:
        inc     de
        rra
        jr      nc,.findAdjustConstant  ; H J K L enter (i.e. L is de+2, J is de+4, etc)
        call    CurrentEdgeValueAdrToHl
        ld      a,(de)                  ; adjustement constant (-8, -1, +1, +8)
        add     a,(hl)
        ld      (hl),a
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
/*
        ; when "quit" is requested, restore tilemap mode to previous values and do
        ; classic ULA CLS (shouldn't hurt even if the user was in different mode)
        ; and return.
*/
        ;FIXME all
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
        ;FIXME all
        ret

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
EdgeAdjustConstantsTable:
        DB      +8, +1, -1, -8

EdgesData:
.hdmi_50    S_MODE_EDGES    {{},{},{ $4000+ 8*80+23, $4000+ 7*80+35, $00 }}
.z48_50     S_MODE_EDGES    {{},{},{ $4000+12*80+23, $4000+11*80+35, $A0 }}
.z128_50    S_MODE_EDGES    {{},{},{ $4000+16*80+23, $4000+15*80+35, $B0 }}
.z128p3_50  S_MODE_EDGES    {{},{},{ $4000+20*80+23, $4000+19*80+35, $C0 }}
.hdmi_60    S_MODE_EDGES    {{},{},{ $4000+ 8*80+23, $4000+ 7*80+54, $00 }}
.z48_60     S_MODE_EDGES    {{},{},{ $4000+12*80+23, $4000+11*80+54, $A0 }}
.z128_60    S_MODE_EDGES    {{},{},{ $4000+16*80+23, $4000+15*80+54, $B0 }}
.z128p3_60  S_MODE_EDGES    {{},{},{ $4000+20*80+23, $4000+19*80+54, $C0 }}
.pentagon   S_MODE_EDGES    {{},{},{ $4000+24*80+23, $4000+23*80+35, $90 }}

state:      S_STATE     {0, 0, 0}

debugCfgName:
        DZ      "test.cfg"
        DB      32|128          ; bit7 terminated for UI of .displayedge tool

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
.oM=$+1 cp      dspedge.MODE_COUNT
        ret     z
        ld      (.oM),a
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
CurrentEdgeValueAdrToHl:
        push    af
        ld      a,(state.edge)  ; 0 1 2 3 -> 0 2 1 3 (from edge direction to edge offset)
        add     a,$7E           ; move b1 to b7
        rlca
        and     3               ; result = b0 <-> b1 swap positions
        push    ix
        pop     hl
        add     hl,a            ; address of current value in edge
        pop     af
        ret

;-------------------------------
ParseCfgFile:
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
        ld      a,CHAR_DOT_GREEN
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
    ; draw the edge type between "O ... P" controls
        ld      a,(state.edge)
        swapnib
        srl     a               ;   A = edge*8
        ld      hl,EdgeLegendTxt
        add     hl,a
        ld      de,LabelEdgeAdr
        call    DrawHlStringAtDe
    ; draw the current edge value above "HJKL" controls
        call    CurrentEdgeValueAdrToHl
        ld      a,(hl)
        ld      de,EdgeValueAdr
        call    DrawAdecAtDe
    ; redraw active table item
        jp      RedrawAllTableData.RedrawCell

;-------------------------------
RedrawEdge:
    ;; reset dynamic tiles gfx, chars numbers:
    ; TOP:      $10 $16 $11
    ; MIDDLE:   $12 ' ' $13  **$18** (full)
    ; BOTTOM:   $14 $17 $15

    ; copy fully invisible tile ($18) to all others as base of drawing
        ld      hl,TILE_GFX_ADR + $19 * 32 - 1  ; $18 as source of stripe data (full tile)
        ld      de,TILE_GFX_ADR + $18 * 32 - 1  ; copy downward to $17 .. $10 tiles
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
        ld      hl,TILE_GFX_ADR + $10 * 32
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
        ld      hl,TILE_GFX_ADR + $10 * 32
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
        ld      c,$18           ; full tile char
        ld      hl,$4000
        ld      a,(ix+S_MODE_EDGES.cur.T)
        call    FillFullBorderRows
        ; HL = address to draw top semi-edge
        ld      de,$1016
        call    FillDetailedRow
    ; fill remaining border top rows with clearing rows (drawing space in middle)
.clearingTopRow:
        bit     0,h     ; 3*80 = 240, 4*80 = 320 -> after four lines the H=$41
        jr      nz,.noClearingTopRow
        ld      de,$1220        ; E=' '
        call    FillDetailedRow
        jr      .clearingTopRow
.noClearingTopRow:
    ; fill 24 rows clearing sides, but skipping 64 chars in middle (64x24 = PAPER area)
        ld      de,(' '<<8)|24  ; D = space char, E = counter
.drawSideRowsLoop:
        ld      b,(ix+S_MODE_EDGES.leftT)
        call    FillBCharsWithC
        ld      (hl),$12
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
        ld      (hl),$13
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
        ld      de,$1220        ; E=' '
        call    FillDetailedRow
        sub     8               ; one more full line to clear?
.clearingBottomRowEntry:
        jr      nc,.clearingBottomRow
    ; draw the bottom part of frame, fill remaining bottom and exit
        ld      de,$1417
        call    FillDetailedRow
        ld      a,(ix+S_MODE_EDGES.cur.B)
        ;  |
        ; fallthrough to FillFullBorderRows
        ;  |
FillFullBorderRows:
    ; HL = start address, A = edge pixels, C = tile char ($18)
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
    ; HL = address, D = left char (+1 right char), E = middle char, C = $18 (tile char)
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
        ld      c,$18
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
        ; draw filename
        ld      hl,CFG_FILENAME
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
LabelEdgeAdr:   EQU     $4000 + 9*80 + 11
EdgeValueAdr:   EQU     $4000 + 11*80 + 14
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
        DC      "O"
        DW      $4000 + 9*80 + 19
        DC      "P"
        DW      EdgeValueAdr - 4
        DB      CHAR_ARROW_L|128
        DW      EdgeValueAdr + 5
        DB      CHAR_ARROW_R|128
        DW      $4000 + 12*80 + 9
        DC      "-8 -1 +1 +8"
        DW      $4000 + 13*80 + 10
        DC      "H  J  K  L"
        DW      LabelQuitAdr
        DC      "Quit"
        DW      $4000 + 16*80 + 10
        DC      "Reload"
        DW      $4000 + 17*80 + 10
        DC      "Save"
        DW      $4000 + 18*80 + 10
        DC      "F 50/60Hz"
        DW      $4000 + 26*80 + 9
        DC      "file ["
        DW      $4000 + 26*80 + 19
        DC      "]"
        DW      0

EdgeLegendTxt:
        DC      CHAR_ARROW_L,"left   "
        DC      CHAR_ARROW_T,"top    "
        DC      CHAR_ARROW_R,"right  "
        DC      CHAR_ARROW_B,"bottom "

TimingLegendTxt:
        DW      $4000 + 19*80 + 10
        DC      "Timing"
ConfirmLegendTxtOn:
        DW      $4000 + 21*80 + 10
        DB      "press ",CHAR_DOT_GREEN,"Y"|128
        DW      $4000 + 22*80 + 10
        DC      "to confirm"
        DW      0
ConfirmLegendTxtOff:
        DW      $4000 + 21*80 + 10
        DC      "        "
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
tilemapPalette_SZ:  EQU $ - tilemapPalette

;-------------------------------
; FONT data for tilemode (32B per char, almost 4kiB of data)
; desperate times, desperate measures: this is font designed for copper-8x6 tilemode
; TODO replace with something designed for 8x8 later

tilemapFont:    EQU     tilemapFont_char24 - 24*32
        ; 24 chars skipped (3*256)
        ; starts at character 32 - 4 dir_arrows - 3 color dots - 1 reserve = 24
tilemapFont_char24:
    OPT push listoff
        INCLUDE "tilemap_font_8x6.i.asm"
    OPT pop

; read parsed CFG into array stored where the font originally was
ReadMarginsArray:   EQU     tilemapFont_char24
ReadMarginsArraySZ: EQU     dspedge.S_MARGINS * dspedge.MODE_COUNT
; and use the space also as parsing buffer (at least 513B needed for "runtime" function)
ParsingBuffer:      EQU     ReadMarginsArray + ReadMarginsArraySZ
    ASSERT ParsingBuffer + 513 <= $

;-------------------------------
;; FIXME requires cleanup (everything below)
;-------------------------------
;-------------------------------
;-------------------------------
;-------------------------------

;-------------------------------
;;FIXME just the remnants of custom error message exit

;-------------------------------
fclose: ret     ; this will be modified to NOP after fopen
        ld      a,(handle)
        ESXDOS  F_CLOSE
        ld      a,201
        ld      (fclose),a              ; lock fclose behind `ret` again (nop->ret)
        ret

;-------------------------------
fread:
handle=$+1  ld a,1              ; SMC self-modify code, storage of file handle
        ESXDOS  F_READ
        ret     nc
        ; in case of error just continue into "fileError" routine
fileError:                      ; esxDOS error code arrives in a
        push    af
        and     7
        out     (254),a         ; modify also BORDER by low 3 bits of error code
        pop     af
        ld      b,1             ; return error message to 32-byte buffer at DE
        ld      de,esxError
        ESXDOS  M_GETERR
        ld      hl,esxError
;         jp      customErrorToBasic

;-------------------------------
prepareForErrorOutput           ; do "CLS" of ULA screen with white paper ("error" case)
        nextreg MMU2_4000_NR_52,5*2   ; page-in the bank5 explicitly
        nextreg MMU3_6000_NR_53,5*2+1
        ld      a,7             ; "error" CLS
        jr      clsWithBordercol.withA

;-------------------------------
clsWithBordercol        ; do "CLS" of ULA screen, using the border colour value from header
        ld      a,7
.withA: out     ($FE),a     ; change border colour
        ; bank 5 should be already paged in here (nextregs reset)
        ld      hl,$4000
        ld      de,$4001
        ld      bc,$1800
        ld      (hl),l
        ldir    ; HL=$5800, DE=$5801 (for next block)
        .3 add  a,a         ; *8
        ld      (hl),a
        ld      bc,32*24-1
        ldir
        ret

last:       ; after last machine code byte which should be part of the binary

    ;; reserved space for values (but not initialized, i.e. not part of the binary)
nexFileVersion  db      0       ; BCD-packed ($13 for V1.3)
esxError        ds      34

lastReserved:   ASSERT  lastReserved < $3D00
    ENDT        ;; end of DISP

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
;         CSPECTMAP
    ENDIF
