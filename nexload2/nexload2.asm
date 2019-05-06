;-------------------------------
; .nexload2
; © Peter Helcmanovsky 2019, license: https://opensource.org/licenses/MIT
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.12.0+)
; Use options: --zxnext (--zxnext=cspect for TESTING snapshot)
;
; The NEX file format: http://devnext.referata.com/wiki/NEX_file_format
;
; Differences from current NEXLOAD (© Jim Bagley):
; - some extra checks of NEX file validity
; - does reset all next registers, including new ones from core 2.00.26
; - different code style, using few more features of sjasmplus (in the effort to make
;   the source easier to read, maintain and modify, but that is subjective)
; - slightly different "architecture", saving some memory transfers and memory usage
;   (but the total amount of memory used in the 0x2000-3FFF area is slightly bigger)
; - a bit smaller NEXTLOAD2 binary (~2000B -> ~1500B)
; - the "preserve NextRegs" flag in header causing the machine to retain lot more state
; - different TESTING mechanism for CSpect debugging of the loader code
;
; Included test files (tm**.nex) were created from official distribution tilemap demo,
; which is included in official distribution under MIT license (AFAIK).
;
; Roadmap (things which may eventually happen, if enough effort + acceptance...):
; # documentation
; - add docs for "preserve NextRegs", i.e. what environment can the code expect after load
; # probably compatible with current V1.2 file format
; - checking machine memory and use the RAMREQ to report low memory
; - passing cmd line args to the loaded code (ideally in C-compatible way)
; - having entry bank dynamically allocated by NextZXOS (entrybank = 255?)
; - delays timed by raster line, not interrupt (then maybe preserve+restore DI/EI?)
; # V1.3+
; - checksums incorporated into the NEX file (maybe into header?) - mostly for file archival/transfers, not loader itself
; - custom palettes also for ULA/HiRes/HiCol screens (maybe +64 flag?)
; - tilemap screen
; - return multiple open file handles to the .nex file. Specify how many handles and an address in the header.
; - With a flag (bit 7 of number of handles?) to force close all existing open handles first.
;
; Changelist:
; v2.2  06/05/2019 P7G    set up C to 255 in case there's no file handle (for C projects)
; v2.1  05/05/2019 P7G    fixing bug in Entry-bank setup
; v2    14/04/2019 P7G    fixing bug in palette loader
; v1    04/04/2019 P7G    Rewriting the NEXLOAD from scratch
;
;-------------------------------
    device zxspectrum48
;-------------------------------

;     DEFINE TESTING
    DEFINE ORG_ADDRESS  $2000
    DEFINE DISP_ADDRESS $2000
    DEFINE SP_ADDRESS   $3D00

    IFDEF TESTING
        UNDEFINE ORG_ADDRESS
        DEFINE ORG_ADDRESS      $8003
        DEFINE TEST_CODE_PAGE   223         ; using the last page of 2MiB RAM (in CSpect emulator)
    ENDIF

NEXLOAD_LOADER_VERSION      EQU     $12     ; V1.2 in bcd
NEXLOAD_MAX_FNAME           EQU     128
NEXLOAD_MAX_BANK            EQU     112

NEXLOAD_LOADSCR_LAYER2      EQU     1       ; loads palette by default
NEXLOAD_LOADSCR_ULA         EQU     2       ; can't have palette
NEXLOAD_LOADSCR_LORES       EQU     4       ; loads palette by default
NEXLOAD_LOADSCR_HIRES       EQU     8       ; Timex HiRes (no palette)
NEXLOAD_LOADSCR_HICOL       EQU     16      ; Timex HiCol (no palette)
NEXLOAD_LOADSCR_NOPAL       EQU     128     ; no palette for Layer2/Lores
NEXLOAD_LOADSCR_HASPAL      EQU     NEXLOAD_LOADSCR_LAYER2|NEXLOAD_LOADSCR_LORES

;; NEX header structures

    STRUCT NEXLOAD_FILE_VERSION
                DS      1, 'V'
V_MAJOR         DB      '1'
                DS      1, '.'
V_MINOR         DB      '2'
    ENDS

    STRUCT NEXLOAD_CORE_VERSION
V_MAJOR         DB      2
V_MINOR         DB      0
V_SUBMINOR      DB      0
    ENDS

    STRUCT NEXLOAD_HEADER
                DS      1, 'N'
                DS      1, 'e'
                DS      1, 'x'
                DS      1, 't'
NEXVERSION      NEXLOAD_FILE_VERSION        ; file version (V1.2 is current)
RAMREQ          DB              ; 0 = 768k, 1 = 1792k
NUMBANKS        DB              ; number of 16k banks to load (0..112)
LOADSCR         DB              ; see NEXLOAD_LOADSCR constants (bitmask flags)
BORDERCOL       DB              ; 0-7
SP              DW              ; stack pointer
PC              DW      0       ; entry point (in default MMU 16k mapping FF:5:2:0) (0=no start, only load)
                DS      2, 0    ; "NUMFILES" - obsolete, keep zeroed
BANKS           DS      NEXLOAD_MAX_BANK, 0 ; 112 banks in total = 1.75MiB ; 0/1 false/true array
LOADBAR         DB      0       ; 0/1 show Layer2 progress bar
LOADBARCOL      DB      0       ; colour of progress bar
LOADDELAY       DB      0       ; delay after each bank is loaded (1/50th of sec)
STARTDELAY      DB      0       ; delay after whole file is loaded (before app entry) 1/50th
PRESERVENEXTREG DB      0       ; 0/1: 0=reset NextRegs to almost-defaults, 1=keep them as they are
COREVERSION     NEXLOAD_CORE_VERSION     ; required minimal core version
HIRESCOL        DB      0       ; Timex 512x192 mode colour for port 255 (bits 5-3)
ENTRYBANK       DB      0       ; Bank to page into C000..FFFF area before start of NEX code
FILEHANDLERET   DW      0       ; 0 = close file, 1..$3FFF = BC contains file handle
                                ; $4000+ = file handle is written into memory at this address (after ENTRYBANK is paged in)
RESERVED        DS      512-NEXLOAD_HEADER.RESERVED,0   ; fill up with zeroes to size 512
    ENDS

;; some further constants, mostly machine/API related

LAYER2_BANK                 equ 9

M_GETERR                    equ $93
F_OPEN                      equ $9A
F_CLOSE                     equ $9B
F_READ                      equ $9D
FA_READ                     equ $01

NEXT_VERSION_NR01           equ $01
PERIPHERAL_1_NR05           equ $05     ;Sets joystick mode, video frequency, Scanlines and Scandoubler.
PERIPHERAL_2_NR06           equ $06     ;Enables Acceleration, Lightpen, DivMMC, Multiface, Mouse and AY audio.
TURBO_CONTROL_NR07          equ $07     ;Turbo mode 0=3.5Mhz, 1=7Mhz, 2=14Mhz
PERIPHERAL_3_NR08           equ $08     ;Enables Stereo, Internal Speaker, SpecDrum, Timex Video Modes, Turbo Sound Next and NTSC/PAL selection.
PERIPHERAL_4_NR09           equ $09     ;AY output modes, sprite_id_lock, kempston port, divMMC, scanlines config
CORE_VERSION_NR0E           equ $0E
LAYER2_RAM_NR12             equ $12
SPRITE_CONTROL_NR15         equ $15     ;Enables/disables Sprites and Lores Layer, and chooses priority of sprites and Layer 2.
CLIP_WINDOW_LAYER2_NR18     equ $18
CLIP_WINDOW_SPRITES_NR19    equ $19
CLIP_WINDOW_ULA_NR1A        equ $1A
CLIP_WINDOW_TILEMAP_NR1B    equ $1B
CLIP_WINDOW_CTRL_NR1C       equ $1C
RASTER_INT_CTRL_NR22        equ $22
SOUNDDRIVE_PORT_MIRROR_NR2D equ $2D
TILEMAP_OFS_X_MSB_NR2F      equ $2F
SPRITE_MIRROR_INDEX_NR34    equ $34
PALETTE_INDEX_NR40          equ $40     ;Chooses a ULANext palette number to configure.
PALETTE_VALUE_NR41          equ $41     ;Used to upload 8-bit colors to the ULANext palette.
PALETTE_FORMAT_NR42         equ $42
PALETTE_CONTROL_NR43        equ $43     ;Enables or disables ULANext interpretation of attribute values and toggles active palette.
PALETTE_VALUE_BIT9_NR44     equ $44     ;Holds the additional blue color bit for RGB333 color selection.
TRANSPARENCY_FALLBACK_NR4A  equ $4A
MMU0_NR50                   equ $50     ;Set a Spectrum RAM page at position 0x0000 to 0x1FFF
MMU1_NR51                   equ $51     ;Set a Spectrum RAM page at position 0x2000 to 0x3FFF
MMU2_NR52                   equ $52     ;Set a Spectrum RAM page at position 0x4000 to 0x5FFF
MMU3_NR53                   equ $53     ;Set a Spectrum RAM page at position 0x6000 to 0x7FFF
MMU4_NR54                   equ $54     ;Set a Spectrum RAM page at position 0x8000 to 0x9FFF
MMU5_NR55                   equ $55     ;Set a Spectrum RAM page at position 0xA000 to 0xBFFF
MMU6_NR56                   equ $56     ;Set a Spectrum RAM page at position 0xC000 to 0xDFFF
MMU7_NR57                   equ $57     ;Set a Spectrum RAM page at position 0xE000 to 0xFFFF
COPPER_CTRL_LO_BYTE_NR61    equ $61
COPPER_CTRL_HI_BYTE_NR62    equ $62
ULA_CTRL_NR68               equ $68
TILEMAP_CTRL_NR6B           equ $6B
TILEMAP_BASE_ADR_NR6E       equ $6E
SPRITE_ATTR_3_INC_NR78      equ $78

TBBLUE_REGISTER_SEL_P243B   equ $243B
TBBLUE_LAYER2_P123B         equ $123B

GRAPHIC_PRIORITIES_SLU      = %00000000 ; Sprites over Layer2 over ULA
GRAPHIC_SPRITES_VISIBLE     = %00000001
LORES_ENABLE                = %10000000

    STRUCT SCREEN_BLOCK_DEF
TRIGGER_BIT     DB          ; bit in NEXLOAD_HEADER.LOADSCR to trigger this block
DRAW_PIX_STRIP  DW          ; draw pixel strip routine for loading-bar support
INIT_CODE       DW          ; address of code which will make the loaded data show
TARGET_PAGE     DB          ; first MMU page
PAGES_COUNT     DB          ; amount of pages to load
PAGE_LENGTH     DB          ; high8b of amount of data to load into single page (max 8k)
    ENDS

;; helper macros

    MACRO ESXDOS service? : push hl : pop ix : rst $08 : db service? : ENDM     ; copies HL into IX
    MACRO NEXTREG2A nextreg? : ld a,nextreg? : call readNextReg2A : ENDM
    MACRO CSPECT_BREAK : IFDEF TESTING : break : ENDIF : ENDM

;;-------------------------------
;; Start of the machine code itself
;;-------------------------------

        ORG     ORG_ADDRESS
__bin_b DISP    DISP_ADDRESS

start:
        ; switch Layer2 write-over-ROM OFF first, the code needs write to $2000..$3FFF
        ld      bc,TBBLUE_LAYER2_P123B      ; (it's probably already disabled by dot
        in      a,(c)                       ; command loader, but just in case...)
        and     ~1          ; clear bit 0
        out     (c),a
        ; initial setup and checks of command line
        ld      (oldStack),sp
        ld      a,h
        or      l
        jp      z,emptyLineFinish   ; HL=0
        ; skip leading spaces and parse filename from command line
        dec     hl
.skipLeadingSpace:
        inc     hl
        ld      a,(hl)
        cp      ' '
        jr      z,.skipLeadingSpace
        ld      de,isFileNameChar   ; char-check for unquoted strings
        cp      '"'
        jr      nz,.fileNameNotInQuotes
        ld      de,isFileNameCharInQuotes   ; char-check for quoted strings
        inc     hl                  ; skip the opening quote
.fileNameNotInQuotes:
        ld      (.checkChar),de     ; SMC the loop to check for correct end-of-name chars
        ld      de,filename
        ld      bc,NEXLOAD_MAX_FNAME-1
.copyFN ld      a,(hl)
.checkChar=$+1: call isFileNameChar
        jr      z,.endFN            ; non-filename char found
        ldi                         ; copy char until full buffer
        jp      pe,.copyFN
.endFN: xor     a
        ld      (de),a              ; filename copied + terminated
        ld      a,(filename)        ; check if filename is empty (no real name provided)
        or      a
        jp      z,emptyLineFinish

        ; lock down + setup the environment (DI, stack)
        di
        ld      sp,SP_ADDRESS

        ; try to open the file
        ld      b,FA_READ
        ld      a,'*'
        ld      hl,filename
        ESXDOS  F_OPEN
        jp      c,fileError
        ld      (handle),a
        xor     a
        ld      (fclose),a          ; unlock fclose routine since file is open (ret->nop)
        ; read the NEX file header
        ld      hl,nexHeader
        ld      bc,NEXLOAD_HEADER
        call    fread
        call    checkHeader ; do the checks while the environment is minimally modified

        ; setup environment further (14MHz, ULA CLS, reset all next regs, palettes, etc)
        call    setupBeforeBlockLoading     ; if "preserve" flag is set, only 14MHz turbo is set

        ; check if there's palette data 512B block and load it + set it up
        ld      a,(nexHeader.LOADSCR)
        and     NEXLOAD_LOADSCR_HASPAL|NEXLOAD_LOADSCR_NOPAL
        jr      z,.NoPalLoad                ; neither layer2 or lores screen
        call    p,LoadFilePalette           ; do this only when "no pal" bit is zero
.NoPalLoad:

        ; read all possible screen block types, if they are in the file + process + show
        ld      hl,screenBlocksDefs-SCREEN_BLOCK_DEF
.screenBlocksLoop:
        add     hl,SCREEN_BLOCK_DEF
        ld      a,(nexHeader.LOADSCR)
        and     (hl)
        call    nz,LoadScreenBlock
        or      (hl)
        jr      nz,.screenBlocksLoop

        ;; load all 16k banks marked as included in header data
        ; the header true/false array is in C order (0,1,2,...)
        ; but file blocks are comming shuffled in  (5,2,0,...) order
        xor     a
.loadAllBanks:
        push    af
        call    getRealBankNumber
        ld      hl,nexHeader.BANKS
        add     hl,a
        inc     (hl)            ; test array content for zero (but preserving A value)
        dec     (hl)
        call    nz,loadBankA
        pop     af
        inc     a               ; loop until all banks are loaded
        cp      NEXLOAD_MAX_BANK
        jr      c,.loadAllBanks

        ; final progress bar + delay
        call    drawProgressBar ; final bar here (must be before EntryBank setup!)
        call    startDelay

        ;; all banks loaded, do the final setup before starting it
        ; map entry bank (for NEX files before V1.2 there should be zero in header = OK)
        ld      a,(nexHeader.ENTRYBANK)
        call    mapBankAToSlot3
        ; pass file handle or close file
        ld      hl,(nexHeader.FILEHANDLERET)
        ld      a,h
        or      l
        jr      nz,.passHandleToApp
        call    fclose          ; close the file if handle doesn't need to be passed
        jr      .handleResolved
.passHandleToApp:               ; file handle was requested in some way, fullfil it
        ld      a,h
        cp      $40             ; 1..$3FFF = pass it in BC register
        jr      nc,.passHandleInMemory
        ld      hl,.handleInBcSMC
.passHandleInMemory:
        ld      a,(handle)
        ld      (hl),a
.handleResolved:
        ; set stack pointer, program counter, and BC to 255 ("no handle")|file handle
.handleInBcSMC=$+1 ld bc,255
        ld      hl,(nexHeader.PC)
        ; jump to entry point (or return to BASIC)
        ld      a,h
        or      l
        jr      z,returnToBasic
        ld      sp,(nexHeader.SP)
    IFNDEF TESTING
        rst     $20
    ELSE
        ; create ending sequence to map-in ROM 8k page before doing final JP (HL)
emulateRst20:
.JpHlInRom=$30C1
        ld      hl,$91ED
        ld      (.JpHlInRom-4),hl
        ld      hl,$FF51
        ld      (.JpHlInRom-2),hl   ; "nextreg MMU1_NR51,$FF"
        ld      hl,$34E9            ; "jp (hl) + 0x34 (ROM48 value)" emulator-safety precaution
        ld      (.JpHlInRom),hl     ; (technically ROM should already kick-in there)
        ld      hl,(nexHeader.PC)   ; reload the HL and jump to that new code patch
        jp      .JpHlInRom-4
    ENDIF
;-------------------------------
cleanupBeforeBasic:                 ; internal cleanup, before the need of old stack (must preserve HL)
        call    fclose
        call    switchLayer2Off
        ; map C000..FFFF region back to BASIC bank
        ld      a,($5b5c)
        and     7
        jp      mapBankAToSlot3

;-------------------------------
emptyLineFinish:                    ; here the stack is still old one (OS/BASIC)
        ld      hl,txt_Usage        ; this fails to show anything in TESTING because ROM-mapping
        call    printmsg            ; show usage info
        ; simple `ret` is enough on real board, but continue with full returnToBasic
        ; to make it work also in TESTING, where the ROM has to be mapped back
returnToBasic:  ; cleanup as much as possible
        call    cleanupBeforeBasic
        ld      sp,(oldStack)
        ei
        xor     a                   ; CF=0, A=0 (OK OK)
.err:   nop                         ; place for 'scf' in case error path reuses this
    IFNDEF TESTING
        ret
    ELSE
        pop     hl                  ; fetch original return address of BASIC stack
        ld      (nexHeader.PC),hl   ; make it as "start address" of NEX
        jr      emulateRst20
    ENDIF

;-------------------------------
customErrorToBasic: ; HL = message with |80 last char
        ld      a,$37               ; nop -> scf in exit path
        ld      (returnToBasic.err),a
        jr      returnToBasic

;-------------------------------
checkHeader:                ; CF=0 (no error), BC = bytes actually read from disk
        ld      hl,NEXLOAD_HEADER   ; check if whole header was read
        sbc     hl,bc
        jr      nz,.FileIsNotNex
        ;;TODO: RAM required check
        ; verify first four bytes are "Next"
        ld      hl,(nexHeader)
        ld      bc,'eN'
        sbc     hl,bc       ; CF=0 from previous test
        jr      nz,.FileIsNotNex
        ld      hl,(nexHeader+2)
        ld      bc,'tx'
        sbc     hl,bc       ; CF=0 from previous test
        jr      nz,.FileIsNotNex
        ; check NEX file version
        ld      a,(nexHeader.NEXVERSION.V_MINOR)
        ld      c,a
        ld      a,(nexHeader.NEXVERSION.V_MAJOR)
        swapnib
        xor     c
        xor     $33         ; A = major.minor in packed BCD (if input was ASCII digits)
        cp      NEXLOAD_LOADER_VERSION+1
        jr      nc,.needsLoaderUpdate
        ; check required core version
        ld      a,(nexHeader.COREVERSION.V_MAJOR)
        swapnib
        ld      d,a
        ld      a,(nexHeader.COREVERSION.V_MINOR)
        or      d
        ld      d,a
        ld      a,(nexHeader.COREVERSION.V_SUBMINOR)
        ld      e,a         ; DE = required core version (xxxx'yyyy'zzzz'zzzz)
        ld      (coreVerHeader),de      ; store it for error message routine
        NEXTREG2A NEXT_VERSION_NR01 ; now read the actual board value
        ld      h,a
        NEXTREG2A CORE_VERSION_NR0E
        ld      l,a         ; HL = board core version (xxxx'yyyy'zzzz'zzzz)
        ld      (coreVerBoard),hl       ; store it for error message routine
        sub     hl,de
        ret     nc          ; CF=0 -> board core is enough up to date
.needsCoreUpdate:
        call    prepareForErrorOutput
        ld      sp,(oldStack)
        call    displayCoreVersions
        ld      hl,errTxt_UpdateCore
        jr      customErrorToBasic  ; will reset some things second time, but nevermind
.needsLoaderUpdate:
        call    prepareForErrorOutput
        ld      sp,(oldStack)
        call    displayFileVersion
        ld      hl,errTxt_UpdateLoader
        jr      customErrorToBasic  ; will reset some things second time, but nevermind
.FileIsNotNex:  ; file header is too short, or doesn't contain "Next" string
        ld      hl,errTxt_NotNexFile
        jr      customErrorToBasic

;-------------------------------
screenBlocksDefs:           ; order of block definitions must be same as block order in file
                            ; trigger_bit, init, page, count, pg_length
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_LAYER2, drawPixelStrip_L2,    LoadScr_showLayer2, LAYER2_BANK*2, 6, $20}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_ULA,    drawPixelStrip_Ula,   LoadScr_showUla,    5*2,           1, $1B}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_LORES,  drawPixelStrip_LoRes, LoadScr_showLoRes,  5*2,           2, $18}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_HIRES,  drawPixelStrip_Ula,   LoadScr_showHiRes,  5*2,           2, $18}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_HICOL,  drawPixelStrip_Ula,   LoadScr_showHiCol,  5*2,           2, $18}
        DB                  0       ; terminator

LoadScreenBlock:
        push    hl          ; preserve original HL for caller
        ld      a,(nexHeader.BORDERCOL)
        out     ($FE),a     ; change border colour
        inc     hl
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        inc     hl
        ld      (drawProgressBar.ds),bc ; setup progress bar draw pixel routine per gfx mode
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        inc     hl
        push    bc          ; init code address on stack
        ld      e,(hl)      ; MMU page
        inc     hl
        ld      d,(hl)      ; count of pages
        inc     hl
        ld      b,(hl)
        ld      c,0         ; BC = length of page block
.pageLoop:
        ld      a,e         ; page-in the memory
        nextreg MMU7_NR57,a
        inc     e
        push    de          ; not preserving BC, if file is correct, BC will stay as is
        ld      hl,$E000    ; target address to load (MMU7)
        call    fread       ; load the page block
        pop     de
        dec     d
        jr      nz,.pageLoop
        ld      hl,LoadScr_showLayer2.l2b   ; address to self-modify (L2 visibility)
        ld      c,GRAPHIC_PRIORITIES_SLU+GRAPHIC_SPRITES_VISIBLE    ; common setup
        ret                 ; jump to init code

LoadScr_showLayer2:         ; make Layer2 visible (on top of ULA)
        ld      (hl),$CF    ; set 1,a (making Layer2 visible)
        nextreg LAYER2_RAM_NR12,LAYER2_BANK
.finishScreenInitClassicUla:
        xor     a           ; classic ULA
.finishScreenInitTimex:
        out     (255),a     ; set Timex modes
        ; layer priorities, LoRes mode and sprites visibility
        ld      a,c
        nextreg SPRITE_CONTROL_NR15,a
        ; layer2 visibility
        ld      bc,TBBLUE_LAYER2_P123B
        in      a,(c)
.l2b=$+1 set    1,a         ; gets modified to set/res driving Layer2 visibility
        out     (c),a
        pop     hl          ; restore original HL
        ret

LoadScr_showLoRes:
        ld      (hl),$8F    ; res 1,a (making Layer2 invisible)
        ld      c,GRAPHIC_PRIORITIES_SLU+GRAPHIC_SPRITES_VISIBLE+LORES_ENABLE
        jr      LoadScr_showLayer2.finishScreenInitClassicUla

LoadScr_showHiRes:
        ld      (hl),$8F    ; res 1,a (making Layer2 invisible)
        ld      a,(nexHeader.HIRESCOL)
        and     %00111000
        or      6
        jr      LoadScr_showLayer2.finishScreenInitTimex

LoadScr_showHiCol:
        ld      (hl),$8F    ; res 1,a (making Layer2 invisible)
        ld      a,2
        jr      LoadScr_showLayer2.finishScreenInitTimex

LoadScr_showUla:
        ld      (hl),$8F    ; res 1,a (making Layer2 invisible)
        jr      LoadScr_showLayer2.finishScreenInitClassicUla

;-------------------------------
LoadFilePalette:
        ld      hl,filePalette
        ld      bc,$200
        call    fread
        nextreg PALETTE_CONTROL_NR43,$10    ; NR43=Layer2 first palette
        ld      a,(nexHeader.LOADSCR)
        test    NEXLOAD_LOADSCR_LAYER2
        jr      nz,.setPalette              ; In case Layer2 screen is loaded (or L2+LoRes)
        nextreg PALETTE_CONTROL_NR43,0      ; NR43=ULA first palette
        ; LoRes palette is set only if Layer2 screen is not in the file (L2+LoRes=fail)
.setPalette:
        nextreg PALETTE_INDEX_NR40,0        ; reset colour index to 0
        ld      hl,filePalette              ; and BC=$0200 from fread
.loop:  ld      a,(hl)
        inc     hl
        nextreg PALETTE_VALUE_BIT9_NR44,a
        dec     c
        jr      nz,.loop
        djnz    .loop
        ret

;-------------------------------
; setup environment further (14MHz, reset next regs if enabled, ULA CLS, reset palettes, ...)
setupBeforeBlockLoading:
        ld      a,(nexHeader.LOADBAR)
        or      a
        call    nz,setupProgressBar             ; configure progress bar routine
        ; configure rest of the loader
        nextreg TURBO_CONTROL_NR07,%10          ; set 14MHz turbo
        ld      a,(nexHeader.PRESERVENEXTREG)
        or      a
        ret     nz          ; next regs should be preserved, only 14MHz is set, keep others

        ;;; reset all next regs to default state

        ;; stop Copper asap (and in correct sequence)
        nextreg COPPER_CTRL_HI_BYTE_NR62, 0     ; STOP + clear high part of CPC
        nextreg COPPER_CTRL_LO_BYTE_NR61, 0     ; clear low part of CPC

        ;; special registers first (which are not overwritten completely)
        NEXTREG2A   PERIPHERAL_2_NR06
        or      %10001001   ; turbo key ON, zxnDMA mode, disable lightpen, disable divMMC
        and     %10001101   ; autopaging, enable multiface, keep PS/2, set AY mode
        out     (c),a       ; set nextreg (BC=$253B here)

        NEXTREG2A   PERIPHERAL_3_NR08
        or      %11011110   ; unlock 128k paging, disable contention, keep ABC/ACB stereo
        and     %11111110   ; enable speaker, specdrum, timex and turbosound
        out     (c),a       ; set nextreg (BC=$253B here)

        NEXTREG2A   PERIPHERAL_4_NR09
        and     %11100011   ; unlock sprite-id pairing, enable Kempston, enable divMMC, keep AY modes and scanlines
        out     (c),a       ; set nextreg (BC=$253B here)

        ;; regular registers (one write into each) (BC=$253B here)
        nextreg CLIP_WINDOW_CTRL_NR1C,$0F       ; reset all clip-window indices to zero
        nextreg SOUNDDRIVE_PORT_MIRROR_NR2D,0
        nextreg ULA_CTRL_NR68,0                 ; default ULA control values

        ;; still regular registers (grouped into consecutive groups, from table data)
        ld      hl,nextRegResetData
        call    SetNextRegistersByDataAdvReg

        ;; special registers which require irregular write pattern (from next table data)
        call    SetNextRegistersByData          ; hl = nextRegResetData2 here

        ;; set all sprites to invisible   (BC=$253B here)
        dec     b
        ld      a,SPRITE_ATTR_3_INC_NR78
        out     (c),a       ; select SPRITE_ATTR_3_INC_NR78 register
        ld      e,128       ; reset 128 sprites
        inc     b
.resetSpritesLoop:
        out     (c),0
        dec     e
        jr      nz,.resetSpritesLoop
        ; reset sprite index on $34 reg back to 0
        xor     a
        nextreg SPRITE_MIRROR_INDEX_NR34,a

        call    switchLayer2Off
        call    clsWithBordercol

        ;; reset palettes
        ; ULA classic palette
        nextreg PALETTE_INDEX_NR40,0
        ld      b,16
.resetUlaPalLoop:
        push    bc
        ld      hl,ulaClassicPalette
        call    SetNextRegistersByData
        pop     bc
        djnz    .resetUlaPalLoop

        ; Layer2 + Sprites palette + exit
        nextreg PALETTE_CONTROL_NR43,$10    ; NR43=Layer2 first palette, NR40 is wrapped at 0
        xor     a
        call    .resetSequentialPalLoop
        nextreg PALETTE_CONTROL_NR43,$20    ; NR43=Sprites first palette, NR40 is wrapped at 0
.resetSequentialPalLoop:
        nextreg PALETTE_VALUE_NR41,a
        inc     a
        jr      nz,.resetSequentialPalLoop
        nextreg PALETTE_CONTROL_NR43,a      ; NR43=ULA first palette, NR40 is wrapped at 0
        ret

;-------------------------------
SetNextRegistersByDataAdvReg:
        ld      a,$3C       ; `inc a` instruction
        jr      SetNextRegistersByData.setRegOp
SetNextRegistersByData:
        xor     a           ; 'nop' instruction
.setRegOp:
        ld      (.RegOp),a  ; SMC self-modify-code - how the next reg changes between data
.newBatchLoop:
        ld      a,(hl)      ; initial register number
        inc     hl
        or      a           ; zero terminates the data
        ret     z
        ld      e,(hl)      ; count of [consecutive registers|data] to set (must be > 0)
        inc     hl
.setDataLoop:
        ld      bc,TBBLUE_REGISTER_SEL_P243B
        out     (c),a       ; select next register
        inc     b           ; set data to register
        ld      d,(hl)
        out     (c),d
        inc     hl          ; advance to next data
.RegOp: nop         ; modify register number in A (actual operation depends...)
        dec     e           ; until all are set
        jr      nz,.setDataLoop
        jr      .newBatchLoop  ; read remaining setup data

        ;; regular next reg writes data
nextRegResetData:
        db      LAYER2_RAM_NR12, 6      ; set six registers starting from $12
        db      9, 12, $E3              ; Layer2 banks, Global transparency color
        db      GRAPHIC_PRIORITIES_SLU+GRAPHIC_SPRITES_VISIBLE  ; SLU order and sprites visible
        db      0, 0                    ; Layer2 offset

        db      RASTER_INT_CTRL_NR22, 2 ; set two registers starting from $22 (raster int.)
        db      0, 0

        db      TILEMAP_OFS_X_MSB_NR2F, 6   ; set six registers starting from $2F (tilemap ofs)
        db      0, 0, 0, 0, 0, 0        ; tilemap ofs, ULA ofs, Sprite attribute index

        db      PALETTE_FORMAT_NR42, 2  ; set two registers starting from $42
        db      $0F, 0                  ; ink format to 15, select default palettes + ULA classic

        db      TRANSPARENCY_FALLBACK_NR4A, 3   ; set three registers starting from $4A
        db      0, $E3, $0F             ; transparency: fallback, sprite-T-index, tilemap-T-index

        db      MMU0_NR50, 8            ; set eight MMU registers
    IFDEF TESTING
        db      $FF, TEST_CODE_PAGE, 10, 11, 4, 5, 0, 1 ; to not page-out the fake dot-command environment
    ELSE
        db      $FF, $FF, 10, 11, 4, 5, 0, 1
    ENDIF

        db      TILEMAP_CTRL_NR6B, 2    ; set two registers starting from $6B
        db      0, 0                    ; default Tilemap control/attribute values

        db      TILEMAP_BASE_ADR_NR6E, 2    ; set two registers starting from $6E
        db      0, 0                    ; default Tilemap base/gfx addresses

        db      0                       ; data terminator

nextRegResetData2:
        ;; irregular regs data
        db      CLIP_WINDOW_LAYER2_NR18, 4
        db      0, 255, 0, 191
        db      CLIP_WINDOW_SPRITES_NR19, 4
        db      0, 255, 0, 191
        db      CLIP_WINDOW_ULA_NR1A, 4
        db      0, 255, 0, 191
        db      CLIP_WINDOW_TILEMAP_NR1B, 4
        db      0, 159, 0, 255
        db      0                       ; data terminator

ulaClassicPalette:
        db      PALETTE_VALUE_NR41, 16
        db      $00,$02,$A0,$A2,$14,$16,$B4,$B6
        db      $00,$03,$E0,$E7,$1C,$1F,$FC,$FF
        db      0                       ; data terminator

;-------------------------------
readNextReg2A:
        ld      bc,TBBLUE_REGISTER_SEL_P243B
        out     (c),a
        inc     b
        in      a,(c)
        ret

;-------------------------------
isFileNameChar:     ; A = filename char to test, returns ZF=0 if char is regular character
        cp      ':'
        ret     z
        cp      ' '
        ret     z
        ; continue into isFileNameCharInQuotes implementation to check other chars
;-------------------------------
isFileNameCharInQuotes: ; A = filename char to test, returns ZF=0 if char is regular character
        cp      13
        ret     z
        cp      '"'
        ret     z
        or      a   ; CF=0, ZF=0/1 (1 = "\0" found)
        ret

;-------------------------------
getRealBankNumber:
        cp      .realBankTableSize
        ret     nc
        ld      hl,.realBankTable
        add     hl,a
        ld      a,(hl)
        ret
.realBankTable:     db 5,2,0,1,3,4,6,7  ; order of banks in the file for first eight banks
.realBankTableSize=$-.realBankTable

;-------------------------------
mapBankAToSlot3:
        ; map the bank into C000..FFFF area
        add     a,a
        nextreg MMU6_NR56,a
        inc     a
        nextreg MMU7_NR57,a
        ret

;-------------------------------
loadBankA:
        push    af
        call    drawProgressBar
        pop     af
        call    mapBankAToSlot3
        ; load the bank content
        ld      hl,$C000
        ld      bc,$4000
        call    fread
        jr      bankLoadDelay

;-------------------------------
; E = X coordinate 16..224+16-1, must preserve HL, BC and E, modifies A, D
; ULA draws 1x2px strip per +1 in E, 224 range = 224x2px bar
drawPixelStrip_Ula:
        nextreg MMU7_NR57, 5*2      ; map the ULA screen to E000..FFFF (!)
        push    hl
        ld      d,$BE               ; last two lines of ULA
        pixelad                     ; HL = classic ULA address (at $4000)
        add     hl,$A000            ; transform VRAM address into E000 range
        call    .xorPixel
        pixeldn                     ; move one pixel down
        call    .xorPixel
        pop     hl
        ret
.xorPixel:
        setae                       ; A = bit-pixel from E coordinate
        xor     (hl)                ; xor the pixel
        ld      (hl),a
        ret

;-------------------------------
; E = X coordinate 16..224+16-1, must preserve HL, BC and E, modifies A, D
; LoRes draws 1px per +1 in E, but alternates lines 94/95, 224 range = 112x2px bar
drawPixelStrip_LoRes:
        nextreg MMU7_NR57, 5*2+1    ; map the very bottom of LoRes to E000..FFFF
        ld      d,$F7               ; last two lines of LoRes
        rrc     e                   ; trade last bit of x-axis for y-axis
        ld      a,(nexHeader.LOADBARCOL)
        ld      (de),a
        rlc     e                   ; restore E back to 16..240 range
        ret

;-------------------------------
; E = X coordinate 16..224+16-1, must preserve HL, BC and E, modifies A, D
; Layer2 draws 1x2 strip per +1 in E, 224 range = 224x2px bar
drawPixelStrip_L2:
        nextreg MMU7_NR57, LAYER2_BANK*2+5  ; map the very bottom of Layer2 to E000..FFFF
        ld      d,$FE                       ; last two lines of Layer2
        ld      a,(nexHeader.LOADBARCOL)
        ld      (de),a
        inc     d
        ld      (de),a
drawPixelStrip_None:
        ret

;-------------------------------
drawProgressBar:    ; using Bresenham's line algorithm math to progress per banks-num
        ret         ; will become NOP if loaderbar is enabled, or RET when full-drawn/disabled
.x=$+1  ld      de,$10      ; X-coordinate is designed to go in range 16..224+16-1
.D=$+1  ld      hl,-225     ; current "D"
.b2=$+1 ld      bc,0        ; 2*banksNum (adding to "D")
.drawStrips:
        ; check if progress bar is already full
        ld      a,e
        cp      224+16
        ret     nc          ; no more drawing
        ; draw "1px" strip and advance E (whatever that means in current GFX mode)
.ds=$+1 call    drawPixelStrip_None
        inc     e           ; advance x coordinate
        add     hl,bc
        jr      nc,.drawStrips
        ld      (.x),de     ; store current "x"
        add     hl,-448     ; adjust "D" for next progress step
        ld      (.D),hl
        ret

;-------------------------------
setupProgressBar:
        xor     a
        ld      (drawProgressBar),a     ; ret -> nop, enabling progress bar drawing
        ld      hl,(nexHeader.NUMBANKS)
        ld      h,0                     ; HL = word(NUMBANKS)
        add     hl,hl                   ; *2
        ld      (drawProgressBar.b2),hl ; set "numbanks*2" value in draw routine
        ret

;-------------------------------
bankLoadDelay:
        ld      a,(nexHeader.LOADDELAY)
.delayL or      a
        ret     z
        ei
        dec     a
        halt
        di
        jr      .delayL

;-------------------------------
startDelay:
        ld      a,(nexHeader.STARTDELAY)
        jr      bankLoadDelay.delayL

;-------------------------------
printmsg:
        ld      a,(hl)
        inc     hl
        and     a
        ret     z                       ; exit if terminator
        and     $7F                     ; clear 7th bit
        rst     $10
        jr      printmsg

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
        jp      customErrorToBasic

;-------------------------------
switchLayer2Off:                ; does switch OFF both write-over-ROM and visibility
        push    bc
        ld      bc,TBBLUE_LAYER2_P123B
        out     (c),0
        pop     bc
        ret

;-------------------------------
prepareForErrorOutput           ; do "CLS" of ULA screen with white paper ("error" case)
        nextreg MMU2_NR52,5*2   ; page-in the bank5 explicitly
        nextreg MMU3_NR53,5*2+1
        call    cleanupBeforeBasic
        ld      a,7             ; "error" CLS
        jr      clsWithBordercol.withA

;-------------------------------
clsWithBordercol        ; do "CLS" of ULA screen, using the border colour value from header
        ld      a,(nexHeader.BORDERCOL)
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

;-------------------------------
displayCoreVersions:
        ld      hl,txt_Coretext1    ;"Your core is "
        call    printmsg
        ld      hl,(coreVerBoard)
        call    .displayVersion
        ld      hl,txt_Coretext2    ;", but\rthis file needs "
        call    printmsg
        ld      hl,(coreVerHeader)
        call    .displayVersion
        ld      hl,txt_Coretext3    ;".\rPlease update your core.\r"
        jp      printmsg
.displayVersion:        ; HL = xxxx'yyyy'zzzzzzzz version number
        ; H -> 160, 16, 10, 1 (*256) ; L = 100, 10, 1
        ld      de,.digitPowersTable
        call    .displayOneNumber
        call    .displayOneGroup    ; display two following groups
.displayOneGroup:
        ld      a,'.'
        rst     $10
.displayOneNumber:
        ex      de,hl
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        inc     hl
        ex      de,hl
        ld      a,c
        or      b       ;CF=0
        ret     z       ; BC==0 -> end of digit printing
        ld      a,'0'-1
.digitLoop:
        inc     a
        sbc     hl,bc
        jr      nc,.digitLoop
        add     hl,bc
        rst     $10
        jr      .displayOneNumber
.digitPowersTable
        dw      10*16*256, 1*16*256, 0, 10*256, 1*256, 0, 100, 10, 1, 0

;-------------------------------
displayFileVersion:
        ld      a,(nexHeader.NEXVERSION.V_MAJOR)
        ld      (txt_LoaderVer2),a
        ld      a,(nexHeader.NEXVERSION.V_MINOR)
        ld      (txt_LoaderVer2+2),a
        ld      hl,txt_LoaderVer
        jp      printmsg

;; zero terminated strings for internal print routine
txt_Usage:
        DZ      ".nexload2 <filename> to load nex file.\r"

txt_Coretext1
        DZ      22, $80, $80, "Your core is "
txt_Coretext2
        DZ      ", but\rthis file needs "
txt_Coretext3
        DZ      ".\rPlease update your core.\r"

txt_LoaderVer   ; not zero terminated -> appends next
        DB      22, $80, $80, "This is a V"
txt_LoaderVer2
        DZ      "?.? file. Please\rupdate your NEXLOAD2 version.\r"

;; or $80 terminated strings for custom esx error
errTxt_NotNexFile:
        DC      "Not a NEX file (short/bad data)"
errTxt_UpdateCore:
        DC      "Update core"
errTxt_UpdateLoader:
        DC      "Update .nexload2"

;-------------------------------
coreVerHeader   dw      0
coreVerBoard    dw      0
oldStack        dw      0

last:       ; after last machine code byte which should be part of the binary

    ;; reserved space for values (but not initialized, i.e. not part of the binary)
esxError        ds      34
filename        ds      NEXLOAD_MAX_FNAME
nexHeader       NEXLOAD_HEADER
filePalette     ds      $200

lastReserved:   ASSERT  lastReserved < $3000
    ENDT        ;; end of DISP

    IFNDEF TESTING
        SAVEBIN "NEXLOAD2",start,last-start
    ELSE
testStart
        ; inject "jp testStart" at $8000 for easy re-run from BASIC (if not overwritten)
        ld      a,$C3
        ld      (ORG_ADDRESS-3),a
        ld      hl,testStart
        ld      (ORG_ADDRESS-2),hl
        ; switch channel to upper screen
        ld      a,2
        call    5633
        ; copy ROM charset to inject it into "dot command" area for CSpect "rst $10"
        ld      hl,$3D00
        ld      de,charsetCopy
        ld      bc,$300
        ldir
        ; move the loader into 0x2000..3FFF area, faking dot command environment
        nextreg MMU1_NR51,TEST_CODE_PAGE
        ; copy ROM charset back as first thing
        ld      hl,charsetCopy
        ld      de,$3D00
        ld      bc,$300
        ldir
        ; copy the loader machine code into the area
        ld      hl,__bin_b
        ld      de,$2000
        ld      bc,last-start
        ldir
;         INCLUDE "nexload2.test.progress.i.asm"  ; this was used to develop progress bars
        ; setup fake argument and launch loader
        ld      hl,testFakeName0
        CSPECT_BREAK
        jp      $2000
; screen-loader and basic functions test
testFakeName0   DZ  "\"s p a c e.nex\""
;  coreVersion.nex   loaderVersion.nex    \"s p a c e.nex\"  tmHiRes.nex   tmLoResNoPal.nex   tmNoStart.nex
;  empty.nex         preserveNextRegs.nex   tmHiCol.nex      tmLoRes.nex   tmNoPic.nex        tmUla.nex
; name parsing test
testFakeName1   DB  "   \"Warhawk.nex\" "
testFakeName2   DB  "  NXtel.nex:"          ; leading space, colon
testFakeName3   DB  "NextDAWDemo.nex",13    ; enter
testFakeName4   DB  "ScrollNutter.nex",0    ; \0
testFakeName6   DB  "Daybreak.nex "         ; trailing space
testFakeName7   DB  " \" tmHiCol.nex \" ",0 ; invalid filename with spaces around

charsetCopy:    DS      $300

        SAVESNA "nexload2.sna",testStart
    ENDIF
