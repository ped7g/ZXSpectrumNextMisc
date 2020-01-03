;-------------------------------
; .nexload2
; © Peter Helcmanovsky 2019, license: https://opensource.org/licenses/MIT
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.13.1+)
; For "testing" snapshot for CSpects use option: -DTESTING (or uncomment define below)
;
; The NEX file format: https://specnext.dev/wiki/NEX_file_format
; For file format roadmap (things which may eventually happen) check the wiki link above.
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
; Roadmap (only for this particular loader, for file format check wiki):
; # documentation
; - add docs for "preserve NextRegs", i.e. what environment can the code expect after load
; # code
; - delays timed by raster line, not interrupt (then maybe preserve+restore DI/EI?)
;
; Changelist:
; v2.6  03/01/2020 P7G    V1.3 files support (EXPBUSDISABLE flag), 28MHz for everyone
;                         V1.3 prototype (subject to changes before finalized):
;                           BANKSOFFSET, LOADSCR2, CLIBUFFER, HASCOPPERCODE, CRC32C
; v2.5  19/12/2019 P7G    28Mhz for files V1.3+
; v2.4  03/06/2019 P7G    Core version check only on Next (machine_id), comments updated,
;                         and syntax of source updated with sjasmplus v1.13.1 features
; v2.3  06/05/2019 P7G    Maximum filename length bumped to 261 chars (LFN max)
; v2.2  06/05/2019 P7G    set up C to 255 in case there's no file handle (for C projects)
; v2.1  05/05/2019 P7G    fixing bug in Entry-bank setup
; v2    14/04/2019 P7G    fixing bug in palette loader
; v1    04/04/2019 P7G    Rewriting the NEXLOAD from scratch
;
;-------------------------------
    device zxspectrum48
    OPT reset --zxnext --syntax=abfw
;-------------------------------

;     DEFINE TESTING
    DEFINE ORG_ADDRESS  $2000
    DEFINE DISP_ADDRESS $2000
    DEFINE SP_ADDRESS   $3D00

    IFDEF TESTING
        OPT --zxnext=cspect
        UNDEFINE ORG_ADDRESS
        DEFINE ORG_ADDRESS      $8003
        DEFINE TEST_CODE_PAGE   223         ; using the last page of 2MiB RAM (in CSpect emulator)
    ENDIF

NEXLOAD_LOADER_VERSION      EQU     $13     ; V1.3 in bcd (supported version of file format)
NEXLOAD_MAX_FNAME           EQU     262
NEXLOAD_MAX_BANK            EQU     112

NEXLOAD_LOADSCR_LAYER2      EQU     1       ; loads palette by default
NEXLOAD_LOADSCR_ULA         EQU     2       ; can't have palette
NEXLOAD_LOADSCR_LORES       EQU     4       ; loads palette by default
NEXLOAD_LOADSCR_HIRES       EQU     8       ; Timex HiRes (no palette)
NEXLOAD_LOADSCR_HICOL       EQU     16      ; Timex HiCol (no palette)
NEXLOAD_LOADSCR_EXT2        EQU     64      ; activate extension LOADSCR2 field in V1.3
NEXLOAD_LOADSCR_NOPAL       EQU     128     ; no palette for Layer2/Lores
NEXLOAD_LOADSCR_HASPAL      EQU     NEXLOAD_LOADSCR_LAYER2|NEXLOAD_LOADSCR_LORES|NEXLOAD_LOADSCR_EXT2
NEXLOAD_LOADSCR2_NONE       EQU     0
NEXLOAD_LOADSCR2_320x256x8  EQU     1
NEXLOAD_LOADSCR2_640x256x4  EQU     2
NEXLOAD_LOADSCR2_TILEMODE   EQU     3

;; NEX header structures

    STRUCT NEXLOAD_FILE_VERSION
                DS      1, 'V'
V_MAJOR         DB      '1'
                DS      1, '.'
V_MINOR         DB      '3'
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
; V1.3 features
EXPBUSDISABLE   DB      0       ; 0 = disable expansion bus in NextReg $80, 1 = do not modify $80

;FIXME - finalize V1.3 design with SevenFFF / RVG / Bagley - PROPOSAL follows
HASCHECKSUM     DB      0       ; 0 = no checksum, 1 = has CRC-32C (Castagnoli) checksum (see end of header CRC32C field)
BANKSOFFSET     DD      0       ; where in the file the first bank starts (mandatory value, only V1.0-V1.2 can have 0)
    ; Can be used in future by loaders to try to load even file versions unknown to them (or emus to speed up loading) -
    ; as BANKS + BANKSOFFSET should cover the "important" part of file, and between are loader screens/etc.
    ; but some SW may expect the initial state of machine to include the loader screens, so this may then break the SW
CLIBUFFER       DW      0       ; pointer to buffer for copy of original command line (after ENTRYBANK is paged in)
CLIBUFFERSIZE   DW      0       ; buffer size - the string is zero terminated if smaller, else just truncated (no zero)
    ; if CLIBUFFER is set, DE is initially set to the CLIBUFFER value (no length info, code must know)
    ; FIXME verify that the original data passed into the NEXLOAD2 are outside of $4000..$FFFF range
    ;  - if inside, mitigate the issue by doing temporary early copy, the TESTING variant has buffer in RAM (overwritten by Bank 2 during load)
LOADSCR2        DB      0       ; when LOADSCR |= 64
    ; 1 = Layer 2 320x256x8bpp (NOPAL flag from LOADSCR does apply): [512B palette +] 81920B data (HIRESCOL is L2 palette offset)
    ; 2 = Layer 2 640x256x4bpp (NOPAL flag from LOADSCR does apply): [512B palette +] 81920B data (HIRESCOL is L2 palette offset)
    ; 3 = Tilemode: 512B palette = 508B color data (254 colors only) + 4B NextRegs $6B, $6C, $6E, $6F (Tilemode configuration)
    ;     Palette block is mandatory, NOPAL flag in LOADSCR is invalid (and behaviour undefined)
    ;     Tilemap data are expected to be stored in regular bank 5 - no specialized data block, bank 5 is first any way
HASCOPPERCODE   DB      0       ; 1 = copper code 2048B block after last screen block, starts Copper with %01 control code
RESERVED        DS      508-NEXLOAD_HEADER.RESERVED,0   ; fill up with zeroes to size 512
CRC32C          DD      0       ; little-endian checksum value (for external tools, not used by loaders)
    ; the checksum is calculated as: file offset 512 -> <EOF>, then first 508 bytes (header w/o last 4B)
    ; last 4B of header (offset 508) will be the CRC-32C value itself (not included in CRC calculation)
    ; (expect most of the NEX files to omit the CRC value) (note the CRC covers also appended binary data)
    ENDS
    ASSERT 512 == NEXLOAD_HEADER

;; some further constants, mostly machine/API related

LAYER2_BANK                 equ 9

M_GETERR                    equ $93
F_OPEN                      equ $9A
F_CLOSE                     equ $9B
F_READ                      equ $9D
F_SEEK                      equ $9F
F_FGETPOS                   equ $A0
FA_READ                     equ $01

MACHINE_ID_NR00             equ $00
NEXT_VERSION_NR01           equ $01
PERIPHERAL_1_NR05           equ $05     ;Sets joystick mode, video frequency, Scanlines and Scandoubler.
PERIPHERAL_2_NR06           equ $06     ;Enables Acceleration, Lightpen, DivMMC, Multiface, Mouse and AY audio.
TURBO_CONTROL_NR07          equ $07     ;Turbo mode 0=3.5MHz, 1=7MHz, 2=14MHz, 3=28MHz
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
ULA_OFS_X_NR26              equ $26
ULA_OFS_Y_NR27              equ $27
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
COPPER_DATA_16B_NR63        equ $63
ULA_CTRL_NR68               equ $68
DISPLAY_CTRL_NR69           equ $69
LORES_CTRL_NR6A             equ $6A
TILEMAP_CTRL_NR6B           equ $6B
TILEMAP_DEFAULT_ATTR_NR6C   equ $6C
TILEMAP_BASE_ADR_NR6E       equ $6E
TILEMAP_GFX_ADR_NR6F        equ $6F
LAYER2_CTRL_NR70            equ $70
SPRITE_ATTR_3_INC_NR78      equ $78
EXPANSION_BUS_CONTROL_NR80  equ $80

TBBLUE_REGISTER_SEL_P243B   equ $243B
TBBLUE_LAYER2_P123B         equ $123B

GRAPHIC_PRIORITIES_SLU      = %00000000 ; Sprites over Layer2 over ULA
GRAPHIC_SPRITES_VISIBLE     = %00000001
LORES_ENABLE                = %10000000

    STRUCT SCREEN_BLOCK_DEF
TRIGGER_BIT     DB          ; bit in NEXLOAD_HEADER.LOADSCR to trigger this block
LOADSCR2_BYTE   DB          ; V1.3 extended format value to check (0 for V1.0-V1.2 files)
LAYER_CTRL_NR15 DB          ; sprites + layer priority control
DISPLAY_NR69    DB          ; direct value for NextReg $69 -> makes L2/ULA/Timex visible
DRAW_PIX_STRIP  DB          ; draw pixel strip routine for loading-bar support (low byte)
INIT_CODE       DW          ; address of code which will make the loaded data show
TARGET_PAGE     DB          ; first MMU page
PAGES_COUNT     DB          ; amount of pages to load (+1 for extra "dec d" at start of loop)
PAGE_LENGTH     DB          ; high8b of amount of data to load into single page (max 8k)
    ENDS

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
        ; switch Layer2 write-over-ROM OFF first, the code needs write to $2000..$3FFF
        call    switchLayer2Off     ; will also make Layer2 switch off, but that's ok?!
        ; initial setup and checks of command line
        ld      (oldStack),sp
        ld      a,h
        or      l
        jp      z,emptyLineFinish   ; HL=0
        ld      (originalHL),hl
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

        ; setup environment further (28MHz, ULA CLS, reset all next regs, palettes, etc)
        call    setupBeforeBlockLoading

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
        push    hl
        and     (hl)
        call    nz,LoadScreenBlock
        pop     hl
        or      (hl)
        jr      nz,.screenBlocksLoop

        ld      a,(nexHeader.HASCOPPERCODE)
        or      a
        call    nz,LoadCopperBlock

        ; check the file offset against BANKSOFFSET
        ld      a,(nexFileVersion)
        cp      $13
        jr      c,.skipFposCheck ; file format V1.0 .. V1.2 don't contain BANKSOFFSET value

        ; original FGETPOS variant, missing on CSpect and ZEsarUX, didn't try with full OS
;         ld      a,(handle)
;         rst     $08
;         DB      F_FGETPOS       ; BCDE = current file pointer

        ; alternate F_SEEK variant, which works even in current CSpect without full OS
        ld      a,(handle)
        sbc     hl,hl           ; HL = 0 (CF=0 from previous CP)
        ld      bc,hl           ; fake
        ld      de,hl           ; fake
        inc     l               ; ADD fwd mode $01
        ESXDOS  F_SEEK          ; BCDE = current file pointer
        ; end of alternate F_SEEK variant   ; TODO switch to FGETPOS one day...

        jr      c,.skipFposCheck    ; should not fail?! but if it ever does, skip check
        ld      hl,(nexHeader.BANKSOFFSET)
        sbc     hl,de           ; CF=0 from F_FGETPOS
        jp      nz,V_1_3_BanksOffsetTestFailed
        ld      hl,(nexHeader.BANKSOFFSET+2)
        sbc     hl,bc           ; CF=0 from previous SBC
        jp      nz,V_1_3_BanksOffsetTestFailed
.skipFposCheck:

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
        ; copy the full CLI buffer as it was given initially to NEXLOAD2
        ld      de,(nexHeader.CLIBUFFER)
        ld      bc,(nexHeader.CLIBUFFERSIZE)
        ld      a,d
        or      e
        jr      z,.noCliBufferProvided
        ld      a,b
        or      c
        jr      z,.noCliBufferProvided
        ld      hl,(originalHL)
        ldir                    ; copy the CLI buffer as is (truncated by max size)
.noCliBufferProvided:
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
; put any draw PixelStrip routine into this area, to have them all with same hi-byte address
drawPixelStrip_Area:

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
; 320x256 / 640x256 draws 1x2/2x2 strip per +1 in E, 224 range = 224x2px/448x2px bar
drawPixelStrip_L2rot:
        push    de
        ld      a,e
        or      $E0
        ld      d,a                 ; $E0 | bottom 5 bits = high byte adr for MMU7
        ; calculate bank number by doing "div 32" to 9bit X (CF:A).
        ld      a,e                 ; CF=0 from OR above, A=16..239
        rra                         ; now do div 32 of 9b value (CF:A)
        swapnib
        and     $0F                 ; bank number 0..15 (per 32 pixel strips, per 8kiB)
        add     a,LAYER2_BANK*2+1   ; add extra +1 to do shift bar +32px to right (centers it)
        nextreg MMU7_NR57,a         ; map the very bottom of Layer2 to E000..FFFF
        ; finalize address in DE, draw two pixels at [x+32,254] and [x+32,255]
        ld      e,$FE               ; Y=254
        ld      a,(nexHeader.LOADBARCOL)
        ld      (de),a
        inc     e                   ; +1 = pixel below
        ld      (de),a
        pop     de                  ; E must be restored
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
; END OF  draw PixelStrip area
        ASSERT high $ == high drawPixelStrip_Area

;-------------------------------
cleanupBeforeBasic:                 ; internal cleanup, before the need of old stack (must preserve HL)
        call    fclose
        call    switchLayer2Off
        ; map C000..FFFF region back to BASIC bank
        ld      a,($5b5c)
        and     7
        jp      mapBankAToSlot3

V_1_3_BanksOffsetTestFailed:
        call    prepareForErrorOutput
        ld      sp,(oldStack)
        ld      hl,errTxt_BanksOffsetMismatch
        jp      customErrorToBasic  ; will reset some things second time, but nevermind

;-------------------------------
checkHeader:                ; in: CF=0 (no error), BC = bytes actually read from disk
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
        ld      (nexFileVersion),a
        cp      NEXLOAD_LOADER_VERSION+1
        jr      nc,.needsLoaderUpdate
        ; read the core version (even if the following check is not done, like in emulator)
        NEXTREG2A NEXT_VERSION_NR01
        ld      h,a
        NEXTREG2A CORE_VERSION_NR0E
        ld      l,a                 ; HL = board core version (xxxx'yyyy'zzzz'zzzz)
        ld      (coreVerBoard),hl   ; store it for error message routine (and other checks)
        ; check if being run on other machine than Next => skip the core requirement check
        NEXTREG2A MACHINE_ID_NR00
        cp      10          ; 8 = emulator, 10 = ZX Spectrum Next
        ret     nz
        ; check required core version (only on the ZX Spectrum Next itself)
        ld      a,(nexHeader.COREVERSION.V_MAJOR)
        swapnib
        ld      d,a
        ld      a,(nexHeader.COREVERSION.V_MINOR)
        or      d
        ld      d,a
        ld      a,(nexHeader.COREVERSION.V_SUBMINOR)
        ld      e,a         ; DE = required core version (xxxx'yyyy'zzzz'zzzz)
        ld      (coreVerHeader),de      ; store it for error message routine
        ; CF=0 from `or d` above
        sbc     hl,de
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
        ;WARNING: fallthrough into customErrorToBasic

;-------------------------------
customErrorToBasic: ; HL = message with |80 last char
        ld      a,$37               ; nop -> scf in exit path
        ld      (returnToBasic.err),a
        jp      returnToBasic

;-------------------------------
screenBlocksDefs:           ; order of block definitions must be same as block order in file
        ; trigger_bit, loadscr2, NR15, NR69, draw_bar, init, page, count+1, pg_length
NR15v   EQU     GRAPHIC_PRIORITIES_SLU+GRAPHIC_SPRITES_VISIBLE
NR69v   EQU     0
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_LAYER2, NEXLOAD_LOADSCR2_NONE,      NR15v,     NR69v+$80, low drawPixelStrip_L2,    LoadScr_showLayer2,  LAYER2_BANK*2, 6+1, $20}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_ULA,    NEXLOAD_LOADSCR2_NONE,      NR15v,     NR69v,     low drawPixelStrip_Ula,   LoadScr_finish,      5*2,           1+1, $1B}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_LORES,  NEXLOAD_LOADSCR2_NONE,      NR15v+$80, NR69v,     low drawPixelStrip_LoRes, LoadScr_finish,      5*2,           2+1, $18}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_HIRES,  NEXLOAD_LOADSCR2_NONE,      NR15v,     NR69v+$06, low drawPixelStrip_Ula,   LoadScr_showHiRes,   5*2,           2+1, $18}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_HICOL,  NEXLOAD_LOADSCR2_NONE,      NR15v,     NR69v+$02, low drawPixelStrip_Ula,   LoadScr_finish,      5*2,           2+1, $18}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_EXT2,   NEXLOAD_LOADSCR2_320x256x8, NR15v,     NR69v+$80, low drawPixelStrip_L2rot, LoadScr_show320x256, LAYER2_BANK*2,10+1, $20}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_EXT2,   NEXLOAD_LOADSCR2_640x256x4, NR15v,     NR69v+$80, low drawPixelStrip_L2rot, LoadScr_show640x256, LAYER2_BANK*2,10+1, $20}
        SCREEN_BLOCK_DEF    {NEXLOAD_LOADSCR_EXT2,   NEXLOAD_LOADSCR2_TILEMODE,  NR15v,     NR69v,     low drawPixelStrip_None,  LoadScr_showTiles,   5*2,           0+1, $20}
        DB                  0       ; terminator

LoadScreenBlock:
        ; V1.3 extension of new types of screens - check the second byte when first bit matched
        ld      a,(nexHeader.LOADSCR2)
        inc     hl
        cp      (hl)
        ret     nz          ; this is not the correct block (only first bit matched)
        ld      a,(nexHeader.BORDERCOL)
        out     ($FE),a     ; change border colour
        ; setup NR15 (sprites visibility, layers priority and LoRes enable)
        inc     hl
        ld      a,(hl)
        nextreg SPRITE_CONTROL_NR15,a
        ; read NR69 init value (do not set yet, may require patching by NEXLOAD_LOADSCR_HICOL)
        inc     hl
        ld      a,(hl)
        ld      (LoadScr_finish.n),a    ; self-modify the `or $nn` instruction
        ; setup draw pixel strip routine address
        inc     hl
        ld      a,(hl)
        ld      (drawProgressBar.ds),a  ; setup progress bar draw pixel routine per gfx mode
            ; only low byte is set, the high byte should be `high drawPixelStrip_Area`
        ; setup init code address (pushed to stack for "retpoline")
        inc     hl
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        push    bc          ; init code address on stack
        ; MMU paging, count of pages, size of pages
        inc     hl
        ld      e,(hl)      ; MMU page
        inc     hl
        ld      d,(hl)      ; count of pages
        ld      c,0
        inc     hl
        ld      b,(hl)      ; BC = length of page block
.pageLoop:
        dec     d
        ret     z           ; jump to init code, when no more pages to load
        ld      a,e         ; page-in the memory
        nextreg MMU7_NR57,a
        inc     e
        push    de          ; not preserving BC, if file is correct, BC will stay as is
        ld      hl,$E000    ; target address to load (MMU7)
        call    fread       ; load the page block
        pop     de
        jr      .pageLoop

LoadScr_showLayer2:
        xor     a           ; mode 256x192, zero pal offset
.customNr70:
        nextreg LAYER2_CTRL_NR70,a
        nextreg LAYER2_RAM_NR12,LAYER2_BANK
LoadScr_finish:
        xor     a
.patchA:
.n=$+1 :or      0           ; will be overwritten by NR69v from the definition block
        ; make layer 2 / timex visible as per value in A
        nextreg DISPLAY_CTRL_NR69,a
        ret

LoadScr_showTiles:
    ; palette block is mandatory for tile mode screen, the last 4B are init data!
    ; so only 254 colours are legit, last 4B are next regs: $6B, $6C, $6E, $6F
        ld      hl,filePalette+254*2
        ld      a,(hl)
        inc     hl
        nextreg TILEMAP_CTRL_NR6B,a
        ld      a,(hl)
        inc     hl
        nextreg TILEMAP_DEFAULT_ATTR_NR6C,a
        ld      a,(hl)
        inc     hl
        nextreg TILEMAP_BASE_ADR_NR6E,a
        ld      a,(hl)
        inc     hl
        nextreg TILEMAP_GFX_ADR_NR6F,a
        jr      LoadScr_finish

LoadScr_show640x256:
        call    LoadScr_show320x256.setupClipWindowAndA
        or      $20
        jr      LoadScr_showLayer2.customNr70

LoadScr_show320x256:
        call    .setupClipWindowAndA
        or      $10
        jr      LoadScr_showLayer2.customNr70
.setupClipWindowAndA:
        nextreg CLIP_WINDOW_CTRL_NR1C,$01   ; reset L2 clip window index
        xor     a
        nextreg CLIP_WINDOW_LAYER2_NR18,a
        nextreg CLIP_WINDOW_LAYER2_NR18,159
        nextreg CLIP_WINDOW_LAYER2_NR18,a
        dec     a
        nextreg CLIP_WINDOW_LAYER2_NR18,a   ; set clip window to 0,159,0,255
        ; set A to HIRESCOL & $0F => palette offset for LAYER2_CTRL_NR70
        ld      a,(nexHeader.HIRESCOL)
        and     $0F
        ret

LoadScr_showHiRes:
        ld      a,(nexHeader.HIRESCOL)
        and     %00111000
        jr      LoadScr_finish.patchA

;-------------------------------
LoadFilePalette:
        ld      hl,filePalette
        ld      bc,$200
        call    fread
        nextreg PALETTE_CONTROL_NR43,%0'001'000'0   ; NR43=Layer2 first palette
        ld      a,(nexHeader.LOADSCR)
        test    NEXLOAD_LOADSCR_LAYER2
        jr      nz,.setPalette              ; In case Layer2 screen is loaded (or L2+LoRes)
        ; check new V1.3 modes in second field of header
        test    NEXLOAD_LOADSCR_EXT2
        jr      z,.setUlaPalette            ; this is LoRes screen (not a new V1.3 mode)
        ld      a,(nexHeader.LOADSCR2)
        cp      NEXLOAD_LOADSCR2_TILEMODE
        jr      nz,.setPalette              ; Layer2 320x256 or 640x256
        nextreg PALETTE_CONTROL_NR43,%0'011'000'0   ; NR43=Tilemap first palette
        jr      .setPalette
.setUlaPalette:
        nextreg PALETTE_CONTROL_NR43,0      ; NR43=ULA first palette (LoRes mode)
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
LoadCopperBlock:
        ld      b,2048/$200     ; 4x 512 = 2048B (buffer has only 512B)
.next512Bblock:
        push    bc
        ld      hl,filePalette
        ld      bc,$200
        call    fread
        ld      hl,filePalette
.setCopperDataLoop:
        ld      a,(hl)
        inc     hl
        nextreg COPPER_DATA_16B_NR63,a
        dec     c
        jr      nz,.setCopperDataLoop
        djnz    .setCopperDataLoop
        pop     bc
        djnz    .next512Bblock
        nextreg COPPER_CTRL_HI_BYTE_NR62,$40    ; reset CPC and start the copper
        ret

;-------------------------------
; setup environment further (28MHz, reset next regs if enabled, ULA CLS, reset palettes, ...)
setupBeforeBlockLoading:
        ld      a,(nexHeader.LOADBAR)
        or      a
        call    nz,setupProgressBar             ; configure progress bar routine
        ; configure rest of the loader
        nextreg TURBO_CONTROL_NR07,%11          ; set 28MHz turbo
        ld      a,(nexHeader.EXPBUSDISABLE)
        or      a
        call    z,disableExpansionBus
        ld      a,(nexHeader.PRESERVENEXTREG)
        or      a
        ret     nz          ; next regs should be preserved, only 28MHz is set, keep others

        ;;; reset all next regs to default state

        ;; stop Copper asap (control byte first, but it should not really matter)
        nextreg COPPER_CTRL_HI_BYTE_NR62, 0     ; STOP + clear high part of CPC
        nextreg COPPER_CTRL_LO_BYTE_NR61, 0     ; clear low part of CPC

        ;; special registers first (which are not overwritten completely)
        NEXTREG2A   PERIPHERAL_2_NR06
        or      %00001001   ; keep F8 turbo key, set zxnDMA mode, keep F3 50/60Hz key,
        and     %10101101   ; divMMC autopaging off, multiface on, keep PS/2, set AY mode
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

        ;; still regular registers (grouped into consecutive groups, from table data)
        ld      hl,nextRegResetData
        call    SetNextRegistersByDataAdvReg

        ;; set all sprites to invisible   (BC=$253B here)
        dec     b
        ld      a,SPRITE_ATTR_3_INC_NR78
        out     (c),a       ; select SPRITE_ATTR_3_INC_NR78 register
        ld      e,128       ; reset 128 sprites
        inc     b
        xor     a
.resetSpritesLoop:
        out     (c),a
        dec     e
        jr      nz,.resetSpritesLoop
        ; reset sprite index on $34 reg back to 0
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
SetNextRegistersByData:
        xor     a           ; 'nop' instruction
        ; SMC self-modify-code - how the next reg changes between data
        ld      (SetNextRegistersByDataAdvReg.RegOp),a
SetNextRegistersByDataAdvReg:   ; "inc a" is the default register operation - just run
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
.RegOp: inc     a           ; modify register number in A (actual operation depends...)
        dec     e           ; until all are set
        jr      nz,.setDataLoop
        jr      .newBatchLoop  ; read remaining setup data

        ;; regular next reg writes data
nextRegResetData:
        db      LAYER2_RAM_NR12, 10     ; set ten registers starting from $12
        db      9, 12, $E3              ; Layer2 banks, Global transparency color
        db      GRAPHIC_PRIORITIES_SLU+GRAPHIC_SPRITES_VISIBLE  ; SLU order and sprites visible
        db      0, 0                    ; Layer2 offset
        db      0, 0, 0, 0              ; X1 of clip windows (L2, sprite, ULA, tiles)
        db      CLIP_WINDOW_LAYER2_NR18, 4      ; set X2 of clip windows
        db      255, 255, 255, 159
        db      CLIP_WINDOW_LAYER2_NR18, 4      ; set Y1 of clip windows
        db      0, 0, 0, 0
        db      CLIP_WINDOW_LAYER2_NR18, 4      ; set Y2 of clip windows
        db      191, 191, 191, 255

        db      RASTER_INT_CTRL_NR22, 2 ; set two registers starting from $22 (raster int.)
        db      0, 0

        db      ULA_OFS_X_NR26, 2       ; ULA offset (since core 3.0.5?)
        db      0, 0

        db      TILEMAP_OFS_X_MSB_NR2F, 6   ; set six registers starting from $2F (tilemap ofs)
        db      0, 0, 0, 0, 0, 0        ; tilemap ofs, LoRes ofs, Sprite attribute index

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

        db      ULA_CTRL_NR68, 5
        ; ULActrl=0, L2/shadowULA/Timex=0, LoResCtrl=0, TilemapCtrl=0, Tile_def_attr=0
        db      0, 0, 0, 0, 0

        db      TILEMAP_BASE_ADR_NR6E, 3    ; set three registers starting from $6E
        db      0, 0, 0                 ; default Tilemap base/gfx addresses, Layer2ctrl = 0

        db      0                       ; data terminator

ulaClassicPalette:
        db      PALETTE_VALUE_NR41, 16
        db      $00,$02,$A0,$A2,$14,$16,$B4,$B6
        db      $00,$03,$E0,$E7,$1C,$1F,$FC,$FF
        db      0                       ; data terminator

;-------------------------------
disableExpansionBus:
    ; V1.3 file feature, when 0 (default value in V1.2) -> disable expansion bus
    ; else (non zero value) do nothing (this subroutine is called after the check already)
    ; (!) The whole feature is applied only for cores 3.0.5+
        ; check core version
        ld      hl,(coreVerBoard)
        ld      de,$3005
        sub     hl,de       ; fake (adds `or a` ahead)
        ret     c           ; don't touch NextReg $80 when core is older than 3.0.5
        ; disable Expansion bus, enable I/O cycles and /IORQULA, enable memory cycles and /ROMCS
        NEXTREG2A   EXPANSION_BUS_CONTROL_NR80
        and     %00001111
        out     (c),a       ; set nextreg (BC=$253B here)
        ret

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
        ld      a,(nexHeader.NUMBANKS)
        add     a,a                     ; NUMBANKS*2
        ld      (drawProgressBar.b2),a  ; set "numbanks*2" value in draw routine
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
errTxt_BanksOffsetMismatch:
        DC      "Header val BANKSOFFSET mismatch"

;-------------------------------
coreVerHeader   dw      0
coreVerBoard    dw      0
oldStack        dw      0

last:       ; after last machine code byte which should be part of the binary

    ;; reserved space for values (but not initialized, i.e. not part of the binary)
nexFileVersion  db      0       ; BCD-packed ($13 for V1.3)
originalHL      dw      0       ; original pointer to line buffer
esxError        ds      34
filename        ds      NEXLOAD_MAX_FNAME
nexHeader       NEXLOAD_HEADER
filePalette     ds      $200    ; will be re-used also for copper code load

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
;         CSP_BREAK
        jp      $2000
; screen-loader and basic functions test
testFakeName0   DZ  "\"s p a c e.nex\""
;  coreVersion.nex   loaderVersion.nex    \"s p a c e.nex\"  tmHiRes.nex   tmLoResNoPal.nex   tmNoStart.nex
;  empty.nex         preserveNextRegs.nex   tmHiCol.nex      tmLoRes.nex   tmNoPic.nex        tmUla.nex
;  t320x256.nex      t640x256.nex
; name parsing test
testFakeName1   DB  "   \"Warhawk.nex\" "
testFakeName2   DB  "  NXtel.nex:"          ; leading space, colon
testFakeName3   DB  "NextDAWDemo.nex",13    ; enter
testFakeName4   DB  "ScrollNutter.nex",0    ; \0
testFakeName5   DB  " CrowleyWorldTour.nex ",0
testFakeName6   DB  "Daybreak.nex "         ; trailing space
testFakeName7   DB  " \" tmHiCol.nex \" ",0 ; invalid filename with spaces around

charsetCopy:    DS      $300

        SAVESNA "nexload2.sna",testStart
;         CSPECTMAP     ; wrong values because of DISP and structures (sjasmplus v1.13.1)
    ENDIF
