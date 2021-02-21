;-------------------------------
; examine.asm - NEX file header examinator for sjasmplus
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.18.0+)
; Usage: sjasmplus --msg=war examine.asm -DNEX_FILE=\"some.nex\"
;
; The NEX file format: https://specnext.dev/wiki/NEX_file_format
;
; This is using sjasmplus macro-language at assembling time to load and display
; NEX file header data, edit the NEX file name here in the source
; before "assembling" to examine it, or use -DNEX_FILE=\"some.nex\"

    IFNDEF NEX_FILE
        DEFINE NEX_FILE "some.nex"
    ENDIF
    DEVICE ZXSPECTRUMNEXT : OPT -Wno-backslash

    ORG 0
    INCBIN NEX_FILE, 0, 512     ; read only header into memory at ORG0

    ; get real file length (can't be done with sj native directives, lua script needed)
    LUA ALLPASS
        fnameDef = sj.get_define("NEX_FILE")
        fnameFirst = string.char(string.byte(fnameDef))
        fnameLast = string.char(string.byte(fnameDef,-1))
        -- strip quotes/apostrophes from fnameDef (they are there for INCBIN in asm part)
        if fnameFirst == fnameLast and ('"' == fnameFirst or "'" == fnameFirst) then
            fnameDef = string.sub(fnameDef, 2, string.len(fnameDef)-1)
        end
        nexfile = io.open(fnameDef,"r")
        if nexfile then
            fsize = nexfile:seek("end")
            io.close(nexfile)
            sj.insert_label("NEX_SIZE", fsize)
        else
            sj.insert_label("NEX_SIZE", 0)
        end
    ENDLUA
    ASSERT 0 < NEX_SIZE

;-------------------------------
; internal struct definitions

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
HASCHECKSUM     DB      0       ; 0 = no checksum, 1 = has CRC-32C (Castagnoli) checksum (see end of header CRC32C field)
BANKSOFFSET     DD      0       ; where in the file the first bank starts (mandatory value, only V1.0-V1.2 can have 0)
    ; Can be used in future by loaders to try to load even file versions unknown to them (or emus to speed up loading) -
    ; as BANKS + BANKSOFFSET should cover the "important" part of file, and between are loader screens/etc.
    ; but some SW may expect the initial state of machine to include the loader screens, so this may then break the SW
CLIBUFFER       DW      0       ; pointer to buffer for copy of original command line (after ENTRYBANK is paged in)
CLIBUFFERSIZE   DW      0       ; buffer size - the string is zero/enter/colon terminated if smaller, else truncated (no terminator)
    ; if CLIBUFFER is set, DE is initially set to the CLIBUFFER value (no length info, code must know)
    ; max buffer size is 2048, if the app needs longer argument lines, preserve banks which contain BASIC sysvars and edit line
    ; and search for the full line remaining in original position (locate by clibuffer content or sysvars)
LOADSCR2        DB      0       ; when LOADSCR |= 64
    ; 1 = Layer 2 320x256x8bpp (NOPAL flag from LOADSCR does apply): [512B palette +] 81920B data (HIRESCOL is L2 palette offset)
    ; 2 = Layer 2 640x256x4bpp (NOPAL flag from LOADSCR does apply): [512B palette +] 81920B data (HIRESCOL is L2 palette offset)
    ; 3 = Tilemode - config data are in TILESCRCONFIG array, (NOPAL flag from LOADSCR does apply): [512B palette]
    ;     Tilemap data are expected to be stored in regular bank 5 - no specialized data block, bank 5 is first any way
HASCOPPERCODE   DB      0       ; 1 = copper code 2048B block after last screen block, starts Copper with %01 control code
TILESCRCONFIG   DS      4, 0    ; NextReg registers $6B, $6C, $6E, $6F values for Tilemode screen loader
BIGL2BARPOSY    DB      0       ; Y position (0..255) of loading bar for new Layer 2 320x256 and 640x256 modes
RESERVED        DS      508-NEXLOAD_HEADER.RESERVED,0   ; fill up with zeroes to size 512
CRC32C          DD      0       ; little-endian uint32_t checksum value (for external tools, not used by loaders)
    ; the checksum is calculated as: file offset 512 -> <EOF>, then first 508 bytes (header w/o last 4B)
    ; last 4B of header (offset 508) will be the CRC-32C value itself (not included in CRC calculation)
    ; (expect most of the NEX files to omit the CRC value) (note the CRC covers also appended binary data)
    ENDS
    ASSERT 512 == NEXLOAD_HEADER

;-------------------------------
; examinator + display info

head    NEXLOAD_HEADER = $0000      ; use the structure to access the header data
    DISPLAY "Examining file: ",NEX_FILE
    IF 'eN' != { head } || 'tx' != { head+2 }
        DISPLAY "First four bytes are not \"Next\": ",/C,{b head+0},/C,{b head+1},/C,{b head+2},/C,{b head+3}
    ENDIF
    IF 'V' != {b head.NEXVERSION } || '.' != {b head.NEXVERSION+2 }
        DISPLAY "Weird file version, not \"Vx.y\": ",/C,{b head.NEXVERSION+0},/C,{b head.NEXVERSION+1},/C,{b head.NEXVERSION+2},/C,{b head.NEXVERSION+3}
    ELSE
        DISPLAY "File version: ",/C,{b head.NEXVERSION+1},".",/C,{b head.NEXVERSION+3}
        IF '1' == {b head.NEXVERSION+1} && '3' == {b head.NEXVERSION+3}
            DISPLAY "WARNING: V1.3 is unofficial extension by Ped7g, not supportted by NextZXOS .nexload"
        ENDIF
    ENDIF
    IF {b head.RAMREQ} : DEFINE+ RAMREQ_INFO " (1792ki)" : ELSE : DEFINE+ RAMREQ_INFO " (768ki)" : ENDIF
    DEFINE CORE_VERSION /D,{b head.COREVERSION.V_MAJOR},".",/D,{b head.COREVERSION.V_MINOR},".",/D,{b head.COREVERSION.V_SUBMINOR}
    DISPLAY "Required RAM: ",/D,{b head.RAMREQ},RAMREQ_INFO,", Required core version: ",CORE_VERSION
    IF 0 == {b head.COREVERSION.V_MAJOR} && {b head.COREVERSION.V_MINOR}
        DISPLAY "WARNING: seems the required core major/minor values are swapped"
    ENDIF
palSize = 0
screenSize = 0
    IF {b head.LOADSCR}
        IF {b head.LOADSCR} & NEXLOAD_LOADSCR_NOPAL
            DISPLAY "load screen: +no palette"
        ELSEIF {b head.LOADSCR} & NEXLOAD_LOADSCR_HASPAL
            DISPLAY "load screen: +palette"
palSize = 512
        ENDIF
        IF {b head.LOADSCR} & NEXLOAD_LOADSCR_LAYER2
            DISPLAY "load screen: layer2 256x192 8bpp"
screenSize = screenSize + 256*192
        ENDIF
        IF {b head.LOADSCR} & NEXLOAD_LOADSCR_ULA
            DISPLAY "load screen: ULA 256x192 6912B"
screenSize = screenSize + 6912
        ENDIF
        IF {b head.LOADSCR} & NEXLOAD_LOADSCR_LORES
            DISPLAY "load screen: LoRes 128x96 8bpp"
screenSize = screenSize + 128*96
        ENDIF
        IF {b head.LOADSCR} & NEXLOAD_LOADSCR_HIRES
            DISPLAY "load screen: Timex HiRes 512x192 1bpp Color 0..7: ",/D,{b head.HIRESCOL}/8
screenSize = screenSize + 2*32*192
        ENDIF
        IF {b head.LOADSCR} & NEXLOAD_LOADSCR_HICOL
            DISPLAY "load screen: Timex HiCol 256x192 8x1 attributes"
screenSize = screenSize + 2*32*192
        ENDIF
        IF {b head.LOADSCR} & NEXLOAD_LOADSCR_EXT2
            DISPLAY "load screen: +V1.3 extensions (TODO to display)"
            ;TODO add V1.3 parsing
        ENDIF
    ELSE
        DISPLAY "No loading screen"
    ENDIF
    IF {b head.LOADBAR}
        DISPLAY "Border colour: ",/D,{b head.BORDERCOL},", show progress bar, colour: ",{b head.LOADBARCOL}
    ELSE
        DISPLAY "Border colour: ",/D,{b head.BORDERCOL},", no progress bar"
    ENDIF
    DISPLAY "Delay after each bank: ",/D,{b head.LOADDELAY},", before start: ",/D,{b head.STARTDELAY}
    IF {b head.PRESERVENEXTREG} : DEFINE+ PRESERVE_INFO " (preserve current)" : ELSE : DEFINE+ PRESERVE_INFO " (reset to NEX defaults)" : ENDIF
    DISPLAY "Preserve next registers: ",/D,{b head.PRESERVENEXTREG},PRESERVE_INFO
    IF 0 < palSize : DEFINE+ PAL_SIZE_INFO ," + ",/D,palSize," (pal)" : ELSE : DEFINE+ PAL_SIZE_INFO : ENDIF
    IF 0 < screenSize : DEFINE+ SCREEN_SIZE_INFO ," + ",/D,screenSize," (screen(s))" : ELSE : DEFINE+ SCREEN_SIZE_INFO : ENDIF
banksSize = {b head.NUMBANKS}*(1<<14)
    IF 0 < banksSize : DEFINE+ BANKS_SIZE_INFO ," + ",/D,banksSize," (banks)" : ELSE : DEFINE+ BANKS_SIZE_INFO : ENDIF
dataSize = NEX_SIZE - (512 + palSize + screenSize + banksSize)
    IF 0 < dataSize : DEFINE+ DATA_SIZE_INFO ," + ",/D,dataSize," (data)" : ELSE : DEFINE+ DATA_SIZE_INFO : ENDIF
totalSize = 512 + palSize + screenSize + banksSize + dataSize
    DISPLAY "File size: 512 (header)" PAL_SIZE_INFO SCREEN_SIZE_INFO BANKS_SIZE_INFO DATA_SIZE_INFO," = ",/D,totalSize
    IF 0 < {head.PC} : DEFINE+ PCVAL_INFO /A,{head.PC} : ELSE : DEFINE+ PCVAL_INFO "no start" : ENDIF
    DISPLAY "PC = ",PCVAL_INFO, " | SP = ",/A,{head.SP}
    DISPLAY "Banks to load (counter): ",/D,{b head.NUMBANKS},", code-entry-bank ($C000): ",/D,{b head.ENTRYBANK}
pcBank = (({head.PC}>>14) * 5) & 7  ; 0, 5, 2, 7
    IF 7 == pcBank
pcBank = {b head.ENTRYBANK}
    ENDIF
spBank = (((({head.SP}-1)>>14)&3) * 5) & 7  ; 0, 5, 2, 7 WRT "SP-1"
    IF 7 == spBank
spBank = {b head.ENTRYBANK}
    ENDIF
bankI = 0
bankRangeB = -1
missingMemRequired = 0
    DUP NEXLOAD_MAX_BANK + 1
        IF 48 <= bankI && {b head.BANKS + bankI} && !{b head.RAMREQ}
missingMemRequired = -1
        ENDIF
        IF NEXLOAD_MAX_BANK == bankI || 5 == bankI || 2 == bankI || {b head.ENTRYBANK} == bankI || 8 == bankI || !{b head.BANKS + bankI}
            ; display previous range
            IF 0 <= bankRangeB
                IF (bankI - bankRangeB) < 3 ; 1 or 2 banks -> individual lines
                    DUP (bankI - bankRangeB)
                        DISPLAY "+ bank ",/D,bankRangeB
bankRangeB = bankRangeB + 1
                    EDUP
                ELSE    ; 3+ banks -> range line
                    DISPLAY "+ banks ",/D,bankRangeB,"..",/D,bankI-1
                ENDIF
            ENDIF
bankRangeB = -1     ; reset range
            ; if one of the mapped banks with extra info (never displayed in range) -> display
            IF bankI < NEXLOAD_MAX_BANK && {b head.BANKS + bankI}
                DEFINE+ BANK_PC_INFO ""
                IF pcBank == bankI : DEFINE+ BANK_PC_INFO " <- PC -<" : ENDIF
                DEFINE+ BANK_SP_INFO ""
                IF spBank == bankI : DEFINE+ BANK_SP_INFO " [SP]" : ENDIF
                IF 8 == bankI : DEFINE+ BANK_INFO BANK_PC_INFO,BANK_SP_INFO," (WARNING: kills NextZXOS browser data)" : ENDIF
                IF 5 == bankI : DEFINE+ BANK_INFO " ($4000..$7FFF)",BANK_PC_INFO,BANK_SP_INFO," (WARNING: kills NextZXOS sysvars)" : ENDIF
                IF 2 == bankI : DEFINE+ BANK_INFO " ($8000..$BFFF)",BANK_PC_INFO,BANK_SP_INFO : ENDIF
                IF {b head.ENTRYBANK} == bankI : DEFINE+ BANK_INFO " ($C000..$FFFF)",BANK_PC_INFO,BANK_SP_INFO : ENDIF
                DISPLAY "+ bank ",/D,bankI,BANK_INFO
            ENDIF
        ELSEIF -1 == bankRangeB     ; regular bank starting new range
bankRangeB = bankI
        ENDIF
bankI = bankI + 1
    EDUP
    IF missingMemRequired
        DISPLAY "WARNING: the file contains bank 48 or higher, but does not have \"require 2MB\" flag set"
    ENDIF
    IF {head.FILEHANDLERET}
        IF {head.FILEHANDLERET} < $4000
            DISPLAY "NEX file handle passed in BC"
        ELSE
            DISPLAY "NEX file handle written to: ",/A,{head.FILEHANDLERET}
        ENDIF
    ELSE
        DISPLAY "NEX file will be closed after load (no file handle passed)."
    ENDIF
