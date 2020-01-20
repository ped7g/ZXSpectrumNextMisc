;-------------------------------
; DISPLAYEDGE runtime library
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
;
; Runtime functions to read config file from system, detect video mode and figure out
; user's settings for current mode (or when mode did change)
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.14.0+)
;
; Define USE_TO_READ_NEXT_REG to your function to read NextReg (IN/OUT in A, preserve BC)
; or keep undefined to let this source add `dspedge.ReadNextReg` function.
;
; Changelist:
; v1    18/01/2020 P7G    Initial version (unfinished)
;-------------------------------
; # API (list of functions):
; DetectMode                   - returns dspedge.MODE_* value (current display mode)

    ; switch sjasmplus to correct syntax variant
    OPT push reset --zxnext --syntax=abfw

    IFDEF DISPLAYEDGE_ORG_ADR
        ORG DISPLAYEDGE_ORG_ADR
    ENDIF

    MODULE dspedge

MODE_HDMI_50        EQU         0
MODE_ZX48_50        EQU         1
MODE_ZX128_50       EQU         2
MODE_ZX128P3_50     EQU         3
MODE_HDMI_60        EQU         4
MODE_ZX48_60        EQU         5
MODE_ZX128_60       EQU         6
MODE_ZX128P3_60     EQU         7
MODE_PENTAGON       EQU         8   ; the board ignores 50/60Hz bit for Pentagon timing
MODE_COUNT          EQU         9

    STRUCT S_MARGINS        ; pixels of margin 0..31 (-1 = undefined margin)
L           BYTE    -1      ; left
R           BYTE    -1      ; right
T           BYTE    -1      ; top
B           BYTE    -1      ; bottom
    ENDS

    ; if no "USE_TO_READ_NEXT_REG" function was provided by source including this file, define own
    IFNDEF USE_TO_READ_NEXT_REG
        DEFINE USE_TO_READ_NEXT_REG @dspedge.ReadNextReg

ReadNextReg:
        ; reads nextreg in A into A
        ; Input:
        ;       A = nextreg to read
        ; Output:
        ;       A = value in nextreg
        ; Uses:
        ;       A, [currently selected NextReg on I/O port $243B]
                push    bc
                ld      bc, $243B   ; TBBLUE_REGISTER_SELECT_P_243B
                out     (c),a
                inc     b       ; bc = TBBLUE_REGISTER_ACCESS_P_253B
                in      a,(c)   ; read desired NextReg state
                pop     bc
                ret
    ENDIF

;;----------------------------------------------------------------------------------------
;; Detect current video mode:
;; 0, 1, 2, 3 = HDMI, ZX48, ZX128, ZX128+3 (all 50Hz), add +4 for 60Hz modes
;; 8 = Pentagon (board generates identical ~49Hz signal for 50/60Hz setting)

DetectMode:
        ; Output:
        ;       A = current mode 0..8
        ; Uses:
        ;       B, side effect: selects NextReg $11 or $03 on I/O port

                ;; read current configuration from NextRegs and convert it to 0..8 value
                ; read 50Hz/60Hz info
                ld      a,$05 ; PERIPHERAL_1_NR_05
                call    USE_TO_READ_NEXT_REG
                and     $04             ; bit 2 = 50Hz/60Hz configuration
                ld      b,a             ; remember the 50/60 as +0/+4 value in B
                ; read HDMI vs VGA info
                ld      a,$11 ; VIDEO_TIMING_NR_11
                call    USE_TO_READ_NEXT_REG
                inc     a               ; HDMI is value %111 in bits 2-0 -> zero it
                and     $07
                jr      z,.hdmiDetected
                ; if VGA mode, read particular zx48/zx128/pentagon setting
                ld      a,$03 ; MACHINE_TYPE_NR_03
                call    USE_TO_READ_NEXT_REG
                ; a = bits 6-4: %00x zx48, %010 zx128/+2, %011 zx128+2A/B,+3,Next, %100 pentagon
                bit     6,a
                jr      nz,.pentagonDetected
                swapnib a
                and     $03             ; A = (0|1)/2/3 for zx48/zx128/pentagon
                jr      nz,.hdmiDetected    ; 1/2/3 -> just add 50/60Hz and return
                inc     a               ; 0->1 to end as ZX48
.hdmiDetected:  add     a,b             ; add 50/60Hz value to final result
                ret
.pentagonDetected:
                ld      a,MODE_PENTAGON
                ret

defaultCfgFileName:
                DZ      "$/sys/displayedge.cfg"     ; zero terminated for esxDOS
                DB      32|128          ; bit7 terminated for UI of .displayedge tool

SanitizeMarginValue:
            ; sanitize the value in A
                test    -32
                ret     z               ; value OK
            ; value is 32..255 .. the 32..127 will become 31, 128..255 will become 0
                ld      a,31
                ret     p
                xor     a
                ret

;;----------------------------------------------------------------------------------------
;; Read and parse cfg file - use it before using API calls to get margin values for mode

ParseCfgFile:
        ; Input:
        ;       HL = filename of CFG file (zero terminated string for OS call)
        ;       DE = S_MARGINS[MODE_COUNT] array (4 * 9 = 36 bytes of memory to store results)
        ;       BC = 513 byte buffer for reading file content
        ; Output:
        ;       when Fc = 1
        ;       A = esxDOS error (fopen, fread or fclose failed)
        ;       The S_MARGINS array is always already initialized to [-1,-1,-1,-1]
        ;       when Fc = 0
        ;       The S_MARGINS array contains parsed values (-1 for missing mode/value)
        ;       (values are sanitized to valid [-1, 0..31] even if file says other)
        ; Uses:
        ;       AF, BC, DE, HL, IX

                ld      (.MarginsPtr),de
                ld      (.BufferPtr),bc
                push    hl
                push    hl
            ; initialize margins array to all -1
                ld      h,d
                ld      l,e
                ld      (hl),-1
                inc     de
                ld      bc,S_MARGINS * MODE_COUNT - 1
                ldir
            ; open the file - use both HL + IX for filename, to work as dot command or app
                pop     hl
                pop     ix
                ld      a,'*'           ; current drive (if not overriden in by fname)
                ld      b,$01           ; read-only
                rst     $08 : DB $9A    ; F_OPEN
                ret     c               ; F_OPEN failed, return with carry set + A=error
                ld      (.Fhandle),a
                ld      bc,0            ; zero bytes in buffer remaining
                ld      hl,(.BufferPtr)
                call    .parseNewLineLoop
            ; F_CLOSE the file
.Fhandle=$+1    ld      a,low .Fhandle  ; self-modify storage for handle
                rst     $08 : DB $9B    ; F_CLOSE
                ret

.parseNewLineLoop:
            ; BC = remaining chars, HL = current buffer
                inc     b
                djnz    .hasMoreThan255CharsInBuffer
            ; read more bytes into buffer to saturate it back to 512B
                ; first compact current buffer
                ld      de,(.BufferPtr)
                xor     a
                or      c
                jr      z,.reallyEmptyBuffer
                ldir
.reallyEmptyBuffer:
                ; calculate how many more to read to saturate buffer
.BufferPtr=$+1  ld      hl,.BufferPtr   ; self-modify storage of value
                inc     h
                inc     h
                sbc     hl,de           ; CF=0 from `or c`
                ld      bc,hl           ; fake BC=HL
                ex      de,hl           ; HL = after compacted buffer (to read to)
                push    hl
                pop     ix
                ld      a,(.Fhandle)
                rst     $08 : DB $9D    ; F_READ: A = file handle, HL+IX = address, BC = bytes to read
                ret     c               ; F_READ failed, return with carry set + A=error
                ; BC=DE=bytes read, HL+=BC
                    ;TODO debug real NextZXOS, CSpect does return HL+=original_BC, not actual!
                    ; workaround for CSpect bug code
                    add     ix,bc
                    push    ix
                    pop     hl
                ; original code expecting HL+=BC
                ld      (hl),0          ; set extra null terminator after last byte read
                ld      de,(.BufferPtr)
                sbc     hl,de           ; HL=bytes in buffer
                ex      de,hl           ; HL=buffer
                ld      bc,de           ; fake BC=DE (bytes in buffer)
                ; check if buffer was topped up to full 512B
                ld      a,b
                cp      2
                jr      z,.hasMoreThan255CharsInBuffer
                ; file did end already, fake long buffer to not do fread any more (and hit zero)
                ld      b,8
.hasMoreThan255CharsInBuffer:
                call    skipWhiteSpace
                ; check for known keywords 'hdmi, zx48, zx128, zx128p3, pentagon', else skipToEol
                call    isKeyword       ; ZF=0 no match, ZF=1 match, HL+BC points after, A=0..8 match number
                jr      nz,.skipToEol
                ld      e,a
                ; look for "assign" character
                call    skipWhiteSpace
                cp      '='
                jr      nz,.skipToEol
                inc     hl
                dec     bc
            ; prepare to parse values
                ld      d,S_MARGINS
                mul     de
.MarginsPtr=$+2 add     de,.MarginsPtr   ; self-modify storage for value
                        ; DE = address into margins array
                call    ParseFourValuesToDe
                ;  |
                ; fallthrough to .skipToEol (to skip rest of line)
                ;  |
.skipToEol:
                ld      a,(hl)
                or      a
                ret     z               ; null-terminated file or EOF
                inc     hl
                dec     bc
                cp      10
                jr      z,.skipEolItself
                cp      13
                jr      nz,.skipToEol
.skipEolItself:
            ; check if next char is also EOL, then keep reading+skipping
                ld      a,(hl)
                cp      13
                jr      z,.skipToEol
                cp      10
                jr      z,.skipToEol
                jr      .parseNewLineLoop

skipWhiteSpace_doSkip:
                inc     hl
                dec     bc
skipWhiteSpace:
                ld      a,(hl)
                cp      ' '         ; space char
                jr      z,skipWhiteSpace_doSkip
                cp      9           ; tab char
                jr      z,skipWhiteSpace_doSkip
                ret

ParseFourValuesToDe:
                call    .parseTwoValuesToDe
.parseTwoValuesToDe:
                call    .parseValueToDe
.parseValueToDe:
            ; skip any whitespace and single comma within it
                call    skipWhiteSpace
                cp      ','
                call    z,skipWhiteSpace_doSkip
                or      a
                ret     z           ; null terminator hit, abort everything
            ; parse the decimal digits into value
                push    de
                ld      e,0
                call    .parseDigits
                ld      a,e
                call    SanitizeMarginValue
                pop     de
                ld      (de),a
                inc     de
                ret
.parseDigits:
            ; check if next char is digit
                ld      a,(hl)
                cp      '0'
                ret     c
                cp      '9'+1
                ret     nc
            ; convert to value
                inc     hl
                dec     bc
                ld      d,10
                mul     de
                sub     '0'
                add     de,a
                jr      .parseDigits

isKeyword:
    ; Fz=0 no match, keeps HL+BC
    ; Fz=1 match, HL+BC points after, A=match number (0..N)
                ld      de,keywordsModes
                push    bc
                push    hl
                ld      bc,$0100    ; match flag + match number
.matchLoop:
                ld      a,(de)
                inc     de
                or      a
                jr      nz,.keywordContinues
                djnz    .wordMismatch
                ; word match, check end word boundary, can be anything <= 32 or '=' char
                ld      a,(hl)
                cp      ' '+1
                jr      c,.wordMatch
                cp      '='
                jr      nz,.wordMismatch
.wordMatch:
                pop     de
                or      a
                sbc     hl,de       ; HL = length of keyword
                ex      de,hl       ; DE = length of keyword, HL = original HL
                ex      (sp),hl
                sbc     hl,de       ; update original BC (in HL and stack)
                ex      (sp),hl
                add     hl,de       ; update original HL
                ld      a,c         ; A = match number
                pop     bc
                cp      a           ; Fz=1 to signal match
                ret
.wordMismatch:
                pop     hl          ; restore original HL
                ld      a,(de)
                or      a
                jr      nz,.moreKeywords
                pop     bc          ; restore original BC
                dec     a           ; Fz=0
                ret
.moreKeywords:
                push    hl
                inc     de
                inc     c
.keywordContinues:
                cp      (hl)
                jr      z,.charDoesMatch
                ld      b,2
.charDoesMatch:
                inc     hl
                jr      .matchLoop

keywordsModes:
                DZ      'hdmi_50'
                DZ      'zx48_50'
                DZ      'zx128_50'
                DZ      'zx128p3_50'
                DZ      'hdmi_60'
                DZ      'zx48_60'
                DZ      'zx128_60'
                DZ      'zx128p3_60'
                DZ      'pentagon'
                DB      0           ; end of keywords

    ENDMODULE

    OPT pop     ; restore original configuration of sjasmplus syntax
