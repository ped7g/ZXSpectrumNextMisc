;; NextZXOS dot command to test functionality of GetZ80NOpcodeSize
;;
;; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT

;     DEFINE CLASSIC_Z80  ; uncomment or define from command line to get classic Z80 variant
;     DEFINE snafile      ; uncomment or define from command line to get .sna test file

    ; switch sjasmplus to correct syntax variant
    OPT reset --syntax=abfw
    IFNDEF CLASSIC_Z80
        OPT --zxnext
    ENDIF

    IFNDEF snafile
        ; dot command is raw binary targetting $2000
        ORG     $2000
    ELSE
        ; snapshot file does uset ZX48 snapshot to make testing easier in any emulator
        DEVICE ZXSPECTRUM48, $5FFF
        ORG     $8000
    ENDIF

start:
    ; test regular instructions (no prefix, or prefix + 00)
        ld      ix,expectedSize_regular
        ld      hl,machineCode
.testRegularLoop:
        call    GetZ80NOpcodeSize
        cp      (ix)
        jp      nz,wrongValueDetected
        ld      de,machineCode
        sbc     hl,de
        jp      nz,modifiedHlDetected
        ex      de,hl   ; restore HL to machineCode
        inc     ix      ; advance IX (expected value address)
        inc     (hl)
        jr      nz,.testRegularLoop

    ; test ED prefix
        ld      ix,expectedSize_ED_prefix
        ld      hl,machineCode
        ld      (hl),$ED
        inc     hl
        ld      (hl),$00
        call    testOneBytePrefix

    ; test DD (IX) prefix
        ld      ix,expectedSize_DD_prefix
        ld      hl,machineCode
        ld      (hl),$DD
        inc     hl
        ld      (hl),$00
        call    testOneBytePrefix

    ; test FD (IY) prefix
        ld      ix,expectedSize_DD_prefix   ; same as FD
        ld      hl,machineCode
        ld      (hl),$FD
        inc     hl
        ld      (hl),$00
        call    testOneBytePrefix

    ; test CB prefix (bit instructions)
        ld      hl,$02FE
        ld      (testOneBytePrefix.cp),hl   ; modify `cp (ix+0)` to `cp 2 : nop`
        ld      hl,machineCode
        ld      (hl),$CB
        inc     hl
        ld      (hl),$00
        call    testOneBytePrefix

    ; test DDCB prefix (IX bit instructions)
        ld      hl,machineCode
        ld      (hl),$DD
        inc     hl
        ld      (hl),$CB
        inc     hl
        ld      (hl),$00
        ld      c,4         ; expected length for all of them is 4
        call    testTwoBytePrefix

    ; test FDCB prefix (IY bit instructions)
        ld      hl,machineCode
        ld      (hl),$FD
        inc     hl
        ld      (hl),$CB
        inc     hl
        ld      (hl),$00
        ld      c,4         ; expected length for all of them is 4
        call    testTwoBytePrefix

    ; test DD/FD prefix chains
        ld      hl,chainOfDdPrefix
        call    testChainedPrefix
        ld      hl,chainOfDdFdPrefix
        call    testChainedPrefix
        ld      hl,chainOfFdPrefix
        call    testChainedPrefix

    ; green border to signal everything went OK
        ld      a,4
        out     (254),a
        ret

modifiedHlDetected:
        add     hl,de   ; restore HL back to the wrong value
        ; TODO all - currently just continues into wrongValueDetected
wrongValueDetected:
        push    af
        ld      a,2
        out     (254),a
        pop     af
        ; TODO all - currently just red border and freeze
        jr      $

testOneBytePrefix:
    ; HL = machineCode+1 (after the prefix)
    ; IX = expected values table
        dec     hl
        call    GetZ80NOpcodeSize
.cp:    cp      (ix)
        jr      nz,wrongValueDetected
        ld      de,machineCode
        sbc     hl,de
        jr      nz,modifiedHlDetected
        ex      de,hl   ; restore HL to machineCode
        inc     ix      ; advance IX (expected value address)
        inc     hl      ; affect second byte of opcode
        inc     (hl)
        jr      nz,testOneBytePrefix
        ret

testTwoBytePrefix:
    ; HL = machineCode+2 (after the prefixes)
    ; C = expected values table
        dec     hl
        dec     hl
        call    GetZ80NOpcodeSize
        cp      c
        jr      nz,wrongValueDetected
        ld      de,machineCode
        sbc     hl,de
        jr      nz,modifiedHlDetected
        ex      de,hl   ; restore HL to machineCode
        inc     hl
        inc     hl      ; affect third byte of opcode
        inc     (hl)
        jr      nz,testTwoBytePrefix
        ret

testChainedPrefix:
    ; HL = chain of prefix bytes (FD or DD) (should be 8 bytes initially)
        ld      b,3     ; do at least three attempts (incrementing HL)
.testLoop:
        call    GetZ80NOpcodeSize
        cp      1
        jr      nz,wrongValueDetected
        inc     hl
        djnz    .testLoop
        ret

__GetZ80NOpcodeSize_INCLUDE_START:
    INCLUDE "Z80N_instructions_size.asm"
;GetZ80NOpcodeSize:
;; returns size of opcode in memory in bytes
;; In:  HL = address of opcode
;; Out: A = opcode size
;; Modifies: DE
__GetZ80NOpcodeSize_INCLUDE_END:

machineCode:
        BLOCK   5, 0

chainOfDdPrefix:
        HEX     DD DD DD DD DD DD DD DD
chainOfDdFdPrefix:
        HEX     DD FD DD FD DD FD DD FD
chainOfFdPrefix:
        HEX     FD FD FD FD FD FD FD FD

expectedSize_regular:
        ;  x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF - regular instructions
        DB  1, 3, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1  ; 0x
        DB  2, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1  ; 1x
        DB  2, 3, 3, 1, 1, 1, 2, 1, 2, 1, 3, 1, 1, 1, 2, 1  ; 2x
        DB  2, 3, 3, 1, 1, 1, 2, 1, 2, 1, 3, 1, 1, 1, 2, 1  ; 3x
        DB  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  ; 4x
        DB  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  ; 5x
        DB  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  ; 6x
        DB  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  ; 7x
        DB  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  ; 8x
        DB  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  ; 9x
        DB  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  ; Ax
        DB  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  ; Bx
        DB  1, 1, 3, 3, 3, 1, 2, 1, 1, 1, 3, 2, 3, 3, 2, 1  ; Cx
        DB  1, 1, 3, 2, 3, 1, 2, 1, 1, 1, 3, 2, 3, 2, 2, 1  ; Dx
        DB  1, 1, 3, 1, 3, 1, 2, 1, 1, 1, 3, 1, 3, 2, 2, 1  ; Ex
        DB  1, 1, 3, 1, 3, 1, 2, 1, 1, 1, 3, 1, 3, 2, 2, 1  ; Fx

expectedSize_ED_prefix:
        ;  x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF - ED prefix - Z80 / Z80N version by define CLASSIC_Z80
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; 0x
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; 1x
    IFDEF CLASSIC_Z80
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; 2x - classic Z80
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; 3x - classic Z80
    ELSE
        DB  2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2  ; 2x - Z80N extensions
        DB  2, 2, 2, 2, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; 3x - Z80N extensions
    ENDIF
        DB  2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2  ; 4x
        DB  2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2  ; 5x
        DB  2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2  ; 6x
        DB  2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2  ; 7x
    IFDEF CLASSIC_Z80
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; 8x - classic Z80
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; 9x - classic Z80
    ELSE
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2  ; 8x - Z80N extensions
        DB  2, 4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; 9x - Z80N extensions
    ENDIF
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; Ax
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; Bx
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; Cx
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; Dx
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; Ex
        DB  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  ; Fx

expectedSize_DD_prefix:
        ;  x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF - DD/FD (IX/IY) prefix
        DB  2, 4, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2  ; 0x
        DB  3, 4, 2, 2, 2, 2, 3, 2, 3, 2, 2, 2, 2, 2, 3, 2  ; 1x
        DB  3, 4, 4, 2, 2, 2, 3, 2, 3, 2, 4, 2, 2, 2, 3, 2  ; 2x
        DB  3, 4, 4, 2, 3, 3, 4, 2, 3, 2, 4, 2, 2, 2, 3, 2  ; 3x
        DB  2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2  ; 4x
        DB  2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2  ; 5x
        DB  2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2  ; 6x
        DB  3, 3, 3, 3, 3, 3, 2, 3, 2, 2, 2, 2, 2, 2, 3, 2  ; 7x
        DB  2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2  ; 8x
        DB  2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2  ; 9x
        DB  2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2  ; Ax
        DB  2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2  ; Bx
        DB  2, 2, 4, 4, 4, 2, 3, 2, 2, 2, 4, 4, 4, 4, 3, 2  ; Cx
        DB  2, 2, 4, 3, 4, 2, 3, 2, 2, 2, 4, 3, 4, 1, 3, 2  ; Dx
        DB  2, 2, 4, 2, 4, 2, 3, 2, 2, 2, 4, 2, 4, 1, 3, 2  ; Ex
        DB  2, 2, 4, 2, 4, 2, 3, 2, 2, 2, 4, 2, 4, 1, 3, 2  ; Fx

    IFDEF snafile
sna_start:
        call    start
        jr      $

        IFDEF CLASSIC_Z80
            SAVESNA "test_z80.sna", sna_start
        ELSE
            SAVESNA "test.sna", sna_start
        ENDIF
        DISPLAY "code size of GetZ80NOpcodeSize include: ", /D, __GetZ80NOpcodeSize_INCLUDE_END-__GetZ80NOpcodeSize_INCLUDE_START
    ENDIF
