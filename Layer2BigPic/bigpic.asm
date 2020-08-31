; "big pic" example, scrolling around 640x512 image in Layer2 320x256 mode
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
; requires ZX Spectrum Next with core3.1.5+
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.14.4+)
; Does use imagemagick "convert" tool to prepare image data for direct INCBIN
; The Makefile has the full-rebuild instructions

    OPT reset --zxnext --syntax=abfw
    DEVICE ZXSPECTRUMNEXT

    MMU 0 n, 9*2 : ORG 0    ; include bigpic data into 16ki banks 9-28 (8ki pages 18-57)
bigpic_top:
    INCBIN "bigpic_top.tga", 0x12 + 3*256, 256*640
bigpic_bot:
    INCBIN "bigpic_bot.tga", 0x12 + 3*256, 256*640
l2buf_top:      ; will land to bank 29-33 (5 banks needed for regular 320x256 layer2 image)
    ASSERT 29*2 == $$l2buf_top
    BLOCK   320*256, 0      ; top area buffer (80kiB) (zero filled = not saved to NEX)
l2buf_bot:      ; lands to banks 34-38
    ASSERT 34*2 == $$l2buf_bot
    BLOCK   320*256, 0      ; bottom area buffer (80kiB) (for simple HW scroll)
    ASSERT 39*2 == $$ && 0 == $     ; verify everything did place as expected

BORDER  MACRO   color?
            ld      a,color?
            out     (254),a
        ENDM

    ORG $E000
start:
        di
        nextreg 7,3             ; 28MHz    ; TURBO_CONTROL_NR_07
        call    ResetDma        ; resets DMA into known state (for simpler transfer init later)
        nextreg $12,$$l2buf_top/2   ; Layer 2 from bank where l2buf starts (29)             ; LAYER2_RAM_BANK_NR_12
        nextreg $70,%00'01'0000 ; 320x256x8 mode, palette offset +0                         ; LAYER2_CONTROL_NR_70
        nextreg $69,%1'0'000000 ; Layer 2 ON, ULA shadow off, Timex = 0                     ; DISPLAY_CONTROL_NR_69
        nextreg $71,0 ,, $16,0 ,, $17,0     ; layer2 X/Y offset = [+0, +0]                  ; LAYER2_XOFFSET_MSB_NR_71, LAYER2_XOFFSET_NR_16, LAYER2_YOFFSET_NR_17
        nextreg $1C,1 ,, $18,0 ,, $18,159 ,, $18,0 ,, $18,255   ; layer2 clip 320x256       ; CLIP_WINDOW_CONTROL_NR_1C, CLIP_LAYER2_NR_18
        nextreg $64,33          ; offset video-line numbering 33 lines up into top border   ; VIDEO_LINE_OFFSET_NR_64
            ; scanlines 1..32 = top BORDER, 33..224 = PAPER, 225..256 = bottom BORDER

    ; copy the initial image data for offset [+0,+0] from the source memory - top part
        ld      a,$$l2buf_top
        ex      af,af'
        ld      a,$$bigpic_top
        call    Copy80k
    ; copy bottom part (offset [+0, +256]) from source memory
        ld      a,$$l2buf_bot
        ex      af,af'
        ld      a,$$bigpic_bot
        call    Copy80k
    ; set up the copper (will be used for vertical scrolling)
        call    InitCopper

    ; now do the scrolling 0..639,0..511 (640x512 big image)
BounceScrollLoop:
        BORDER  0
        call    WaitForScanlineUnderUla
        BORDER  3
        call    ScrollVertical
        BORDER  4
        call    ScrollHorizontal
        jr      BounceScrollLoop

scrollXpos  dw      0
scrollYpos  dw      0
deltaX      dw      1
deltaY      dw      1

highAddrToPage:
    ; IN: HL = 000P PPPP PPPo oooo (P = 8ki page, O = offset/256) (top 16b of 24b address)
    ; OUT: A = PPPP PPPP, HL = 000o oooo 0000 0000
        ld      a,l
        ex      af,af
        ld      a,l
        and     $1f
        ld      l,a
        ex      af,af
        xor     l
        or      h
        rlca
        rlca
        rlca            ; A = PPPP PPPP (18..57 actively used for source data)
        ld      h,l
        ld      l,0
        ret

ScrollHorizontal:
    ; draw deltaX new columns into Layer2 VRAM (at current scrollXpos)
        ld      hl,(scrollXpos)
        ld      de,(deltaX)
        ld      a,e
        or      a
        ret     z       ; no scrolling (deltaX == 0)
        jp      p,.positiveDeltaX   ; for positive deltaX the column to overdraw is Xpos
        add     hl,de   ; for negative deltaX the column to overdraw is Xpos+deltaX
        add     hl,-320 ; and for negative deltaX the source of data is left side
        ld      d,10    ; for source data -> l2buffer page calculation D=10 (deltaX<0) or 0 (deltaX>0)
        neg             ; A=abs(deltaX)
.positiveDeltaX:
        ld      b,a     ; number of columns to copy at columns [HL .. HL+(b-1)]
        ld      c,0     ; BC = column*256 = amount of bytes to copy
        add     hl,$$bigpic_top*32+320  ; add page of first column on right side (source data)
        ; here HL = addressOfSourceColumn/256 (8kiB page 18..37 in top 11 bits, offset in bottom 5 bits)
        call    highAddrToPage
        ; map 0000..3FFF address space to "top" source data 8kiB pages (HL is already address)
        nextreg $50,a
        inc     a
        nextreg $51,a
        ; calculate source pages number for "bottom" source data and store it
        add     a,$$bigpic_bot-$$bigpic_top-1
        ld      (.bottomSrcPage),a
        ; map 4000..7FFF address space to l2buffer "top" page
        add     a,$$l2buf_top-($$bigpic_bot+10)
        add     a,d
        nextreg $52,a
        inc     a
        nextreg $53,a
        add     a,10-1
        ld      (.bottomDstPage),a
        call    CopyColumns
        BORDER  5
        ; remap the memory for "bottom" parts and do the same copy again
.bottomSrcPage=$+1: ld  a,0
        nextreg $50,a
        inc     a
        nextreg $51,a
.bottomDstPage=$+1: ld  a,0
        nextreg $52,a
        inc     a
        nextreg $53,a
        call    CopyColumns
    ; scroll the scrollXpos value from 0 to 320 and back
        ld      hl,(scrollXpos)
        ld      de,(deltaX)
        add     hl,de
        ld      (scrollXpos),hl
        ; flip delta at HL==0 {0,0} or HL==320 {1,64}
        ld      a,h
        rrca
        rrca            ; A = 64 (if H==1) or 0 (if H==0)
        xor     l       ; A == 0 when HL==0 or HL==320
        jr      nz,.notAtEdgeYet
    ; set Layer2 X-offset next registers to 0 (for both 0 and 320 Xpos)
        nextreg $16,a   ; this is done here with zero
        nextreg $71,a   ; because setting $71+$16 to "320" (64, 1) is not valid
        ld      a,d
        cpl
        ld      d,a
        ld      a,e
        neg
        ld      e,a
        ld      (deltaX),de
        ret
.notAtEdgeYet:
    ; set Layer2 X-offset next registers to L, H (1..319 X-offset value)
        ld      a,l
        nextreg $16,a
        ld      a,h
        nextreg $71,a
        ret

CopyColumns:
    ; H source address MSB, B bytes length MSB
    ; quick-init DMA to copy "B*256" bytes from address "H*256" to address "(H+$40)*256"
        ld      c,$6B       ; ZXN_DMA_P_6B I/O port
        ld      de,%0'1010'1'01'1'01'0'10'01    ; D = WR0, E = WR4
        out     (c),d       ; WR0: source address and length high-bytes only
        out     (c),h       ; source address MSB
        out     (c),b       ; length MSB
        out     (c),e       ; WR4: destination address high-byte
        set     6,h         ; convert the $00..3F source address into $40..7F destination
        out     (c),h       ; destination address MSB
        ld      de,$CF87    ; D = DMA_LOAD, E = DMA_ENABLE
        out     (c),d       ; DMA_LOAD = load new values for transfer from WR regs
        out     (c),e       ; DMA_ENABLE = execute the transfer
        res     6,h         ; restore H back to original value (will be used twice)
        ret

ScrollVertical:
    ; scroll the scrollYpos value from 0 to 256 and back
        ld      hl,(scrollYpos)
        ld      de,(deltaY)
        add     hl,de
        ld      (scrollYpos),hl
        ; if L == 0, flip deltaY (+1 <-> -1)
        ld      a,l
        or      a
        jr      nz,.notAtEdgeYet
        ld      a,d
        cpl
        ld      d,a
        ld      a,e
        neg
        ld      e,a
        ld      (deltaY),de
.notAtEdgeYet:
    ; modify the copper template code to use correct Ypos values
    ; modify the C_MOVE $17 instruction
        ld      de,(scrollYpos)
        nextreg $61,CopperCodeTemplate.modifyYpos - CopperCodeTemplate      ; COPPER_CONTROL_LO_NR_61
        ld      a,e
        nextreg $60,a       ; result: "C_MOVE $17, scrollYpos & 0xFF"       ; COPPER_DATA_NR_60
    ; modify the C_WAIT instruction to C_WAIT(line=(256 - y_scroll), h=38)
        nextreg $61,CopperCodeTemplate.modifyWait - CopperCodeTemplate      ; COPPER_CONTROL_LO_NR_61
        ; WAIT(line=(256 - y_scroll), h=38)
        ld      hl,($8000 | (38<<9)) + 256
        sub     hl,de       ; fake instruction (or a : sbc hl,de)
        ld      a,h
        nextreg $63,a       ; using 16b copper write here for whole WAIT    ; COPPER_DATA_16B_NR_63
        ld      a,l
        nextreg $63,a                                                       ; COPPER_DATA_16B_NR_63
        ret

C_WAIT  MACRO   line?, horizontal?
            DB      $80 + ((horizontal?<<1) & $7E) + ((line?>>8) & $01)
            DB      line? & $FF
        ENDM

C_MOVE  MACRO   reg?, value?
            DB      reg?, value?
        ENDM

        DEFINE  C_HALT C_WAIT $1FF, $FF

CopperCodeTemplate:
        C_WAIT  0, 0        ; C_WAIT(line=0, h=0)
.modifyYpos: equ $+1
        C_MOVE  $17, 0      ; the value is modified to (ypos&0xFF), $17 is Layer2 Yoffset   ; LAYER2_YOFFSET_NR_17
        C_MOVE  $12, $$l2buf_top/2      ; $12 is first bank of visible Layer2 (top image)   ; LAYER2_RAM_BANK_NR_12
.modifyWait:
        C_WAIT  0, 0        ; this is modified to C_WAIT(line=(256-ypos), h=38)
        C_MOVE  $12, $$l2buf_bot/2      ; set bank to bottom image                          ; LAYER2_RAM_BANK_NR_12
        C_HALT
.end:

InitCopper:
        nextreg $61,0       ; reset low 8b copper index                 ; COPPER_CONTROL_LO_NR_61
        nextreg $62,0       ; reset high 8b copper index + stop copper  ; COPPER_CONTROL_HI_NR_62
    ; select copper write 16b nextreg $63
        ld      bc,$243B    ; TBBLUE_REGISTER_SELECT_P_243B
        ld      a,$63       ; COPPER_DATA_16B_NR_63
        out     (c),a
    ; write the copper code template into copper memory (to be modified by scroll code)
        inc     b           ; BC = TBBLUE_REGISTER_ACCESS_P_253B
        ld      hl,CopperCodeTemplate
        ld      a,CopperCodeTemplate.end - CopperCodeTemplate
.uploadTemplateCode:
        outinb              ; out (c),(hl) : inc hl (but preserves B)
        dec     a           ; loop counter (B is used for I/O port number)
        jr      nz,.uploadTemplateCode
    ; start the copper (in the "restart at scanline 0 mode")
        nextreg $62,%11'000'000     ; COPPER_CONTROL_HI_NR_62
        ret

WaitForScanlineUnderUla:    ; wait for scanline 257 (just below 320x256 area)
    ; read NextReg $1E - MSB of current video line
        ld      bc,$243B    ; TBBLUE_REGISTER_SELECT_P_243B
        ld      a,$1E       ; VIDEO_LINE_MSB_NR_1E
        out     (c),a       ; select NextReg $1E
        inc     b           ; BC = TBBLUE_REGISTER_ACCESS_P_253B
    ; if still in scanlines 256+, then wait whole frame until scanline 0 is reached
.skipOldFrameEnd:
        in      a,(c)
        rra                 ; check just top bit of MSB
        jr      c,.skipOldFrameEnd
    ; wait till 256+ scanlines (MSB = 1)
.waitFor256line:
        in      a,(c)
        rra                 ; check just top bit of MSB
        jr      nc,.waitFor256line
    ; wait till line 257 (LSB == 1)
        dec     b           ; BC = TBBLUE_REGISTER_SELECT_P_243B
        ld      a,$1F       ; VIDEO_LINE_LSB_NR_1F
        out     (c),a       ; select NextReg $1F
        inc     b           ; BC = TBBLUE_REGISTER_ACCESS_P_253B
.waitLoop:
        in      a,(c)       ; read the video line LSB
        dec     a
        jr      nz,.waitLoop    ; wait for LSB == 1
        ret

Copy8k:
    ; A has read page, A' has write page -> copy will increment them both by +1
        nextreg $54,a
        inc     a
        ex      af,af'
        nextreg $55,a
        inc     a
        ex      af,af'
        ld      hl,$8000 ,, de,$A000 ,, bc,$2000
        ldir
        ret

Copy80k:
    ; A has read page, A' has write page, do 10 loops of 8kiB copy = 80kiB
        ld      b,10
.loopTenTimes:
        push    bc
        call    Copy8k
        pop     bc
        djnz    .loopTenTimes
        ret

ResetDma:
        ; init DMA - full re-init of everything
        ld      hl,DmaFullInit
        ld      bc,(DmaFullInitSz<<8)|$6B   ; ZXN_DMA_P_6B
        otir
        ret

DmaFullInit:
    BLOCK 6, $C3            ; 6x DMA_RESET (to get out of any regular state, if 5B data are expected)
    DB  %0'1111'1'01        ; WR0 = A->B transfer (source addres = length = 0)
    DW  0, 0                ; source address, length
    DB  %0'1'01'0'100, %10  ; WR1 = A memory, ++, cycle length=2
    DB  %0'1'01'0'000, %00'1'000'10, 0  ; WR2 = B memory, ++, cycle length=2, prescalar=0
    DB  %1'01'0'11'01       ; WR4 = continuous mode + destination address = 0
    DW  0                   ; destination address
    DB  %10'0'0'0010        ; WR5 = stop after block, /CE only
DmaFullInitSz EQU $ - DmaFullInit


    SAVENEX OPEN "bigpic.nex", start, $FF00, 0, 2 : SAVENEX CORE 3,1,5 : SAVENEX CFG 0
    SAVENEX AUTO : SAVENEX CLOSE
    CSPECTMAP "bigpic.map"
