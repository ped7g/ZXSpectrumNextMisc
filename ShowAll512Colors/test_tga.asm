    DEVICE ZXSPECTRUM48
    ORG 0
/*
 marching the cube...
  |G
  |
  |_____ B
  /
 /
/R

000
001 100 010
002 101 200 110 011 020
003 102 201 300 210 111 012 021 120 030
004 ... 040
005 ... 050
006 ... 060
007 ... 070
107 206 305 404 503 602 701 ! 710 611 512 413 314 215 116 017 ! 026 ...
207

707 ! 716 617 ! 527 626 725 ! 734 635 536 437 ! 347 ...
717 ! 726 627 ! 537 636 735 ! 744 645 546 447 ! ...

757 ! 766 667 ! 577 676 775
767 ! 776 677
777
*/

n=0
vb=0
vr=0
vg=0
dir=0   ; 0 -> from B toward R, !0 -> from R toward B (G is always last resort)
    DUP 512
        DB vr, vg, vb
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

    OUTPUT "test512.tga"
    ; TGA header: ID length 0, colormap 0=none, image type 2 (uncompressed true color)
    ; colormap specs 5x00
    HEX 00 00 02 0000000000
    ; image spec: x0, y0, width, height (4x word)
    DW 0, 0, 4*32, 4*16
    ; image spec: bits per pixel, alpha+direction (2x byte)
    DB 24, 0
    ; pixels ($$RRGGBB) (upside down image!)
    OPT push listoff
rowN=31
    DUP 16
        DUP 4
n=rowN
            DUP 32
                .4 DB {b n*3+2}*255/7, {b n*3+1}*255/7, {b n*3+0}*255/7
n=n-1
            EDUP
        EDUP
rowN=rowN+32
    EDUP
    OPT pop
    OUTEND
