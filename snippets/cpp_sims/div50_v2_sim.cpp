#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cassert>

/*
 * "v2" routine based on original work of jcardoso, making it a bit more precise and faster
 * simulating routine (fixed point 8.8 x 8.8, so four MUL instructions):
        ld      c,e
        ld      b,d                     ; keep a copy of de
        ld      h,d
        ld      d,31                    ; ~0.12 (0.12109375) in 8.8  = %00011111
        ld      l,d                     ; hl = in.h, 0.12
        mul     de                      ; de = in.l * 0.12 * 256
        ld      a,d
        ex      de,hl
        mul     de                      ; de = in.h * 0.12
        add     de,a
        ex      de,hl                   ; hl = in * 0.12 (max value 7935 (255*31+30))
        ld      e,c
        ld      d,5
        mul     de                      ; de = in.l * 5 (max value 1275)
        add     hl,de                   ; hl = in.l * 5 + in * 0.12 (can't overflow, max: 1275+7935)
        ld      a,h
        ld      e,b
        ld      d,5
        mul     de                      ; de = in.h * 5
        add     de,a                    ; de = (in * 5.12)>>8
        ret     ; DE out, not HL
        ;=4+4+4+7+4+8 +4+4+8 +8+4+4+7+8 +11+4+4+7+8 +8+10 = 130T
*/

constexpr uint16_t div50_v2_mul = 0x051F;   // hi 5, low 31, ie. 5.12109375 (0x051E is 5.1171875)

uint16_t div50_fp88_fp88_v2(const uint16_t in) {
    const uint8_t in_l = in & 0xFF;
    const uint8_t in_h = in >> 8;
    const uint16_t m1 = in_l * (div50_v2_mul & 0xFF);
    const uint16_t m2 = in_h * (div50_v2_mul & 0xFF);
    const uint16_t m3 = in_l * (div50_v2_mul >> 8);
    const uint16_t m4 = in_h * (div50_v2_mul >> 8);
    int32_t r = m2 + (m1 >> 8);         // in * 0.12
    r += m3;
    assert(0 <= r && r <= 0xFFFF);      // this must not overflow, the asm code assumes the result will fit into 16b
    r = m4 + (r >> 8);                  // (in * 5.12) >> 8
    return r;
}

int main() {
    uint16_t i = 0;
    int sumerr_v2 = 0;
    // table header if the output is used as CSV file
    printf("input i,as fp8.8,div50_v2,as fp8.8,e,r,r,o,r,,i,n,,r,e,s,u,l,t");
    do {
        const uint16_t expected = i / 50;
        const uint16_t div50_v2 = div50_fp88_fp88_v2(i);
        // output new-line, current `i` and div50 result every 64 values
        if (0 == (i&0x3F)) printf("\n%d,%3.2f,%d,%1.5f,", i, i / 256.0, div50_v2, div50_v2 / 256.0);
        const int err = int(div50_v2) - int(expected);
        printf("%d,", err);
        sumerr_v2 += abs(err);          // update cumulative error sum with absolute error
    } while (++i);  // do all 64ki inputs
    printf("\nabserror_v2,%d,avg. err,%1.5f\n", sumerr_v2, sumerr_v2 / 65536.0);
}
