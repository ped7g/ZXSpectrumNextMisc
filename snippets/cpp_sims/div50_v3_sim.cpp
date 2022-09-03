#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cassert>

/*
 * "v3" is idea to do fixed point 8.8 x 3.5 multiplication, having only two MUL instructions.
 * simulating routine (fixed point 8.8 x 3.5):
        ld      h,d
        ld      d,%101'00100            ; 5.125 in fixed point 3:5 form
        ld      l,d
        mul     de
        ex      de,hl
        mul     de
        ld      a,h
        add     de,a                    ; DE = top 16 bits of (DE * 5.125) in fixed point 11:13
        ld      b,5
        bsrl    de,b                    ; shift DE to become (x * 5.125 / 256) in fixed point 8:8
        ret
*/

static constexpr uint16_t div50_v3_mul = 0b101'00100;  // value 5.125 encoded in fp3.5

static uint16_t div50_v3(const uint16_t in) {
    const uint8_t in_l = in & 0xFF;
    const uint8_t in_h = in >> 8;
    const uint16_t m1 = in_l * div50_v3_mul;
    const uint16_t m2 = in_h * div50_v3_mul;
    int32_t r = m2 + (m1 >> 8);         // in * 5.125 >> 3
    assert(0 <= r && r <= 0xFFFF);      // this must not overflow, the asm code assumes the result will fit into 16b
    r = r >> 5;                         // (in * 5.125) >> 8 == in / 49.95121951219512
    return r;
}

int main() {
    uint16_t i = 0;
    int sumerr_v3 = 0;
    // table header if the output is used as CSV file
    printf("input i,as fp8.8,div50_v3,as fp8.8,e,r,r,o,r,,i,n,,r,e,s,u,l,t");
    do {
        const uint16_t expected = i / 50;
        const uint16_t result_v3 = div50_v3(i);
        // output new-line, current `i` and div50 result every 64 values
        if (0 == (i&0x3F)) printf("\n%d,%3.2f,%d,%1.5f,", i, i / 256.0, result_v3, result_v3 / 256.0);
        const int err = int(result_v3) - int(expected);
        printf("%d,", err);
        sumerr_v3 += abs(err);          // update cumulative error sum with absolute error
    } while (++i);  // do all 64ki inputs
    printf("\nabserror_v3,%d,avg. err,%1.5f\n", sumerr_v3, sumerr_v3 / 65536.0);
}
