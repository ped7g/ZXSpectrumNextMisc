// to build and run this test:
//   cc Z80N_instructions_size.c test_Z80sizes.c && ./a.out

#include <stdint.h>
#include <stdio.h>

typedef uint8_t byte;

byte opcodeSz(byte *b);

int main(void) {
    int i, j;
    byte testOpcode[5] = { 0, 0, 0, 0, 0 };
    // label header - regular instructions
    printf("   ");
    for (j = 0; j < 16; ++j) printf(" x%X", j);
    printf(" - regular instructions\n");
    // table content
    for (i = 0; i < 16; ++i) {
        printf("%Xx", i);
        for (j = 0; j < 16; ++j) {
            printf("  %d", opcodeSz(testOpcode));
            ++testOpcode[0];
        }
        printf("\n");
    }
    // label header - ED prefix
    printf("\n   ");
    for (j = 0; j < 16; ++j) printf(" x%X", j);
    printf(" - ED prefix - Z80N version\n");
    testOpcode[0] = 0xED;
    // table content
    for (i = 0; i < 16; ++i) {
        printf("%Xx", i);
        for (j = 0; j < 16; ++j) {
            printf("  %d", opcodeSz(testOpcode));
            ++testOpcode[1];
        }
        printf("\n");
    }
    // label header - DD (IX) prefix
    printf("\n   ");
    for (j = 0; j < 16; ++j) printf(" x%X", j);
    printf(" - DD (IX) prefix\n");
    testOpcode[0] = 0xDD;   testOpcode[1] = 0;
    // table content
    for (i = 0; i < 16; ++i) {
        printf("%Xx", i);
        for (j = 0; j < 16; ++j) {
            printf("  %d", opcodeSz(testOpcode));
            ++testOpcode[1];
        }
        printf("\n");
    }
    // testing specials
    byte testOpcode2[] = { 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD };
    printf("\nField of 0xDD: %d\n", opcodeSz(testOpcode2));
    return 0;
}
