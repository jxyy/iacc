#include<stdio.h>

extern unsigned int scheme_entry();

#define INT_TAG_WIDTH 2
#define INT_TAG 0x0
#define BOOL_TAG_WIDTH 6
#define BOOL_TAG 0x2f
#define EMPTY_LIST_TAG_WIDTH 8
#define EMPTY_LIST_TAG 0x3f
#define CHAR_TAG_WIDTH 8
#define CHAR_TAG 0x0f

typedef unsigned int scheme_value;

void print(scheme_value value) {
    scheme_value tag2 = value & 0x3;
    switch(tag2) {
        case INT_TAG:
            printf("%d\n", ((int)value) >> INT_TAG_WIDTH);
            return;
    }

    scheme_value tag6 = value & 0x3f;
    switch(tag6) {
        case BOOL_TAG:
            if(value >> BOOL_TAG_WIDTH)
                printf("#t\n");
            else
                printf("#f\n");
            return;
    }

    scheme_value tag8 = value & 0xff;
    switch(tag8) {
        case EMPTY_LIST_TAG:
            printf("()\n");
            return;
        case CHAR_TAG:
            ;
            char c = (value >> CHAR_TAG_WIDTH);
            switch(c) {
                case '\t':
                    printf("#\\tab\n");
                    break;
                case 0x0a:
                    printf("#\\newline\n");
                    break;
                case 0x0d:
                    printf("#\\return\n");
                    break;
                case ' ':
                    printf("#\\space\n");
                    break;
                default:
                    printf("#\\%c\n", c);
                    break;
            }
            return;
    }
    printf("#<unknown 0x%08x>", value);
}

int main(int argc, char** argv) {
    //printf("%d\n", scheme_entry());
    print(scheme_entry());
    return 0;
}
