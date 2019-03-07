#include<stdio.h>
#include<sys/mman.h>
#include<unistd.h>

extern unsigned int scheme_entry(char* stack_index);

#define INT_TAG_WIDTH 2
#define INT_TAG 0x0
#define BOOL_TAG_WIDTH 6
#define BOOL_TAG 0x2f
#define EMPTY_LIST_TAG_WIDTH 8
#define EMPTY_LIST_TAG 0x3f
#define CHAR_TAG_WIDTH 8
#define CHAR_TAG 0x0f

typedef unsigned int scheme_value;

static char* allocate_protected_space(int size){
	int page = getpagesize();
	int aligned_size = ((size + page - 1) / page) * page;
	char* p = mmap(0, aligned_size + 2 * page,
					PROT_READ | PROT_WRITE,
					MAP_ANONYMOUS | MAP_PRIVATE,
					0, 0);
	if (p == MAP_FAILED){
        printf("allocate space failed");
		return NULL;
	}
	int status = mprotect(p, page, PROT_NONE);
	if(status != 0){
        printf("make protected page failed: %d", status);
		return NULL;
	}
	status = mprotect(p + page + aligned_size, page, PROT_NONE);
	if(status != 0){
        printf("make protected page failed: %d", status);
		return NULL;
	}
	return (p + page);
}

static void deallocate_protected_space(char* p, int size){
	int page = getpagesize();
	int aligned_size = ((size + page - 1) / page) * page;
	int status = munmap(p - page, aligned_size + 2 * page);
	if(status != 0){
		printf("deallocate space failed: %d", status);
	}
}

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
    int stack_size = 16 * 4096;
	char* stack_top = allocate_protected_space(stack_size);
    char* stack_base = stack_top + stack_size;
    if (stack_top == NULL) {
        return 0;
    }
    print(scheme_entry(stack_base));
    deallocate_protected_space(stack_top, stack_size);
    return 0;
}
