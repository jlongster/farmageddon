#include <stdio.h>
#include <stdlib.h>

typedef struct _pvrheader {
	int headerLength;
	int height;
	int width;
	int numMipmaps;
	int flags;
	int dataLength;
	int bpp;
	int bitmaskRed;
	int bitmaskGreen;
	int bitmaskBlue;
	int bitmaskAlpha;
	int pvrTag;
	int numSurfs;
} pvrheader;

int main(int argc, char** argv) {
    FILE *f = fopen(argv[1], "r");
    pvrheader *header = (pvrheader*)malloc(sizeof(pvrheader));
    
    

    printf("%s %d %d",
           (char*)&header->pvrTag,
           header->width,
           header->height);
    
    return 0;
}
