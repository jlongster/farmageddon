
(include "image#.scm")
(c-declare "#include <OpenGLES/ES1/glext.h>")

;; CGImage (PNG loading)

(c-declare #<<end-c-code

 CGImageRef CGImageRef_load(const char *filename) {
    NSString *path = [NSString stringWithFormat:@"%s", filename];
    UIImage *img = [UIImage imageWithContentsOfFile:path];
    if(img) return [img CGImage];
    return NULL;
 }

 unsigned char* CGImageRef_data(CGImageRef image) {
     NSInteger width = CGImageGetWidth(image);
     NSInteger height = CGImageGetHeight(image);
     unsigned char *data = (unsigned char*)calloc(width*height, 4);

     CGContextRef context = CGBitmapContextCreate(data,
                                                  width, height,
                                                  8, width * 4,
                                                  CGImageGetColorSpace(image),
                                                  kCGImageAlphaPremultipliedLast);

     CGContextDrawImage(context,
                        CGRectMake(0.0, 0.0, (float)width, (float)height),
                        image);
     CGContextRelease(context);

     return data;
}

end-c-code
)

(define CGImageRef-width
  (c-lambda (CGImageRef) int "___result = CGImageGetWidth(___arg1);"))

(define CGImageRef-height
  (c-lambda (CGImageRef) int "___result = CGImageGetHeight(___arg1);"))

(define CGImageRef-load
  (c-lambda (char-string) CGImageRef "CGImageRef_load"))

(define CGImageRef-data
  (c-lambda (CGImageRef) unsigned-int8-array "CGImageRef_data"))

;; PVRCT loading

(c-declare #<<end-c-code

#include <stdio.h>
#include <stdlib.h>

#define PVR_TEXTURE_FLAG_TYPE_MASK 0xff

static char gPVRTexIdentifier[4] = "PVR!";

enum {
    kPVRTextureFlagTypePVRTC_2 = 24,
    kPVRTextureFlagTypePVRTC_4
};

struct pvrheader {
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
};
typedef struct pvrheader pvrheader;
 
struct pvrdata {
    GLenum format;
    int width;
    int height;
    int alpha;
    int data_size;
    void* data;
};
typedef struct pvrdata pvrdata;

pvrdata* unpack_pvr_data(const char* filename) {
    pvrheader *header = (pvrheader*)malloc(sizeof(pvrheader));
    pvrdata *result = (pvrdata*)malloc(sizeof(pvrdata));
    int flags, pvrTag;
    int dataLength = 0;
    int width = 0, height = 0;
    int formatFlags;

    FILE *f = fopen(filename, "r");
    fread(header, sizeof(pvrheader), 1, f);
    
    pvrTag = CFSwapInt32LittleToHost(header->pvrTag);

    if (gPVRTexIdentifier[0] != ((pvrTag >>  0) & 0xff) ||
        gPVRTexIdentifier[1] != ((pvrTag >>  8) & 0xff) ||
        gPVRTexIdentifier[2] != ((pvrTag >> 16) & 0xff) ||
        gPVRTexIdentifier[3] != ((pvrTag >> 24) & 0xff)) {
        free(header);
        free(result);
        return NULL;
    }

    flags = CFSwapInt32LittleToHost(header->flags);
    formatFlags = flags & PVR_TEXTURE_FLAG_TYPE_MASK;

    if (formatFlags == kPVRTextureFlagTypePVRTC_4 ||
        formatFlags == kPVRTextureFlagTypePVRTC_2) {

        if (formatFlags == kPVRTextureFlagTypePVRTC_4)
            result->format = GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG;
        else if (formatFlags == kPVRTextureFlagTypePVRTC_2)
            result->format = GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG;
	
        result->width = CFSwapInt32LittleToHost(header->width);
        result->height = CFSwapInt32LittleToHost(header->height);
        result->alpha = CFSwapInt32LittleToHost(header->bitmaskAlpha);
        
        dataLength = CFSwapInt32LittleToHost(header->dataLength);
        result->data = malloc(dataLength);
        result->data_size = dataLength;
        fread(result->data, dataLength, 1, f);
        free(header);
        return result;
    }

    free(header);
    free(result);
    return NULL;
}
           
end-c-code
)

(define pvr-load
  (c-lambda (char-string) pvrdata* "unpack_pvr_data"))

(define pvr-format
  (c-lambda (pvrdata*) int "___result=___arg1->format;"))

(define pvr-width
  (c-lambda (pvrdata*) int "___result=___arg1->width;"))

(define pvr-height
  (c-lambda (pvrdata*) int "___result=___arg1->height;"))

(define pvr-alpha
  (c-lambda (pvrdata*) bool "___result=___arg1->alpha;"))

(define pvr-data
  (c-lambda (pvrdata*) void-array "___result_voidstar=___arg1->data;"))

(define pvr-data-size
  (c-lambda (pvrdata*) int "___result=___arg1->data_size;"))
