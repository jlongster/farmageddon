
(c-declare #<<end-c-code

 CGImageRef CGImageRef_load(const char *filename) {
    NSString *path = [NSString stringWithFormat:@"%@/%s",
                      [[NSBundle mainBundle] resourcePath],
                      filename];
    UIImage *img = [UIImage imageWithContentsOfFile:path];
    if(img) return [img CGImage];
    return NULL;
 }

 unsigned char* CGImageRef_data(CGImageRef image) {
     NSInteger width = CGImageGetWidth(image);
     NSInteger height = CGImageGetHeight(image);
     unsigned char *data = (unsigned char*)calloc(width*height, 4);

     NSLog(@"%x %d %d", image, width, height);

     CGContextRef context = CGBitmapContextCreate(data,
                                                  width, height,
                                                  8, width * 4,
                                                  CGImageGetColorSpace(image),
                                                  kCGImageAlphaPremultipliedLast);

     CGContextDrawImage(context,
                        CGRectMake(0.0, 0.0, (float)width, (float)height),
                        image);
     CGContextRelease(context);

     NSLog(@"[%d %d %d %d]", data[0], data[1], data[2], data[3]);
     NSLog(@"[%d %d %d %d]", data[4], data[5], data[6], data[7]);
     NSLog(@"[%d %d %d %d]", data[8], data[9], data[10], data[11]);
     NSLog(@"[%d %d %d %d]", data[12], data[13], data[14], data[15]);

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
