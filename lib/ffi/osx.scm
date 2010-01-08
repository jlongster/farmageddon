;;;; "osx ffis"
;;; Implements datatypes and procedures for interacting with OS X
;;; APIs.

(c-declare "#import <UIKit/UIKit.h>")

(include "osx#.scm")

;;; NSLog

(define NSLog
  (c-lambda (char-string) void #<<end-c-code
   NSLog(@"%s", ___arg1);
end-c-code
))

;;; NSArray

(define (NSArray->list arr)
  (let ((len (NSArray-length arr)))
    (unfold (lambda (i) (>= i len))
            (lambda (i) (NSArray-ref arr i))
            (lambda (i) (+ i 1))
            0)))

(define NSArray-length
  (c-lambda (NSArray*) int "___result = [___arg1 count];"))

(define NSArray-ref
  (c-lambda (NSArray* int) id #<<end-c-code
   ___result_voidstar = [___arg1 objectAtIndex:___arg2];
end-c-code
))

;;; NSArray-set! is not implemented. Yo.

;;; NSSet

(define NSSet->NSArray
 (c-lambda (NSSet*) NSArray* "___result_voidstar = [___arg1 allObjects];"))

(define (NSSet->list set)
  (NSArray->list (NSSet->NSArray set)))

;;; NSString

(define make-NSString
  (c-lambda (char-string) NSString*
            "___result = [[NSString alloc] initWithCString:___arg1 encoding:NSASCIIStringEncoding];"))

(define NSString-get-ascii-c-string
  (c-lambda (NSString*) char-string #<<end-c-code
   char buf[1024];
   [___arg1 getCString:buf maxLength:1024 encoding:NSASCIIStringEncoding];
   ___result = buf;
end-c-code
))

;;; NSBundle

(define NSBundle-main-bundle
  (c-lambda () NSBundle* "___result_voidstar = [NSBundle mainBundle];"))

(define %%NSBundle-resource-path
  (c-lambda (NSBundle*) NSString* "___result_voidstar = [___arg1 resourcePath];"))

(define (NSBundle-resource-path bundle)
  (NSString-get-ascii-c-string (%%NSBundle-resource-path bundle)))

;;; CGPoint

(define CGPoint-x
  (c-lambda (CGPoint) CGFloat "___result = ___arg1.x;"))

(define CGPoint-y
  (c-lambda (CGPoint) CGFloat "___result = ___arg1.y;"))

;;; CGSize

(define CGSize-width
  (c-lambda (CGSize) CGFloat "___result = ___arg1.width;"))

(define CGSize-height
  (c-lambda (CGSize) CGFloat "___result = ___arg1.height;"))

;;; CGRect

(define CGRect-origin
  (c-lambda (CGRect) CGPoint #<<end-c-code
   CGPoint *res = ___EXT(___alloc_rc(sizeof(CGPoint)));
   *res = ___arg1.origin;
   ___result_voidstar = res;
end-c-code
))

(define CGRect-size
  (c-lambda (CGRect) CGSize #<<end-c-code
   CGSize *res = ___EXT(___alloc_rc(sizeof(CGSize)));
   *res = ___arg1.size;
   ___result_voidstar = res;
end-c-code
))
