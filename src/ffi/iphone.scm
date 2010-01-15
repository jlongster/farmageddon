;;;; "iphone ffis"
;;; Implements datatypes and procedures for interacting with iPhone
;;; APIs.

(c-declare "#import <UIKit/UIKit.h>")

(include "iphone#.scm")

;;;; UIView
 
(define current-view (make-parameter #f))

(c-define (register-view view) (UIView*) void "register_view" ""
  (current-view view))

(define UIView-bounds
  (c-lambda (UIView*) CGRect #<<end-c-code
   CGRect *res = ___EXT(___alloc_rc(sizeof(CGRect)));
   *res = ___arg1.bounds;
   ___result_voidstar = res;
end-c-code
))

(define UIView-width
  (c-lambda (UIView*) int "___result=___arg1.bounds.size.width;"))

(define UIView-height
  (c-lambda (UIView*) int "___result=___arg1.bounds.size.height;"))

;;;; UIEvent

;;; Nothing implemented yet

;;;; UITouch & Touch events

(define UITouch-tap-count
  (c-lambda (UITouch*) int "___result = ___arg1.tapCount;"))

(define UITouch-timestamp
   (c-lambda (UITouch*) NSTimeInterval "___result = ___arg1.timestamp;"))

(define (UITouch-location touch)
  (if (current-view)
      ;; (let ((point (%%UITouch-location touch (current-view))))
      ;;   (cons (CGPoint-x point) (CGPoint-y point)))
      (cons (%%UITouch-location-x touch (current-view))
            (%%UITouch-location-y touch (current-view)))
      (error "No current view available")))

(define %%UITouch-location-x
  (c-lambda (UITouch* UIView*) int "___result = [___arg1 locationInView:___arg2].x;"))

(define %%UITouch-location-y
  (c-lambda (UITouch* UIView*) int "___result = [___arg1 locationInView:___arg2].y;"))

;; TODO: Make UITouch-location use this one
(define %%UITouch-location
  (c-lambda (UITouch* UIView*) CGPoint #<<end-c-code
   CGPoint* res = ___EXT(___alloc_rc(sizeof(CGPoint)));
   *res = [___arg1 locationInView:___arg2];
   ___result_voidstar = res;
end-c-code
))

(c-define (touches-began touches event)
    (NSSet* UIEvent) void "touches_began" ""
  (run-event-handlers 'touches-began
                      (NSSet->list touches)
                      event))

(c-define (touches-moved touches event)
    (NSSet* UIEvent) void "touches_moved" ""
  (run-event-handlers 'touches-moved
                      (NSSet->list touches)
                      event))

(c-define (touches-ended touches event)
    (NSSet* UIEvent) void "touches_ended" ""
  (run-event-handlers 'touches-ended
                      (NSSet->list touches)
                      event))

(c-define (touches-cancelled touches event)
    (NSSet* UIEvent) void "touches_cancelled" ""
  (run-event-handlers 'touches-cancelled
                      (NSSet->list touches)
                      event))

;;;; UIAcceleration & Accelerometer events

(define UIAcceleration-x
  (c-lambda (UIAcceleration*) UIAccelerationValue
            "___result = ___arg1.x;"))

(define UIAcceleration-y
  (c-lambda (UIAcceleration*) UIAccelerationValue
            "___result = ___arg1.y;"))

(define UIAcceleration-z
  (c-lambda (UIAcceleration*) UIAccelerationValue
            "___result = ___arg1.z;"))

(define UIAcceleration-timestamp
  (c-lambda (UIAcceleration*) NSTimeInterval
            "___result = ___arg1.timestamp;"))

(c-define (did-accelerate accelerometer acceleration)
    (UIAccelerometer* UIAcceleration*) void "did_accelerate" ""
  (run-event-handlers 'did-accelerate
                      accelerometer
                      acceleration))

;;;; Usage

;;; (define-event-handler (touches-began touches event)
;;;   (for-each (lambda (touch)
;;;               (pp "tap count: ")
;;;               (pp (UITouch-tap-count touch))
;;;               (pp "location: ")
;;;               (pp (UITouch-location touch))
;;;             touches))

;;; (define-event-handler (did-accelerate device accel)
;;;   (pp (UIAcceleration-x accel))
;;;   (pp (UIAcceleration-y accel))
;;;   (pp (UIAcceleration-z accel)))
