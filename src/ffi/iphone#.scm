;;;; "iphone ffi types"
;;; Defines c types for the iphone ffis.

(c-define-type UIView "UIView")
(c-define-type UIView* (pointer UIView))
(c-define-type UIEvent "UIEvent")
(c-define-type UIEvent* (pointer UIEvent))
(c-define-type UITouch "UITouch")
(c-define-type UITouch* (pointer UITouch))

(c-define-type UIAccelerometer "UIAccelerometer")
(c-define-type UIAccelerometer* (pointer UIAccelerometer))
(c-define-type UIAcceleration "UIAcceleration")
(c-define-type UIAcceleration* (pointer UIAcceleration))
(c-define-type UIAccelerationValue double)
