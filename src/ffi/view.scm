
(define (hide-high-score-field)
  ((c-lambda (UIView*) void "[___arg1 hideHighScoreField];")
   (current-view)))

(define (show-high-score-field x y)
  ((c-lambda (UIView* int int) void
             "[___arg1 showHighScoreField:___arg2 y:___arg3];")
   (current-view) x y))

(define (high-score-field-value)
  ((c-lambda (UIView*) char-string "___result = [___arg1 highScoreFieldValue];")
   (current-view)))
