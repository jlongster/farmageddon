
(include "feint#.scm")

(c-declare "#include \"score.h\"")

(define (feint-open-dashboard)
  ((c-lambda (UIView*) void "[___arg1 feintOpenDashboard];")
   (current-view)))

(define (HighScore-name score)
  (NSString-get-ascii-c-string
   ((c-lambda (HighScore*) NSString* "___result = ___arg1.name;")
    score)))

(define HighScore-score
  (c-lambda (HighScore*) int "___result = ___arg1.score;"))
