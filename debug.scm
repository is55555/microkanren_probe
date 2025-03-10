;; debug.scm - Debugging toggle and macro

(define enable-debug #t)  ;; Change to #t to enable debugging

(define-syntax debug
  (syntax-rules ()
    ((debug expr)
     (if enable-debug
         (begin expr)  ;; Executes expr only if debugging is enabled
         (begin)))))   ;; Expands to nothing when debugging is off
