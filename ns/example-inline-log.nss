(ns-set 'separator "::")

(ns "app"

  ;; Main logger namespace
  (define log-prefix "[APP] ")

  ;; Inline helper functions (want to call them as app::debug, app::info, etc.)
  (ns-inline "log"
    (define (debug msg)
      (display (string-append log-prefix "[DEBUG] " msg)))

    (define (info msg)
      (display (string-append log-prefix "[INFO] " msg)))))
