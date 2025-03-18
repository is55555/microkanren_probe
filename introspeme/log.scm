(define log-levels '((debug . 10)
(info . 20)
(warning . 30)
(error . 40)
(critical . 50)))

(define current-log-level 10)  ;; Default: DEBUG level

(define (log level msg)
(let ((level-value (cdr (assq level log-levels))))
(when (>= level-value current-log-level)
(display (string-append "[" (symbol->string level) "] " msg "\n")))))

(define (set-log-level level)
(let ((level-value (cdr (assq level log-levels))))
(if level-value
(set! current-log-level level-value)
(display "Invalid log level!\n"))))

;; Usage
(log 'debug "This is a debug message.")     ;; Always prints (default level)
(log 'info "This is an info message.")
(log 'warning "This is a warning.")
(log 'error "This is an error.")
(log 'critical "This is critical.")

(set-log-level 'warning) ;; Change log level

(log 'debug "This won't print.")   ;; Below current level
(log 'info "This won't print.")    ;; Below current level
(log 'warning "This will print.")  ;; At current level
(log 'error "This will print.")    ;; Above current level
(log 'critical "This will print.") ;; Above current level

; =====

(define log-file "logfile.txt")

(define (current-timestamp)
  (number->string (current-second)))  ;; Basic timestamp

(define (log level msg)
  (let ((level-value (cdr (assq level log-levels))))
    (when (>= level-value current-log-level)
      (let ((formatted-msg (string-append "[" (current-timestamp) "] "
                                          "[" (symbol->string level) "] " msg "\n")))
        (call-with-output-file log-file
          (lambda (port)
            (display formatted-msg port))
          #:append)
        (display formatted-msg)))))

;; Example Usage
(log 'info "Logging with timestamps and file support.")



(import (scheme base) (srfi 180)) ;; Requires SRFI-180 (JSON)

(define (log-json level msg)
  (let ((json-log (json-write
                   `(("timestamp" . ,(current-second))
                     ("level" . ,(symbol->string level))
                     ("message" . ,msg)))))
    (display json-log)
    (newline)))

(log-json 'info "Structured logging example.")




(define log-output-port (make-parameter (current-output-port)))  ;; Default: stdout

(define (set-log-destination destination)
  (cond
    ((eq? destination 'stdout) (log-output-port (current-output-port))) ;; Console output
    ((eq? destination 'file) (log-output-port (open-output-file "logfile.txt" #:append)))
    ((eq? destination 'string) (log-output-port (open-output-string)))
    (else (error "Unknown log destination" destination))))

(define (log level msg)
  (let ((formatted-msg (string-append "[" (symbol->string level) "] " msg "\n")))
    (display formatted-msg (log-output-port))))

(define (get-log-string)
  (if (output-port-open? (log-output-port))
      (get-output-string (log-output-port))
      ""))  ;; Return empty if not using string output



;; Default: Logs to stdout
(log 'info "This goes to stdout.")

;; Switch to file logging
(set-log-destination 'file)
(log 'warning "This goes to a file.")

;; Switch to string logging
(set-log-destination 'string)
(log 'debug "This is stored in memory.")

;; Retrieve stored log (when using string output)
(display "Stored Log: ")
(display (get-log-string))
(newline)


; // with function-based switch:

(define (log-to destination level msg)
(let ((output-port
       (cond
         ((eq? destination 'stdout) (current-output-port))
         ((eq? destination 'file) (open-output-file "logfile.txt" #:append))
         ((eq? destination 'string) (open-output-string))
         (else (error "Invalid log destination")))))
  (display (string-append "[" (symbol->string level) "] " msg "\n") output-port)
  (if (eq? destination 'string) (get-output-string output-port) #t)))

; usage

(log-to 'stdout 'info "Standard output log.")
(log-to 'file 'error "File-based logging.")
(define log-string (log-to 'string 'debug "In-memory logging."))

(display "Captured log: ") (display log-string) (newline)
