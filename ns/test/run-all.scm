;; test/run-all.scm --- Run all test harness and sample .nss files

(load "test/harness/test-harness.scm")

(define (run-tests)
  (display "=== Running test harness files ===\n")

  ;; Env tests
  (load "test/env/test-env-stack.scm")
  (load "test/env/test-env-shadowing.scm")

  ;; Rewrite tests
  (load "test/rewrite/test-rewrite.scm")
  (load "test/rewrite/test-rewrite2.scm")
  (load "test/rewrite/test-rewrite-lambda.scm")
  (load "test/rewrite/test-rewrite-extra-let-bindings.scm")

  ;; Util tests
  (load "test/util/test-util.scm"))

(define (run-nss-samples)
  (display "\n=== Running .nss samples ===\n")
  (let* ((files '("example.nss"
                 "example-inline.nss"
                 "lambda-rewrite-tests-standalone.nss"))
         (timestamp (current-date-time-string)))
    (for-each
     (lambda (fname)
       (let* ((basename (substring fname 0 (- (string-length fname) 4))) ; strip .nss
              (outfile (string-append "test/samples/logs/" basename "-" timestamp ".scm"))
              (cmd (string-append "chezscheme --script nss-cli.scm test/samples/" fname
                                  " > " outfile)))
         (display "Running: ") (display fname) (newline)
         (system cmd)))
     files)))

(define (current-date-time-string)
  (let* ((now (current-second))
         (tm (seconds->local-time now)))
    (format "~4,'0d-~2,'0d-~2,'0d_~2,'0d-~2,'0d"
            (+ 1900 (time-year tm))
            (+ 1 (time-month tm))
            (time-day tm)
            (time-hour tm)
            (time-minute tm))))

(run-tests)
(run-nss-samples)
