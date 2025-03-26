;; nss-cli.scm - Namespace Preprocessor cli for Scheme
;; Usage: chezscheme --script nss-cli.scm input.nss output.scm

(load "nss-lib.scm")


#;(define (read-all port)
(let loop ((forms '()))
  (let ((form (read port)))
    (if (eof-object? form) (reverse forms) (loop (cons form forms))))))

(define (read-all port)
(let loop ((forms '()))
  (let ((form (read port)))
    (if (eof-object? form)
        (reverse forms)
        (begin
          (display "[read-all] >>> read: ") (write form) (newline)
          (loop (cons form forms)))))))

(define (compile-nss input-filename output-filename)
(let ((in (open-input-file input-filename))
      (out (open-output-file output-filename 'replace)))
  (display ";; This file was automatically generated from a `.nss` source.\n" out)
  (display ";; Do not modify this file directly — edit the original `.nss` file instead.\n\n" out)
  (let ((forms (read-all in)))
    (for-each
     (lambda (form)
       (for-each
        (lambda (out-form)
          (write out-form out)
          (newline out)
          (newline out))
        (process-form form)))
     forms))
  (close-input-port in)
  (close-output-port out)))

(define (main)
(let ((args (command-line)))
  (if (< (length args) 3)
      (begin
        (display "Usage: chezscheme --script nss.scm input.nss output.scm\n")
        (exit 1))
      (compile-nss (cadr args) (caddr args)))))

(main)
