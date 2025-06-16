#lang racket

(provide parse-file)

(define (parse-file filename)
  (call-with-input-file filename
    (lambda (in)
      (let loop ([exprs null])
        (let ([expr (read in)])
          (if (eof-object? expr)
              (reverse exprs)
              (loop (cons expr exprs))))))))