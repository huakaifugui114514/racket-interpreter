#lang racket

(require parser/s-expression
         runtime/class
         runtime/inheritance
         runtime/dispatch
         runtime/environment)

(define (main)
  (command-line
   #:args (filename)
   (let ([program (parse-file filename)])
     (eval-program program))))

(module+ main
  (main))