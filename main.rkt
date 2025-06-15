#lang racket

(require "parser.rkt"
         "runtime.rkt")

(define (main)
  (command-line
   #:args (filename)
   (let ([program (parse-file filename)])
     (eval-program program))))

(module+ main
  (main))