#lang racket

(require "parser.rkt"
         "runtime.rkt"
         racket/cmdline)

(define (main)
  (command-line
   #:args (filename)
   (with-handlers ([exn? (lambda (e)
                           (displayln (exn-message e))
                           (exit 1))])
     (let ([program (parse-file filename)])
       (eval-program program)
       (exit 0)))))

(module+ main
  (main))