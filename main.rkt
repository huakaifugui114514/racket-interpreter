#lang racket

(require "parser.rkt"
         "runtime.rkt"
         racket/cmdline)

;; 主函数
(define (main)
  (command-line
   #:args (filename)
   (with-handlers ([exn? (lambda (e)
                           (displayln (exn-message e))
                           (exit 1))])
     (let ([program (parse-file filename)])
       (eval-program program)
       (exit 0)))))

;; 当直接运行时调用主函数
(module+ main
  (main))