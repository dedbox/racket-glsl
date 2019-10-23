#lang racket/base

(require racket/base
         racket/format
         racket/string
         syntax/parse/define
         (for-syntax racket/base
                     glsl/private))

(provide #%app #%datum #%top #%top-interaction
         (rename-out [module-begin #%module-begin]))

(module reader syntax/module-reader glsl)

(define-simple-macro (module-begin form ...)
  #:with (form* ...) (map glsl-top (attribute form))
  (#%module-begin
   (define the-string (format "~a\n" (string-join (list form* ...) "\n")))
   (provide the-string)))
