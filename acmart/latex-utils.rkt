#lang racket
;; -*- Scheme -*-
(require racket/port scribble/core scribble/decode)

(provide (all-defined-out))

;; Consider x as a tree where nodes are cons cells and leaves are either
;; strings or ignorable junk, and concatenate all the strings together.
(define (flatten-text . x)
  (call-with-output-string
   (λ (out)
     (letrec [[f (λ (x)
                   (cond
                    [(string? x) (display x out)]
                    [(pair? x) (f (car x)) (f (cdr x))]))]]
       (f x)))))

;; Null Style
(define null-style (make-style #f '()))

;; Raw output
(define (normal-mode . x)
  ;; No need to call flatten-text, I hope
  (make-element null-style (decode-content x)))

;; Style for raw output
(define raw-style (make-style #f '(exact-chars)))

;; Raw output
(define (raw-mode . x)
  ;; No need to call flatten-text, I hope
  (make-element raw-style x))

;; Q: Should we be using (make-multiarg-element style (list x1 x2 x3))
;; when there are several (mandatory) arguments?

;; Call a LaTeX command with an optional argument (pass #f if absent)
(define (latex-command name optional . args)
  (list
   (raw-mode "\\" name)
   (if optional
       (list (raw-mode "[") optional (raw-mode "]"))
       '())
   (map (λ (x) (list (raw-mode "{") x (raw-mode "}"))) args)))

;; Call a LaTeX command with a single optional argument
(define (latex-command/o name (optional #f))
  (latex-command name optional))

;; Call a LaTeX command with a single mandatory argument
(define (latex-command/m name . args)
  (latex-command name #f args))

;; Call a LaTeX command with several mandatory arguments
(define (latex-command/mm name . args)
  (apply latex-command name #f args))

;; Call a LaTeX command with a single mandatory argument
(define (latex-command/om name optional . mandatory)
  (latex-command name optional mandatory))

;; Use a LaTeX environment
(define (in-latex-environment envname . content)
  (list
   (raw-mode (latex-command/mm "begin" envname) "\n")
   content
   (list (raw-mode "\n" (latex-command/mm "end" envname) "\n"))))

;; Define a LaTeX wrapper function
(define-syntax define-latex-wrapper
  (syntax-rules ()
    ;; LaTeX environment
    [(_ name wrap style)
     (define (name . str)
       (nest wrap (in-latex-environment style
                                        (decode-paragraph str))))]
    ;; LaTeX command with no argument
    [(_ name wrap style ())
     (define (name) (nest wrap (latex-command style)))]
    ;; LaTeX command with one optional argument
    [(_ name wrap style ([optional]))
     (define (name [optional #f])
       (nest wrap (latex-command style optional)))]
    ;; LaTeX command with one optional and one mandatory arguments
    [(_ name wrap style ([optional] mandatory))
     (define (name optional . mandatory)
       (nest wrap (latex-command style optional mandatory)))]
    ;; LaTeX command with an optional and several mandatory arguments
    [(_ name wrap style ([optional] mandatory ...))
     (define (name optional mandatory ...)
       (nest wrap (latex-command style optional mandatory ...)))]
    ;; LaTeX command with one mandatory argument
    [(_ name wrap style (mandatory))
     (define (name . mandatory)
       (nest wrap (latex-command style #f mandatory)))]
    ;; LaTeX command with several mandatory arguments
    [(_ name wrap style (mandatory ...))
     (define (name mandatory ...)
       (nest wrap (latex-command style #f mandatory ...)))]))

;;; Define and export LaTeX wrappers
(define-syntax-rule (define-latex-wrappers (name . spec) ...)
  (begin
    (define-latex-wrapper name . spec) ...
    (provide name ...)))


;;; Move the below to a misc-utils file?

;;; Assertion
(define (assert x)
  (unless x (error "assertion failed")))

;;; Ensure some object is wrapped in a list
(define (ensure-list foo) (if (list? foo) foo (list foo)))

;;; Separate a list with a separator
(define (separated-list separator . list)
  (if (null? list) '()
      (letrec ([loop (λ (l)
                       (if (null? (cdr l))
                           l
                           (list* (car l) separator (loop (cdr l)))))])
        (loop list))))

;;; Separate a list with commas
(define (comma-separated . list)
  (apply separated-list ", " list))

;;; Magic nest macro
(define-syntax nest
  (syntax-rules ()
    [(nest) #f]
    [(nest () x ...) (nest x ...)]
    [(nest (x ...) y z ...) (x ... (nest y z ...))]
    [(nest x) x]))

;;; Debug macro for print-debugging
;; tag is typically a constant string or keyword to identify who is printing,
;; but can be an arbitrary expression returning a tag to be display'ed first;
;; if the expression returns #f, nothing is printed.
;; exprs are expressions, which when the tag was not #f are evaluated in order,
;; with their source code then their return values being printed each time.
;; The last expresion is *always* evaluated and its multiple values
;; are returned, but its source and return values are only printed if tag
;; was not #f;
;; previous expressions are not evaluated at all if tag returned #f.
;; The macro expansion has relatively low overhead in space or time.

(define-syntax DBG
  (syntax-rules ()
    [(_ tag-expr)
     (DBG-helper tagval '() '() #f #f)]
    [(_ tag-expr dbg-expr ... expr)
     (let ([tagval tag-expr]
           [thunk (λ () expr)])
       (if tagval
           (DBG-helper tagval '(dbg-expr ...) (list (λ () dbg-expr) ...)
                       'expr thunk)
           (thunk)))]))

(define (DBG-helper tag dbg-exprs dbg-thunks expr thunk)
  (letrec
      ([f (λ (fmt . args) (display (apply format fmt args)))]
       [v (λ (l) (for-each (λ (x) (f " ~s" x)) l) (newline))]
       [x (λ (expr thunk)
            (f "  ~s =>" expr)
            (call-with-values thunk (λ x (v x) (apply values x))))])
    (if tag
        (begin
          (f "~a\n" tag)
          (for-each x dbg-exprs dbg-thunks)
          (if thunk (x expr thunk) (values)))
        (if thunk (thunk) (values)))))

