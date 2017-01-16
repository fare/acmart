#lang racket

;; based heavily on acmsmall, itself based heavily on lipics

;; known discrepancies with actual tex-based acmart output:
;; - bibliography entries are: "Author. Year. Title, ...", autobib can't do that
;; - figures have hrules, and "Figure N" and captions look different
;; - tables don't use the ACM's special commands

(require
 "acmart/download.rkt"
 "acmart/latex-utils.rkt"
 racket/class
 racket/port
 scribble/base scribble/decode
 (except-in scribble/core paragraph)
 (rename-in scribble/doclang [#%module-begin -#%module-begin])
 scribble/private/defaults
 scribble/html-properties scribble/latex-properties
 setup/main-collects
 setup/collects
 (for-syntax racket/base racket/syntax))

(provide (all-from-out scribble/base)
         (all-from-out "acmart/latex-utils.rkt")
         (except-out (all-from-out scribble/doclang)
                     -#%module-begin)
         (rename-out [--#%module-begin #%module-begin])
         authorinfo conferenceinfo terms keywords
         ccsxml ccsdesc received include-abstract
         acmart-style)

;; Reader configuration for #lang
(module reader scribble/base/reader scribble/acmart #:wrapper1 (λ (t) (t)))

(define-syntax (--#%module-begin stx)
  (syntax-case stx ()
    [(_ (options ...) ?e ...)
     (with-syntax ([doc (format-id stx "doc")])
       (quasisyntax/loc stx
         (-#%module-begin doc (post-process (acmart-options options ...))
                          () ?e ...)))]
    [(mb string (options ...) ?e ...)
     (begin
       (unless (string? (syntax->datum #'string))
         (error "assertion failed"))
       #'(mb (options ...) ?e ...))]))

(define possible-formats
  '(manuscript acmsmall acmlarge acmtog
    sigconf siggraph sigplan sigchi sigchi-a))

(define (acmart-options
         #:format (format 'manuscript)
         #:screen (screen #f)
         #:review (review #f)
         #:natbib (natbib #t)
         #:anonymous (anonymous #f)
         #:authorversion (authorversion #f))
  (letrec
      ([frob (λ (name val default acceptable printer)
               (assert (memq val acceptable))
               (if (eq? val default) #f
                   (list (printer name val))))]
       [flag (λ (name val default)
               (frob name val default '(#f #t)
                     (λ (name val)
                       (if val name (list name "=false")))))]
       [opts
        (nest
         (apply comma-separated)
         (filter identity)
         (list
          (frob "format" format 'manuscript possible-formats
                (λ (name val) (symbol->string val)))
          (flag "review" review #f)
          (flag "natbib" natbib #t)
          (flag "anonymous" anonymous #f)
          (flag "authorversion" authorversion #f)))])
    (if (null? opts) "" (flatten-text "[" opts "]"))))

(define ((post-process options) doc)
  (add-defaults
   doc
   (string->bytes/utf-8 (format #<<FORMAT
%% Scribble needs these options, so provide before acmart
\PassOptionsToPackage{usenames,dvipsnames}{color}
\documentclass~a{acmart}
\bibliographystyle{plain}
FORMAT
options))
   (collection-file-path "style.tex" "scribble" "acmart")
   (list acmart-class-path acm-bibtex-style-path)
   #f))

(define-syntax extract-formal-argument
  (syntax-rules ()
    [(_ (optional value)) optional]
    [(_ mandatory) mandatory]))

(define-syntax pass-function-formals
  (syntax-rules ()
    [(_ (prefix ...) formal ... . rest)
     (apply prefix ... (extract-formal-argument formal) ... rest)]
    [(_ (prefix ...) formal ...)
     (prefix ... (extract-formal-argument formal) ...)]))

(define (pretitle #:raw (raw #f) content)
  (make-paragraph
   (make-style 'pretitle (if raw '(exact-chars) '()))
   content))
(define (pretitle-raw content) (pretitle #:raw #t content))

;;; Topmatter --  these must all precede title!
(define-latex-wrappers
  ;; NB: see cssxml, cssdesc, terms, keywords, set-top-matter below

  [acm-journal (pretitle) "acmJournal" (short-name)]
  [title (pretitle) "title" ([short-title] full-title)]
  [subtitle (pretitle) "subtitle" (subtitle)]

  ;; Author information
  [author (pretitle) "author" (author)]
  [orcid (pretitle) "orcid" (orcid)]
  [affiliation (pretitle) "affiliation" (affiliation)] ;; may be repeated

  ;; For use within affiliation
  [position (pretitle) "position" (x)]
  [institution (pretitle) "institution" (x)]
  [department (pretitle) "department" (x)]
  [streetaddress (pretitle) "streetaddress" (x)]
  [city (pretitle) "city" (x)]
  [state (pretitle) "state" (x)]
  [postcode (pretitle) "postcode" (x)]
  [country (pretitle) "country" (x)]

  [thanks (pretitle) "thanks" (x)]
  [titlenote (pretitle) "titlenote" (x)]
  [subtitlenote (pretitle) "subtitlenote" (x)]
  [authornote (pretitle) "authornote" (x)]

  [acm-volume (pretitle) "acmVolume" (x)]
  [acm-number (pretitle) "acmNumber" (x)]
  [acm-article (pretitle) "acmArticle" (x)]
  [acm-year (pretitle) "acmYear" (x)]
  [acm-month (pretitle) "acmMonth" (x)]
  [acm-article-seq (pretitle) "acmArticleSeq" (x)]
  [acm-price (pretitle) "acmPrice" (x)]
  [acm-isbn (pretitle) "acmISBN" (x)]
  [acm-doi (pretitle) "acmDOI" (x)]

  ;; Set by the typesetter
  [acm-badge-r (pretitle) "acmBadgeR" ([url] graphics)]
  [acm-badge-l (pretitle) "acmBadgeL" ([url] graphics)]

  [start-page (pretitle) "startPage" (x)]

  ;; x must be one of "none" "acmcopyright" "acmlicensed" "rightsretained"
  ;; "usgov" "usgovmixed" "cagov" "cagovmixed"
  [set-copyright (pretitle) "setcopyright" (x)]

  [copyright-year (pretitle) "copyrightyear" (x)] ;; defaults to acm-year

  [teaser-figure (pretitle) "teaserfigure"]

  [received (pretitle) "received" ([stage] date)])

(define (abstract . abstract-)
  (nest
   (pretitle)
   (in-latex-environment "abstract")
   (decode-content abstract-)))
(provide abstract)

;; Don't let Scribble encode the email, or it will turn - into {-}
(define (email email-)
  (pretitle (latex-command "email" #f (raw-mode email-))))
(provide email)

;;; NB: Use \renewcommand{\shortauthors}{...} *after* \maketitle
;;; to redefine the author list for the running head, in case it is too long.


;; figure table figure* table*
;; sigchi-a: sidebar marginfigure margintable

(define-latex-wrappers
  [printonly () "printonly"]
  [screenonly () "screenonly"]

  [anonsuppress () "anonsuppress"]

  [acknowledgments () "acks"]
  [grant-sponsor () "grantsponsor" ([sponsor-id] name url)]
  [grant-num () "grantnum" ([url] sponsor-id number)]

  [cite-style () "citestyle" (x)]
  [set-cite-style () "setcitestyle" (x)])

(define acm-colors
  '("ACMBlue" "ACMYellow" "ACMOrange" "ACMRed"
    "ACMLightBlue" "ACMGreen" "ACMPurple" "ACMDarkBlue"))


(define-latex-wrappers
  [paragraph () "paragraph"]
  [bottom-stuff () "bottomstuff"])

(define (terms . x)
  (latex-command/mm "terms" (apply comma-separated x)))
(define (keywords . x)
  (latex-command/mm "keywords" (apply comma-separated x)))

(define (set-top-matter #:printccs printccs
                        #:printacmref printacmref
                        #:printfolios printfolios)
  (let ([f (λ (x v) (list x "=" (if v "true" "false")))])
    (latex-command/m
     "settopmatter"
     (separated-list ", "
      (f "printcss" printccs) ;; !!!!! TYPO in acmart v1.25 !!!!
      (f "printacmref" printacmref)
      (f "printfolios" printfolios)))))
(provide set-top-matter)

(define (authorinfo author- #:orcid (orcid- #f) (affiliation- '()) (email- #f))
  (list
   (author author-)
   (when orcid- (orcid orcid-))
   (map affiliation (ensure-list affiliation-))
   (when email- (email email-))))

(define (conferenceinfo #:short-name (short-name #f) name date venue)
  (pretitle
   (latex-command "acmConference" short-name name (raw-mode date) venue)))

;; abstract has to be before the title
;; but there's got to be a better way to do it...
(define-syntax (include-abstract stx)
  (syntax-case stx ()
    [(_ module)
     (let ()
       (define name* (gensym 'name))
       #'(begin
           (require (rename-in module [doc name*]))
           (in-latex-environment
            "abstract"
            (for/list ([p (in-list (part-blocks name*))])
              (pretitle (paragraph-content p))))))]))

;; NB: Use tool at http://dl.acm.org/ccs.cfm to generate a ccsxml block
;; including invocatiosn of ccsdesc.
(define (ccsxml . content) ; see http://dl.acm.org/ccs.cfm for these two
  (pretitle-raw
   (raw-mode (list "\\begin{CCSXML}\n" content "\n\\end{CCSXML}\n"))))
(define (ccsdesc n . content)
  (latex-command "ccsdesc" (if n (number->string n) '()) content))


;; Bibliography setup
(define autobib-style-extras
  (let ([abs (λ (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list
     (make-css-addition (abs "autobib.css"))
     (make-tex-addition (abs "autobib.tex")))))

(define bib-single-style (make-style "AutoBibliography" autobib-style-extras))
(define bibentry-style (make-style "Autobibentry" autobib-style-extras))
(define colbibnumber-style (make-style "Autocolbibnumber" autobib-style-extras))
(define colbibentry-style (make-style "acmartAutocolbibentry" autobib-style-extras))


;;; Bibliographic style, as an alternative to number-style
(define acmart-style
  (new
   (class object%
     (define/public (bibliography-table-style) bib-single-style)
     (define/public (entry-style) colbibentry-style)
     (define/public (disambiguate-date?) #f)
     (define/public (collapse-for-date?) #f)
     (define/public (get-cite-open) "[")
     (define/public (get-cite-close) "]")
     (define/public (get-group-sep) "; ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i) date-cite)
     (define/public (render-author+dates author dates)
       (list* author " " dates))
     (define (make-label i)
       (string-append "autobiblab:" (number->string i)))
     (define/public (bibliography-line i e)
       (list e))
     (super-new))))

;; Download necessary style files if necessary
(download-acmart-files)