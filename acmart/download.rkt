#lang racket/base

;; Provides helpers for downloading acmsmall style files
;; (mostly taken from the lipics package)

(require file/md5
         file/unzip
         net/url
         racket/file
         racket/port
         racket/system)

(provide acmart-class-path
         acmart-bst-path
         download-acmart-files)

(define acmart-url
  "https://www.acm.org/binaries/content/assets/publications/consolidated-tex-template/acmart.zip")
(define acmart-hash #"d2e70e6c9f1af2942ae75456d0cdda70")

(define acmart-base-path
  (build-path (find-system-path 'addon-dir) "acmart-style-files"))
(define acmart-class-path (build-path acmart-base-path "acmart" "acmart.cls"))
(define acmart-bst-path (build-path acmart-base-path "acmart" "ACM-Reference-Format.bst"))

;; Download acmsmall class file to the add-on directory
(define (download-acmart-files)
  (unless (directory-exists? acmart-base-path)
    (define tmp (make-temporary-file))
    (displayln (format "Downloading class file via ~a" acmart-url))
    (define out (open-output-file tmp #:exists 'truncate))
    (call/input-url (string->url acmart-url)
                    get-pure-port
                    (λ (in) (copy-port in out)))
    (close-output-port out)
    (define hash
      (with-input-from-file tmp
        (λ () (md5 (current-input-port) #t))))
    (unless (equal? hash acmart-hash)
      (raise-arguments-error 'scribble/acmsmall
                             "Invalid MD5 hash for acmsmall tarball"
                             "expected" acmart-hash
                             "given" hash))
    ;; Don't make the directory until we have a valid download
    (make-directory acmart-base-path)
    (unzip tmp (make-filesystem-entry-reader #:dest acmart-base-path))

    ;; Invoke the Makefile in that directory...
    (parameterize ([current-directory (build-path acmart-base-path "acmart")])
      (system "pdflatex acmart.ins" #:set-pwd? #t))))
    