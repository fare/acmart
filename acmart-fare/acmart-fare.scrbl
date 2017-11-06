#lang scribble/manual

@(require (for-label scribble/acmart
                     scribble/decode))

@title{@tt{acmart} support for Scribble}

@(defmodule scribble/acmart #:lang)

The @racketmodname[scribble/acmart] language provides support for the
@hyperlink["http://www.acm.org/publications/proceedings-template"]{@tt{acmart}}
paper format, used by various ACM journals.

http://www.acm.org/binaries/content/assets/publications/consolidated-tex-template/acmart.pdf

The language provides all of the bindings of @racketmodname[scribble/base]
in addition to additional procedures for supporting @tt{acmart}-specific
typesetting.

Immediately following @racketid{#lang at-exp scribble/acmart} should be a form
pdescribing the options to pass to the @tt{documentclass},
with the following keyword arguments (all of them optioanal)
having the following default values:
@racketid{(#:format 'manuscript :screen #f #:review #f #:natbib #t #:anonymous #f #:authorversion #f)}.
The valid values for @racketid{#:format} are:
@racketid{manuscript acmsmall acmlarge acmtog sigconf siggraph sigplan sigchi sigchi-a}. The other arguments are boolean flags.

More commands are available, that mimic those in the LaTeX class,
as documented in < http://www.acm.org/binaries/content/assets/publications/consolidated-tex-template/acmart.pdf >.

For an example article that uses this package, see @hyperlink["https://github.com/fare/asdf2017"]{Building Portable Common Lisp Applications with ASDF 3.3}.


THIS PACKAGE IS A WORK IN PROGRESS AND
THE REST OF THIS MANUAL IS NOT UP TO DATE.


If using the @racketmodname[scriblib/autobib] library, ensure that the
@racket[define-cite] form is used with the @racket[acmart-style] setting
for @racket[#:style].

Also see the
@(hyperlink "https://github.com/fare/acmart/blob/master/example.scrbl"
            "example document")
to get started.

Known differences in actual TeX-based @tt{acmart} documents:
@itemlist[
@item{Bibliography entries are: "Author. Year. Title, ...".}
@item{Figures have no hrules, and captions look different.}
@item{Tables use the ACM's special commands.}
]

@defproc[(author [name pre-content?] [affil pre-content?] ... ...)
         paragraph?]{
  Registers the authors of the paper.
  The arguments should be an alternating list of author names and affiliations
  (created by @racket[affil]).
}
@defproc[(affiliation [pre-content pre-content?])
         element?]{
  Registers the affiliation of a given author.
}
@defproc[(email [pre-content pre-content?])
         element?]{
  Registers the email of a given author.
}

@defproc[(abstract [pre-content pre-content?] ...) paragraph?]{
  Typesets an abstract.
}
@defform[(include-abstract abstract-path)]{
  Includes the contents of the file at @racket[abstract-path] as abstract.
}

@defproc[(paragraph [pre-content pre-content?] ...) element?]{
  Typesets a paragraph.
}

@defproc[(paragraph* [pre-content pre-content?] ...) element?]{
  Typesets a paragraph.
}

@defproc[(acknowledgments [pre-content pre-content?] ...)
         element?]{
  Typesets an acknowledgments section. This will normally go in the last
  section of a document. Note that @racket[acknowledgments] will not typeset
  as expected if it is used outside of a section, such as in the top-level
  of a document that uses @racket[include-section].
}

@deftogether[(@defproc[(set-copyright [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-volume [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-number [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-article [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-year [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-month [pre-content pre-content?] ...) paragraph?]
              @defproc[(doi [pre-content pre-content?] ...) paragraph?]
              @defproc[(issn [pre-content pre-content?] ...) paragraph?]
              @defproc[(keywords [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-format [pre-content pre-content?] ...) paragraph?]
              @defproc[(bottom-stuff [pre-content pre-content?] ...) paragraph?]
              @defproc[(ccsxml [pre-content pre-content?] ...) paragraph?]
              @defproc[(ccsdesc [pre-content pre-content?] ...) paragraph?]
)]{
Simple wrappers over the corresponding TeX macros in the @tt{acmart} style.
Consult the @tt{acmart} style documentation for details.
}

@defproc[(received [received-date pre-content?] [revised-date pre-content?] [accepted-date pre-content?]) paragraph?]{
Sets various relevant dates. Must be in the last section of the paper.
}

@defthing[acmart-style any/c]{
  A value used to customize bibliography generation for the paper.
  Provide this as an argument for the @racket[define-cite] form using
  the @racket[#:style] keyword.

  To have the proper bibliography title, use @racket[#:sec-title "REFERENCES"]
  when generating the bibliography.
}
