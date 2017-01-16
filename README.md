acmart
======

This package provides support for scribble to target the
[2017 ACM Master Article Template](http://www.acm.org/publications/proceedings-template),
LaTeX class `acmart.cls` v1.25.

Its current state is very preliminary --- not fully working
(notably, no effort was spent on the HTML backend), and
with no promise that its interface is stable.

A lot of inspiration was taken from Vincent St-Amour's `acmsmall` package,
itself inspired from Asumu Takikawa's and Vincent St-Amour's `lipics` package.

For an example article that uses this package, see
[Building Portable Common Lisp Applications with ASDF 3.3](https://github.com/fare/asdf2017)

TODO: the v1.25 class distributed on the ACM site has important bugs,
at least some of which are fixed in v1.28 or later as found
[on github](https://github.com/borisveytsman/acmart).
Use the latter source instead of the former.
Since we only need a couple of files, maybe download them directly
using the "correct" version URL from github?
