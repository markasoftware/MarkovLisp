# MarkovLisp

A minimal Markov Chain implementation in Common Lisp. I was really impressed at how small I was able to make it using the `loop` macro! It builds models and generates sentences fairly fast thanks to an internal representation that plays nice with consy lists.

## Usage

```lisp
(load "markov.lisp")
(defvar *model* (markov:make-model "Jack and jill went up the hill."))
(markov:gen-sentence *model* "Jack") ;; => "Jack and jill went up the hill."

(ql:quickload :alexandria)
(defvar *moby-model* (markov:make-model (alexandria:read-file-into-string "moby-dick.txt")))
(markov:gen-sentence *moby-model* "lad") ;; => "lad a ledge; the ardour to exhale from the utmost energies."
```

## Room for improvement

* Better punctuation handling.
* Interactive demo (maybe autocomplete?)
