#lang scribble/manual

@(require planet/scribble
          scribble/eval
          (for-label (this-package-in main)
                     racket/base))


@(define my-evaluator (make-base-eval))


@title{porter-stemmer: a packaging of the porter-stemming implementation by Programming Praxis}

@(defmodule/this-package main)

This is a simple packaging of the
@link["http://programmingpraxis.com/2009/09/08/porter-stemming/"]{reference
implementation} of the
@link["http://tartarus.org/martin/PorterStemmer/"]{Porter Stemming
Algorithm}; it normalizes English terms to their stems.  As a quick
example:

@interaction[#:eval my-evaluator
             (require (planet dyoo/porter-stemmer))
             (stem "racketeer")
             (stem "singing")]

@defproc[(stem [a-word string]) string]{

@racket[stem] takes in a single word and returns its stem, according to the
porter-stemming algorithm.

For example:
@interaction[#:eval my-evaluator
             (stem "smiling")
             (stem "frowned")
             (map stem (regexp-split #px"\\s" 
                                     (string-append "What are the roots that clutch, what branches grow\n"
                                                    "Out of this stony rubbish?")))]

Caveat: note that the stemmer does not deal with punctuation.
}