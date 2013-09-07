# A "universal" function definition form

Let's jam on some riffs concerning function definition.

## Why?

When it comes to defining function signatures, Racket uses at least
three approaches. Here's a (relatively) simple example involving one
required argument and one optional argument. First, plain (untyped)
Racket with contracts:

```racket
(provide (contract-out [mult ((number?) (number?) . ->* . number?)]))
(define (mult x [y 1])
  (* x y))
```

Next, the equivalent in Typed Racket:

```racket
(: mult (case-> (Number -> Number)
                (Number Number -> Number)))
(define (mult x [y 1])
  (* x y))
```

Of course this must use types (instead of contracts). However there is
also a difference in _form_. That seems unnecessary.

Indeed, when we write documentation, Scribble's `defproc` uses exactly
the same form for untyped and typed:

```racket
;; Untyped
@defproc[(mult [x number?][y number? 1]) number?]{ some doc text }
;; Typed
@defproc[(mult [x Number][y Number 1]) Number]{ some doc text }
```

Although PL designers and connoisseurs might not mind such variety, a
newcomer would. There's plenty else they could be learning instead. It
would be nice instead to use one, consistent notation.

As we saw above, such notation essentially already exists --- it's the
respresentation for function signatures used by Scribble's
[defproc][]. So riff number one is: Let's use that as the starting
point.

## Doc strings to a Scribble `pre-flow` in a `doc` submodule

Speaking of Scribble: Many functions require only a paragraph or two
of documentation, plus examples. It would be great if a function
definition could supply a doc string. This should end up as a Scribble
`pre-flow` in a `doc` submodule, much like the convention of using a
`test` submodule for unit tests.

## Examples used for both doc and tests

Speaking of tests: Good documentation includes a few examples of using
the function, including its edge cases: A set of example inputs ando
utputs. Do such examples sound similar to unit tests? Yep. Besides,
nothing is more embarrassing than doc examples that don't work. As a
result, a function definition should allow you to provide examples,
which end up being used both (a) in the documentation and (b) as
`rackunit` `check-equal?` tests.

## Public vs. private, and contracts handled right

While we're at it, let's provide two variations of the form: `defn`
and `defn-`. The former `provide`s the function, the latter does
not. Not only does this avoid some busywork (writing out `provide`s),
it also lets us do the right thing with respect to contracts. Using
`define/contract` vs. `(provide (contract-out ....))` can make a large
difference for performance.

Their usage is exactly the same, so simply changing the name will
toggle between private and public.

> *NOTE*: Although they could be named `defproc` and `defproc-`. That could be confusing due to Scribble `defproc`. Using `defn` and `defn-` potentially confusing only if you know Clojure.

# Examples

First, a simple example without any doc or examples:

```racket
#lang at-exp racket
(defn (mult [x number?][y number? 1] -> number?)
  (* x 3))
```

This will `expand-once` to:

```racket
(begin
  (define (mult x (y 1))
    (* x 3))
  (provide (contract-out (mult (->* (number?) (number?) number?)))))
```

This example adds a `#:doc` and multiple `#:ex` examples:

```racket
#lang at-exp racket
(defn (mult [x number?][y number? 1] -> number?)
  #:doc @list{Multiplies @racket[x] by @racket[y].
              Use it fruitfully.}
  #:ex [10 3 => 30]
  #:ex [0 3 => 0]
  #:ex [10 => 10]
  #:ex [0 => 0]
  ;; And the function body:
  (* x 3))
```

It will `expand-once` to this:

```racket
(begin
  (define (mult x (y 1))
    (* x 3))
  (provide (contract-out (mult (->* (number?) (number?) number?))))
  (module+ test
   (require rackunit)
   (check-equal? (mult 10 3) 30)
   (check-equal? (mult 0 3) 0)
   (check-equal? (mult 10) 10)
   (check-equal? (mult 0) 0))
  (module+ doc
   (defproc
    (mult (x number?) (y number? 1))
    number?
    (list
     "Multiplies "
     (racket x)
     " by "
     (racket y)
     "."
     "\n"
     "Use it fruitfully.")
    "\n"
    "Examples:\n"
    "> (mult 10 3)\n30\n"
    "> (mult 0 3)\n0\n"
    "> (mult 10)\n10\n"
    "> (mult 0)\n0\n")))
```

And to see the resulting expansion of the `defproc` from the `doc`
submodule:

```racket
> (require (submod "." doc))

(box-splice (list (nested-flow (style 'vertical-inset '()) (list (table (style 'boxed '(#(struct:attributes ((class . "RBoxed"))))) (list (list (nested-flow (style #f '()) (list (nested-flow (style "RBackgroundLabel" '(decorative command #(struct:alt-tag "div") #(struct:attributes ((class . "SIEHidden"))))) '(#(struct:nested-flow #(struct:style "RBackgroundLabelInner" (#(struct:alt-tag "div"))) (#(struct:paragraph #(struct:style #f #4=(omitable)) "procedure"))))) (paragraph (style #f '(#(struct:attributes ((class . "RForeground"))) . #4#)) (list (element #f (list (element #3=(style "RktPn" '(tt-chars . #0=(#(struct:css-addition (collects #"scribble" #"racket.css")) #(struct:tex-addition (collects #"scribble" #"racket.tex"))))) "(") (part-relative-element #<procedure:.../manual-bind.rkt:135:7> #<procedure:.../manual-bind.rkt:168:7> #<procedure:.../manual-bind.rkt:169:7>) #1=(element 'hspace '(" ")) #5=(cached-element #2=(style "RktVar" '(tt-chars . #0#)) "x" ...) #1# (element #f (list "[" (element #f (list #8=(cached-element #2# "y" ...) "]")))) (element #3# ")"))) #1# 'rarr #1# #6=(cached-element (style "RktSym" '(tt-chars . #0#)) "number?" ...)))))) (list (paragraph (style #f #4#) (list #7=(element 'hspace '("  ")) #5# #1# ":" #1# #6#))) (list (paragraph (style #f #4#) (list #7# #8# #1# ":" #1# #6# #1# "=" #1# (cached-element (style "RktVal" '(tt-chars . #0#)) "0" ...)))))))) "hi" "\n" "Examples:\n" "> (mult 0)\n0\n" "> (mult 10 20)\n200\n") ...)
```

## Caveats

The current implementation is a sketch --- simple and simplistic. In
addition to the TO-DO list below, I'm confident there are gotchas and
stupidities that haven't even occurred to me yet.

## TO-DO

- The Typed Racket versions are implemented, but there's a bug with
  optional arguments (see `typed/main.rkt`).

- `#:rest` arguments

- The `rackunit` expansion is simplistic; `check-equal?` won't handle
  everything. e.g. The function:
  - Returns `values`
  - Returns `float`
  - Raises an exception for some inputs.

- Great, it makes `doc` submodules. Now what? i.e. What's the
  equivalent of `raco test`?

- The non-`provide` variation (currently named `defn-`) probably
  shouldn't bother to make docs. i.e. Wouldn't you only doc "public"
  functions `provide`d outside the module?

[defproc]: http://docs.racket-lang.org/scribble/doc-forms.html#(form._((lib._scribble/manual..rkt)._defproc))
