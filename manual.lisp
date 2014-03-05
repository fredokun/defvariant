
#|

# Defvariant: User Manual

`defvariant` implements the *variant* types (a.k.a. disjoint sum types) in Common Lisp.  This document describes the main functionalities of `defvariant` and it is provided in two forms:

 1. `manual.md`: the document in the *markdown* format
 2. `manual.lisp`: the lisp source code of the manual, and also an executable Common Lisp script.

Hence the manual can be read, but it is also possible (and recommanded !) to follow the manual interactively within a *repl* (e.g. *slime*).


## Installation

This is a *quickproject* package so assuming the latter is installed, using defvariant simply consists loading the library as follows.

|#

(ql:quickload "defvariant")

#|

Now, we will do things properly so we will define a package
for the manual.

|#

(defpackage :defvariant-manual
  (:use :cl :defvariant)
  (:import-from :defvariant :*example-enabled* :example))

(in-package defvariant-manual)

#|

Normally, the `example` macro is not exported by `defvariant` but
we will use it also in the manual (cf. `defvariant.lisp`) for the details.

|#

(setf *example-enabled* t) ;; we activate the "examples-as-assertion".

#|


## Motivations

In typefull functional programming languages, especially Ocaml and Haskell, a natural companion to record (or structure) types is the **variant type**. Instead of a labelled kind of (cartesian) product type, it is a labelled kind of disjoint sum. Whereas the labels play a somewhat secondary role in records (except for extensible records, which are neat !) they are central in variants to ensure the disjointness of the different cases.

Ok so now let's start the blasphemy, here follows some OCaml code to demonstrate the principle of variant types:

```
    type point = Point of (int * int) 
                | NamedPoint of (string * int * int)
```

Not a very interesting example but that will do the work.
From this OCaml offers various things such as:
  
  - natural equality and ordering relations,
  - and a most useful and favorite **pattern matching** facility

Pattern matching is really an appreciable feature, for example to print things:

```
    let string_of_point p = match p with
      | Point (x,y) -> printf "(%d,%d)" x y
      | NamedPoint (n,x,y) -> printf "%s(%d,%d)" n x y
```
  
Well ... that's it for OCaml ! Variants are a neat feature but in OCaml like in most Bugle programming language: this is what the language designers thought was good for you ... and the heck if you want something different !

### Variants in Lisp ###

As far as I see things (I have relatively bad eyes) ... variants are not a common feature in dynamically-typed language such as Lisp or others. But if there is one single take-away from this long blog post is that: variants *are* useful, whatever the language !

So what would variants look like in Common Lisp? Well, I know the standard Common Lisp macros (such as `defstruct` which is one of the simplest) are often embedded not-so-mini-languages (think `loop` !). But I would vote for simplicity and stupidity for variants, at least as far as this post is concerned.

So to mimic Ocaml's `point` type I would write something like:

```lisp
 (defvariant point
     (plain x y)
     (named name x y))
```
 
For the deconstruction of points, we would then either go the basic lisp-ish way:

```lisp
(defun point->string (p)
  (cond ((point-plain-p p)
         (format nil "(~d,~d)" (point-plain-x p) (point-plain-y p))
        ((point-named-p p)
         (format nil "~s(~d,~d)" (point-named-name p) 
                                 (point-named-x p) 
                                 (point-named-y p))))))
```
            
Another possibility would be to go the macro-slightly-ocaml-ish way:

```lisp
(defun point->string (p)
  (match-point p
    (plain (x y) (format nil "(~d,~d)" x y))
    (named (n x y) (format nil "~s(~d,~d)" n x y))))
```        

I don't know for you but I like the second one better ...


## Case study: geometric shape

To illustrate the main functionality of the package, we use a slightly more complete variant type for geometric shapes.

A shape is either :
  - a `point` with cartesian coordinates `x` and `y`
  - a `segment` from point (`p1-x`,`p1-y`) to  (`p2-x`,`p2-y`) 
  - a `triangle` with points (`p1-x`,`p1-y`), (`p2-x`,`p2-y`) and (`p3-x`,`p3-y`)
  - an `ellipse` with center (`center-x`,`center-y`) and radius `x-radius` and `y-radius`
  - a group of figure in a list named `contents`. 

**Remark**: it *is* a bad idea to use  variant types for implementing extensible data structures. So we know already that introducing a new kind a geometric shape has an important impact on the source code. On the contrary, introducing new functions to manipulate shapes is easy.

## The defvariant macro

The macro `defvariant` allows to define a variant type using the
following syntax: 

```lisp
(defvariant <variant-type>
  (<case_1> <slot_1_1> ... <slot_1_M1>)
  ...
  (<case_N> <slot_N_1> ... <slot_1_MN>))
```

Our variant type for geometric shapes is defined as follows.

|#

(defvariant shape
  (point x y)
  (segment p1-x p1-y p2-x p2-y)
  (triangle p1-x p1-y p2-x p2-y p3-x p3-y)
  (ellipse center-x center-y x-radius y-radius)
  (group contents))
  
#|


The `<variant-type>` is a structure `name-and-options` (cf. CLHS for `defstruct`) that will create a dedicated structure with the given name and without slot. 

Here, we thus have a structure type `shape` but it is abstract in that there is no possibility of constructing an instance of `shape`.

|#

(example (shape-p 12) => nil)

#|

Each case of the variant type generates a structure type named `<variant>-<case_i>` and with slots as defined in the rest of the elements. The structure is made concrete and by default, the case can be constructed using the structure constructor `make-shape-<case_i>`.

For example, the `point` case leads to the creation of a `point` structure with slots `x` and `y`.

|#

(example (shape-point-p (make-shape-point :x 2 :y 1)) => t)

(example (shape-point-x (make-shape-point :x 2 :y 1)) => 2)

(example (shape-point-y (make-shape-point :x 2 :y 1)) => 1)

#|

Note that each case structure is a subtype of the abstract variant type.

For example a `point` is also a `shape`.

|#

(example (shape-p (make-shape-point :x 2 :y 1)) => t)

#|

**Remark**:  except for the abstract structure type, for which it is enforced that it has no constructor and no specific slot, each variant case can be a complete `defstruct` expression.

The complete syntax is thus a kind of `defstruct`:

```lisp
(defvariant <name-and-option>
  (<name-and-options>  {<slot-description>}*)
  ...
  (<name-and-options>  {<slot-description>}*))
```

(cf. the Common Lisp HyperSpec explanations for `defstruct`).


## The match macro

The definition of a variant type `<variant>` with the `defvariant` macro generates a dedicated `match-<variant>` macro that provides a friendly dispatcher for functions that manipulate instances of the variant by case.

It is inspired by the *pattern matching* facility of ML-like languages but in a much simpler - although very often sufficent - version.  It is a good complement to the direct manipulation of the variant objects as structures.

*Remark*: a complementary way of manipulating variants in Common Lisp is to define the structures manually and rely on the `optima` library for pattern matching. This is more featureful but we will see some of the advantages of using `defvariant`'s matching facility, especially compile-time error and exhaustiveness checking.

**TODO**

