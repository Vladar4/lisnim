lisnim - Lisp interpreter in Nim
================================

Inspired by [lis.py](http://norvig.com/lispy.html) by Peter Norvig.

This is "just for fun" project. Don't expect much.

Usage
-----

lis [source1 source2 ...]

Syntax
------

* (quote ...)
* (list ...)
* (if test conseq alt)
* (define name value)
* (defun name (args...) body)
* (some-function args...)

Types
-----

* List: (item1 item2 ...)
* Number: int, float or ratio
* Bool: T or NIL

Built-in functions
------------------

* **Boolean:** t, nil
* **Type-check:** bool?, number?
* **Constants:** pi, e
* **Arithmetics:** +, -, \*, /, max, min, abs, round, mod, odd?, even?
* **Comparison:** =, !=, >, <, >=, <=
* **List:** car, cdr, len

Example
-------

(defun factorial (n) (if (<= n 1) 1 (\* n (factorial (- n 1)))))

Notes
-----

* Nested functions are not supported.
