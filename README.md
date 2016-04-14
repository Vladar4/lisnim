lisnim - Lisp interpreter in Nim
================================

Inspired by [lis.py](http://norvig.com/lispy.html) by Peter Norvig.

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

