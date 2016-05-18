lisnim - Lisp interpreter in Nim
================================

Inspired by [lis.py](http://norvig.com/lispy.html) by Peter Norvig.

This is "just for fun" project. Don't expect much.

Usage
-----

lis [source1 source2 ...]

Syntax
------

* (quote ...) or '(...)
* (list ...)
* (if test conseq alt)
* (def name value)
* (def (name args...) body)
* (lambda (args...) body)
* (echo arg) // returns arg
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
* **Check:** nil?, null?
* **Constants:** pi, e
* **Arithmetics:** +, -, \*, /, max, min, abs, round, mod, odd?, even?
* **Comparison:** =, !=, >, <, >=, <=
* **List:** cons, car, cdr, len
* **Output:** echo
* **Exit:** exit, quit (with optional errorcode)

Example
-------

`(def (factorial n) (if (<= n 1) 1 (\* n (factorial (- n 1)))))`

Building
--------

To build:
`nim c lis.nim`

To run on Linux:

`./lis`

`./lis filename`

To run on Windows:

`.\lis.exe`

`.\lis.exe filename`

File
----

File should consist of individual expressions, one per line.

`(def (plus1 n) (+ n 1))`

Or you can use \\ before line break to span a single expression on multiple lines.

```
(def (plus2 n) \
  (+ n 2))
```

Notes
-----

* Nested functions are not supported.
