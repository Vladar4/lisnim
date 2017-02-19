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
* (fn arg body) or (fun arg body)
* (do (expr1) (expr2) ...) or (begin (expr1) (expr2) ...)
* (echo arg)
* (call-some-function args...)

Types
-----

* List: (item1 item2 ...)
* Number: int, float or ratio
* Bool: T or NIL
* String: "line of characters bound in quotes"

Built-in functions
------------------

* **Boolean:** t, nil
* **Type-check:** bool?, number?
* **Check:** nil?, null?
* **Constants:** pi, e
* **Arithmetics:** +, -, \*, /, max, min, abs, round, mod, odd?, even?
* **Comparison:** =, !=, >, <, >=, <=
* **List:** cons, snoc, car, cdr, len
* **Output:** echo
* **Strings:** capitalize, upcase, downcase, length, char, format, fmt
* **Exit:** exit, quit (with optional errorcode)

Example
-------

```
(def (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))
```

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


Embedding:
----------

```nim
  import lis

  proc myfunc(args: openarray[Atom]): Atom {.cdecl.} =
    stdout.write "This is my own function! "
    for arg in args:
      stdout.write $arg
    stdout.write "\n"

  setAtom("myfunc", atom myfunc)
  discard execLine("""(myfunc "hello," " world" "!")""")
```

