import
  terminal,
  unittest,
  macros,
  lis

# convenience
template `->`(a, b: untyped): typed =
  check execLine(a) == b

template `=>`(a, b: untyped): typed =
  check execLine(a).kind == b

template isError(a: untyped): typed =
  check execLine(a).kind == aError


styledEcho styleBright, fgWhite,
     "Testing lis.nim:"
echo "================"


test "Typechecks":
  """(bool? t)"""        -> atom true
  """(bool? nil)"""      -> atom true
  """(bool? 5)"""        -> atom false
  """(bool? 5.55555)"""  -> atom false
  """(bool? (> 4 5))"""  -> atom true
  """(bool? "string")""" -> atom false
  """(bool? '())"""      -> atom false
  isError """(bool?)"""

  """(number? 4)"""         -> atom true
  """(number? 4.5678)"""    -> atom true
  """(number? t)"""         -> atom false
  """(number? nil)"""       -> atom false
  """(number? "string")"""  -> atom false
  """(number? '()')"""      -> atom false
  isError """(number?)"""

  # nil? and null? are equivalent
  """(nil? nil)"""        -> atom true
  """(nil? '())"""        -> atom true
  """(nil? t)"""          -> atom false
  """(nil? 4)"""          -> atom false
  """(nil? '(3 4))"""     -> atom false
  """(nil? "string")"""   -> atom false
  isError """(nil?)"""

  """(defined? defined?)"""     -> atom true
  """(defined? i-w4lk-4l0n3)""" -> atom false


test "Arithmetic":
  """(+ 1 2)""" -> atom number 3
  """(- 3 2)""" -> atom number 1
  """(* 2 3)""" -> atom number 6
  """(/ 4 2)""" -> atom number 2
  """(- 3 7)""" -> atom number(-4)

  isError """(/ 4 0)"""


test "Math functions : max":
  """(max 3)"""            -> atom number 3
  """(max 3 4)"""          -> atom number 4
  """(max -4 -6)"""        -> atom number(-4)
  """(max 3 4 5 6 1 12)""" -> atom number 12
  """(max 4.567 2.345)"""  -> atom number 4.567

  isError """(max)"""


test "Math functions : min":
  """(min 12 5 1)"""       -> atom number 1
  """(min 4)"""            -> atom number 4
  """(min -4 5)"""         -> atom number(-4)
  """(min -4 -6)"""        -> atom number(-6)
  """(min 4.567 2.345)"""  -> atom number 2.345

  isError """(min)"""


test "Math functions : abs":
  """(abs -4)""" -> atom number 4
  """(abs 4)"""  -> atom number 4
  """(abs 0)"""  -> atom number 0

  isError """(abs)"""


test "Math functions : round":
  """(round 4.9123123)""" -> atom number 4
  """(round 4)"""         -> atom number 4

  isError """(round)"""


test "Math functions : mod":
  """(mod 7 2)"""  -> atom number 1
  """(mod 4 2)"""  -> atom number 0
  """(mod 5 2)"""  -> atom number 1
  """(mod 7 3)"""  -> atom number 1
  """(mod 7 4)"""  -> atom number 3

  isError """(mod)"""
  isError """(mod 4)"""


test "Math functions : odd?":
  """(odd? 3)"""      -> atom true
  """(odd? 4)"""      -> atom false
  """(odd? 3 5 7)"""  -> atom true
  """(odd? 3 4 7)"""  -> atom false

  isError """(odd?)"""


test "Math functions : even?":
  """(even? 4)"""      -> atom true
  """(even? 3)"""      -> atom false
  """(even? 4 6 8)"""  -> atom true
  """(even? 4 5 8)"""  -> atom false

test "Logic":
  """(= 4 4 4)"""    -> atom true
  """(= 4 4 4.0)"""  -> atom true
  """(= 4 5)"""      -> atom false
  """(= t t)"""      -> atom true
  """(= nil nil)"""  -> atom true
  """(= t nil)"""    -> atom false
  """(= 4 t)"""      -> atom false
  """(= 5 "test")""" -> atom false
  """(= "test" "test")"""  -> atom true
  """(= '(1 2) '(1 2))"""  -> atom true
  """(= '(1 2) '(1 3))"""  -> atom false

  """(!= 4 4 4)"""    -> atom false
  """(!= 4 4 5)"""    -> atom true
  """(!= 4 4 4.0)"""  -> atom false
  """(!= 4 5)"""      -> atom true
  """(!= t t)"""      -> atom false
  """(!= nil nil)"""  -> atom false
  """(!= t nil)"""    -> atom true
  """(!= 4 t)"""      -> atom true
  """(!= 5 "test")""" -> atom true
  """(!= "test" "test")""" -> atom false
  """(!= '(1 2) '(1 2))""" -> atom false
  """(!= '(1 2) '(1 3))""" -> atom true

  # No point in exaustive checking of
  # all >, <, >=, <=
  # They're delegated to Nim anyway -
  # one is correct - all are correct
  """(> 3 2 1)""" -> atom true
  """(> 3 1 2)""" -> atom false
  """(> t nil)""" -> atom true
  """(> nil t)""" -> atom false
  isError """(>)"""
  isError """(> 5)"""
  isError """(> 5 "test")"""
  isError """(> "test")"""

test "Lists":
  var one = atom number 1
  var two = atom number 2
  var thr = atom number 3
  var atom_lst = atom(@[one, two, thr])
  """(list 1 2 3)"""         -> atom_lst
  """(cons 1 '(2 3))"""      -> atom_lst
  """(snoc '(1 2) 3)"""      -> atom_lst
  """(car '(1 2 3))"""       -> atom number 1
  """(cdr '(0 1 2 3))"""     -> atom_lst
  """(cdr (list 5))"""       -> atom()
  isError """(car '())"""
  isError """(cdr '())"""

test "Len with lists and strings":
  """(len (list 1 2 3))"""   -> atom number 3
  """(len '())"""            -> atom number 0
  """(len "")"""             -> atom number 0
  """(len "test")"""         -> atom number 4
  """(len "Хлеб")"""         -> atom number 4
  isError """(len 5)"""
  isError """(len t)"""


test "Echo":
  """(echo 5)"""         -> atom number 5
  """(echo)"""           -> atom()
  """(echo "test")"""    -> atom "\"test\""
  """(echo t)"""         -> atom true
  """(echo nil)"""       -> atom false


test "Strings":
  """(capitalize "some test string")""" -> atom "\"Some Test String\""
  """(capitalize "несколько мягких булок")""" -> atom "\"Несколько Мягких Булок\""

  """(upcase "test")"""    -> atom "\"TEST\""
  """(upcase "хлеб")"""    -> atom "\"ХЛЕБ\""

  """(downcase "TEST")"""  -> atom "\"test\""
  """(downcase "ХЛЕБ")"""  -> atom "\"хлеб\""

  """(char "string" 2)"""  -> atom "r"
  """(char "string" 0)"""  -> atom "s"
  isError """(char "str" 45)"""

  """(format "$1:$1/$2" 4 "Sora")""" -> atom "\"4:4/Sora\""


test "Errors":
  isError """(/4 5)"""


echo "================"
if programResult == 0:
  styledEcho styleBright, fgGreen,
     "     PASSED     "
else:
  styledEcho styleBright, fgRed,
     "     FAILED     "

