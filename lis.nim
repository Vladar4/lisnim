# lisnim
# Copyright (C) 2016 Vladar
# 
# This software is provided 'as-is', without any express or implied
# warranty.  In no event will the authors be held liable for any damages
# arising from the use of this software.
# 
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
# 
# 1. The origin of this software must not be misrepresented; you must not
# else: return software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source distribution.
# 
# Vladar vladar4@gmail.com

# Contributors:
# Esgorhannoth esgorhannoth@gmail.com

import
  os, parseutils, rationals, sequtils, tables, unicode
import strutils except toLower, toUpper


# NUMBER #

type
  Number* = Rational[int]

template number*(i: float): Number = toRational(i, high(int32))
template number*[T](i: T): Number = toRational(i)


# ERROR #
type
  Error* = object   ## Error object
    s*: string

template error*(str: string): Error =
  Error(s: str)


# ENV, BUILTIN, FUN, ATOM #

type
  Env = ref EnvObj  ##  Environment for ``eval()``
  EnvObj = object
    table: TableRef[string, Atom] ##  Environment table
    outer: Env                    ##  Outer (parent) environment

  Builtin* = proc(args: openArray[Atom]): Atom {.cdecl.} ##  Built-in proc type

  Fun* = object  ##  Function object
    args: seq[string] ##  Function arguments (for user-defined functions)
    body: seq[Atom]   ##  Function body (for user-defined functions)
    builtin: Builtin  ##  Function reference (for built-in functions)
    env: Env          ##  Function environment

  AtomKind* = enum
    aList, aNumber, aSymbol, aBool, aFun, aError

  Atom* = object                  ##  Atom type
    case kind*: AtomKind          ##  Variant of
    of aList:   list*: seq[Atom]  ##  List of atoms
    of aNumber: n*: Number        ##  Number
    of aSymbol: s*: string        ##  Symbol
    of aBool:   b*: bool          ##  Boolean
    of aFun:    f*: Fun           ##  Function
    of aError:  e*: Error         ##  Error


# ATOM #

template atom*(): Atom = Atom(kind: aList, list: @[])
template atom*(val: seq[Atom]): Atom = Atom(kind: aList, list: val)
template atom*(val: openArray[Atom]): Atom = Atom(kind: aList, list: @val)
template atom*(val: Number): Atom = Atom(kind: aNumber, n: val)
template atom*(val: string): Atom = Atom(kind: aSymbol, s: val)
template atom*(val: bool): Atom = Atom(kind: aBool, b: val)
proc atom*(val: Fun): Atom = Atom(kind: aFun, f: val)
proc atom*(err: Error): Atom = Atom(kind: aError, e: err)
proc atom*(builtin: Builtin): Atom =
  atom(Fun(args: @[], body: @[], builtin: builtin))
proc atom*(args: seq[string], body: seq[Atom], env: Env): Atom =
  atom(Fun(args: args, body: body, env: env))


# Symbol procs

proc is_comment*(a: Atom): bool =
  ##  ``Return`` `true` if atom is a comment (starts with `;`),
  ##  or `false` otherwise.
  return case a.kind:
    of aSymbol: a.s[0] == ';'
    else: false


proc is_quoted*(a: Atom): bool =
  ##  ``Return`` `true` if atom is quoted (starts with `'`),
  ##  or `false` otherwise.
  return case a.kind:
  of aSymbol:
    a.s[0] == '\''
  else:
    false


proc is_string*(a: Atom): bool =
  ##  ``Return`` `true` if atom is string (bound in quotes),
  ##  or `false` otherwise.
  return case a.kind:
  of aSymbol:
    (a.s[0] == '\"') and (a.s[^1] == '\"')
  else:
    false


proc `$`*(obj: Atom): string # forward declaration
proc str_strip*(a: Atom): string =
  ## Strip quotes from around the string.
  if a.is_string:
    a.s[1..^2]
  else:
    $a


proc is_valid_id*(a: Atom): bool =
  ##  ``Return`` `true` if atom is a valid id, or false otherwise.
  return a.kind == aSymbol and not is_quoted(a)


proc `$`*(obj: Atom): string =
  ##  Convert ``Atom`` to ``string``.
  case obj.kind
  of aList:                                     # List
    result = "( "
    for i in obj.list: result &= $i & " "
    result &= ")"
  of aNumber:                                   # Number
    let
      f = obj.n.toFloat
      i = obj.n.toInt
    if f - i.float != 0: return $f
    else: return $i
  of aSymbol:                                 # Symbol
    if obj.is_string:
      return obj.str_strip()
    else:
      return obj.s
  of aBool: return if obj.b: "T" else: "NIL"  # Boolean
  of aFun:                                    # Function
    if obj.f.body.len > 0:
      let args = if obj.f.args.len > 0:
          "(" & obj.f.args.foldl(a & ", " & b) & ")"
        else:
          "()"
      result = "Function " & args
    else:
      result = "Built-in function"
  of aError: return "ERROR: " & obj.e.s       # Error


# For NumberBoolFunc

proc `==`*(a, b: Number): bool =
  return rationals.`==`(a, b)


proc `!=`*(a, b: Number): bool =
  return not rationals.`==`(a, b)


# Error procs

proc `$`*(obj: Error) : string =
  return "ERROR: " & obj.s


proc write*(err: Error): Error =
  writeLine(stderr, $err)
  return err


# List procs

proc reverse*(input: seq[Atom]): seq[Atom] {.noSideEffect.} =
  ##  Reverse ``input`` sequence.
  result = @[]
  for i in countdown(input.high, 0):
    result.add(input[i])


proc cons*(elem, lst: Atom): Atom =
  ##  ``cons`` an element to the beginning of a list.
  var rev = lst.list.reverse()
  rev.add(elem)
  return atom rev.reverse()


proc snoc*(lst, elem: Atom): Atom =
  ##  ``snoc`` an element to the end of a list.
  result = atom lst.list
  result.list.add(elem)


proc car*(x: Atom): Atom =
  ##  ``Return`` head of ``Atom.list``.
  return case x.kind:
  of aList:
    if x.list.len > 0: x.list[0]
    else: atom false
  of aError:
    x
  else:
    atom error "Not a list: " & $x


proc cdr*(x: Atom): Atom =
  ##  ``Return`` tail of ``Atom.list``.
  return case x.kind:
  of aList:
    if x.list.len > 1: atom(x.list[1..^1])
    else: atom false
  of aError:
    x
  else:
    atom error "Not a list: " & $x


proc quote*(x: Atom): Atom =
  ##  ``Return`` tail of ``x.list`` as list or as single ``Atom``
  ##  (in case of tail's length of `1`).
  return case x.kind:
  of aList:
    if x.list.len != 2:
      atom error "Quote requres 1 argument"
    else:
      x.list[1]
  else:
    atom error "Illegal argument for quote"


proc toStrList*(a: Atom): seq[string] =
  ##  Convert ``Atom.list`` to a sequence of strings.
  result = @[]
  case a.kind:
    of aList:
      for i in a.list:
        if is_valid_id(i):
          result.add(i.s)
        else: return @[]
    else: return


proc take*[T](ls: seq[T], n: int = 1): seq[T] =
  return if n > ls.len: ls
         elif n < 1: @[]
         else: ls[0 .. <n]


proc takelast*[T](ls: seq[T], n: int = 1): seq[T] =
  return if n > ls.len: ls
         elif n < 1: @[]
         else: ls[^n .. ^1]


proc drop*[T](ls: seq[T], n: int = 1): seq[T] =
  return if n > ls.len: @[]
         elif n < 1: ls
         else: ls[n .. ls.high]


proc droplast*[T](ls: seq[T], n: int = 1): seq[T] =
  return if n > ls.len: @[]
         elif n < 1: ls
         else: ls[0 .. <(ls.len - n)]


# Lambda
proc lambda*(args: seq[string], body: seq[Atom], env: Env): Fun =
  return Fun(args: args, body: body, env: env)


# ENV #

proc newEnv(pairs: openArray[(string, Atom)], outer: Env = nil): Env =
  ##  Create a new ``Env``.
  Env(table: newTable[string, Atom](pairs), outer: outer)


proc `[]`(env: Env, key: string): Atom =
  ##  Get ``key`` value from thr environment ``env``.
  let key = key.toLower()
  return if env.table.contains(key): env.table[key]
         elif env.outer != nil: env.outer[key]
         else: atom error "No such variable: " & key


proc `[]=`(env: Env, key: string, val: Atom) {.inline.} =
  ##  Set ``key`` in the environment ``env`` to given ``val``.
  env.table[key.toLower] = val


# GLOBAL_ENV #

template fun_isType*(typ: AtomKind, args: openArray[Atom]): untyped =
  ##  Template for a function of type ``kind(atom): bool``.
  for i in args:
    if i.kind != typ: return atom false
  return atom true


template fun_numbers*(op: untyped, args: openArray[Atom]): untyped =
  ##  Template for a function of type ``op(num1, num2, ...): num``.
  case args[0].kind:
  of aNumber:
    result = atom args[0].n
  of aError:
    return args[0]
  else:
    return atom error "Not a number: " & $args[0]

  for i in 1..args.high:
    if args[i].kind == aNumber:
      result.n = op(result.n, args[i].n)
    else:
      return atom error "Not a number: " & $args[i]


template fun_bool*(op: untyped, args: openArray[Atom]): untyped =
  ##  Template for a function of type ``op(num1, num2, ...): bool``.
  result = Atom(kind: aBool)
  if args[0].kind notin {aNumber, aBool}:
    return atom error "Not a number nor bool: " & $args[0]

  for i in 1..args.high:
    if args[i-1].kind == aNumber:
      if args[i].kind == aNumber: result.b = op(args[i-1].n, args[i].n)
      else:
        return atom error "Not a number: " & $args[i]
    else:
      if args[i].kind == aBool: result.b = op(args[i-1].b, args[i].b)
      else:
        return atom error "Not a bool: " & $args[i]


proc fun_isBool*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_isType(aBool, args)


proc fun_isNumber*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_isType(aNumber, args)


proc fun_is_null*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len <= 0:
    return atom error "null? needs 1 argument"
  let fst = args[0]
  return case fst.kind:
  of aList:
    atom(fst.list.len <= 0)
  of aBool:
    atom(not fst.b)
  of aError:
    #fst
    atom true
  else:
    atom false


proc fun_plus*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`+`, args)


proc fun_minus*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`-`, args)


proc fun_multiply*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`*`, args)


proc fun_divide*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`/`, args)


proc fun_max*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`max`, args)


proc fun_min*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`min`, args)


proc fun_abs*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len <= 0:
    return atom error "abs needs 1 argument"
  if args[0].kind == aNumber:
    return atom abs(args[0].n)
  else:
    return atom error "Not a number: " & $args[0]


proc fun_round*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len <= 0:
    return atom error "round needs 1 argument"
  if args[0].kind == aNumber:
    return atom number((args[0].n).toInt)
  else:
    return atom error "Not a number: " & $args[0]


proc fun_mod*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 2:
    return atom error "mod needs 2 arguments"
  for i in args:
    case i.kind:
    of aNumber: continue
    of aError: return i
    else: return atom error "Not a number: " & $i
  let
    a = fun_divide([args[0], args[1]])
    b = fun_round([a])
  return atom(a.n - b.n)


type NumberBoolFunc = proc(a, b: Number): bool
proc odd_or_even*(chk: NumberBoolFunc, args: openArray[Atom]): Atom =
  for i in args:
    case i.kind:
    of aNumber:
      if chk( fun_mod([i, atom(number(2))]).n, atom(number(0)).n ):
        return atom false
    of aError:
      return i
    else:
      return atom error "Not a number: " & $i
  return atom true


proc fun_odd*(args: openArray[Atom]): Atom {.cdecl.} =
  return odd_or_even(`==`, args)


proc fun_even*(args: openArray[Atom]): Atom {.cdecl.} =
  return odd_or_even(`!=`, args)


proc fun_eq*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`==`, args)


proc fun_ne*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`!=`, args)


proc fun_gt*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`>`, args)


proc fun_lt*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`<`, args)


proc fun_ge*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`>=`, args)


proc fun_le*(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`<=`, args)


proc fun_cons*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 2:
    return atom error "cons needs 2 arguments: atom and list"
  let elem = args[0]
  let lst  = args[1]
  return case lst.kind:
  of aList:
    case elem.kind:
    of aSymbol, aNumber, aBool, aFun:
      cons(elem, lst)
    of aError:
      elem
    else:
      atom error "First argument to cons must be either a symbol or a number"
  else:
    atom error "Second argument to cons must be a list"


proc fun_snoc*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 2:
    return atom error "snoc needs 2 argument: list and atom"
  let lst = args[0]
  let elem = args[1]
  return case lst.kind:
  of aList:
    case elem.kind:
    of aSymbol, aNumber, aBool, aFun:
      snoc(lst, elem)
    of aError:
      elem
    else:
      atom error "Second argument to snoc must be a symbol or a number"
  else:
    atom error "First argument to snoc must be a list"


proc fun_car*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return atom error "car needs 1 argument"
  return case args[0].kind:
  of aList:
    car atom args[0].list
  of aError:
    args[0]
  else:
    atom error "car argument must be of list type"


proc fun_cdr*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return atom error "cdr needs 1 argument"
  return case args[0].kind:
  of aList:
    cdr atom args[0].list
  of aError:
    args[0]
  else:
    atom error "Cdr argument must be of list type"


proc fun_len*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return atom error "len needs 1 argument"
  if args[0].kind == aList:
    return atom number(len(args[0].list))
  else:
    return atom number(1)


proc fun_echo*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len < 1:
    return atom error "echo needs 1 or more arguments"
  for arg in args:
    stdout.write $arg
  stdout.write "\n"
  return args[0]


proc fun_capitalize*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return atom error "capitalize needs 1 argument"
  let fst = args[0]
  if fst.is_string:
    return atom fst.str_strip().title()
  else:
    return atom error "Not a string: " & $fst


proc fun_upcase*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return atom error "upcase needs 1 argument"
  let fst = args[0]
  if fst.is_string:
    return atom fst.s.toUpper()
  else:
    return atom error "Not a string: " & $fst


proc fun_downcase*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return atom error "downcase needs 1 argument"
  let fst = args[0]
  if fst.is_string:
    return atom fst.s.toLower()
  else:
    return atom error "Not a string: " & $fst


proc fun_length*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return atom error "length needs 1 argument"
  let fst = args[0]
  if fst.is_string:
    return atom number fst.str_strip.runeLen
  else:
    return atom error "Not a string: " & $fst


proc fun_char*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 2:
    return atom error "char needs 2 arguments"
  let fst = args[0]
  if fst.is_string:
    let str = fst.str_strip()
    if args[1].kind == aNumber:
      let idx = args[1].n.toInt
      if (idx >= 0) and (idx < str.len):
        return atom str.toRunes[idx].toUtf8
      else:
        return atom error "Out of bounds: " & $idx
    else:
      return atom error "Not a number: " & $args[1]
  else:
    return atom error "Not a string: " & $fst


proc fun_format*(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len < 1:
    return atom error "format needs 1 or more arguments"
  let fst = args[0]
  if fst.is_string:
    var subs: seq[string] = @[]
    for i in 1..args.high:
      subs.add $args[i]
    return atom(fst.str_strip() % subs)
  else:
    return atom error "Not a string: " & $fst


proc quit_with*(errorcode: int, newline = false) =
  if newline:
    echo ""
  echo "Good bye!"
  quit(errorcode)


proc fun_quit*(args: openArray[Atom]): Atom {.cdecl.} =
  ## Now we can exit properly. Yay!
  if args.len > 0:
    if args[0].kind == aNumber: # error code
      quit_with(args[0].n.toInt())
      return atom false
  quit_with(0)
  return atom false


var global_env = newEnv([
  ("t",         atom true),
  ("nil",       atom false),
  ("bool?",     atom fun_isBool),
  ("number?",   atom fun_isNumber),
  ("nil?",      atom fun_is_null),
  ("null?",     atom fun_is_null),
  ("pi",        atom number 3.141592653589793),
  ("e",         atom number 2.718281828459045),
  ("+",         atom fun_plus),
  ("-",         atom fun_minus),
  ("*",         atom fun_multiply),
  ("/",         atom fun_divide),
  ("max",       atom fun_max),
  ("min",       atom fun_min),
  ("abs",       atom fun_abs),
  ("round",     atom fun_round),
  ("mod",       atom fun_mod),
  ("odd?",      atom fun_odd),
  ("even?",     atom fun_even),
  ("=",         atom fun_eq),
  ("!=",        atom fun_ne),
  (">",         atom fun_gt),
  ("<",         atom fun_lt),
  (">=",        atom fun_ge),
  ("<=",        atom fun_le),
  ("cons",      atom fun_cons),
  ("snoc",      atom fun_snoc),
  ("car",       atom fun_car),
  ("cdr",       atom fun_cdr),
  ("len",       atom fun_len),
  ("echo",      atom fun_echo),
  ("capitalize",atom fun_capitalize),
  ("upcase",    atom fun_upcase),
  ("downcase",  atom fun_downcase),
  ("length",    atom fun_length),
  ("char",      atom fun_char),
  ("format",    atom fun_format),
  ("fmt",       atom fun_format),
  ("exit",      atom fun_quit),
  ("quit",      atom fun_quit),
  ])


# FUN #

proc eval(x: Atom, env: Env = global_env): Atom # Forward declaration


proc eval_params(fun: Fun, args: seq[Atom], outer_env: Env): Env =
  result = newEnv([])
  result.outer = fun.env
  # add aFun env to params (closure)
  # for key in fun.env.table.keys:
  #   result[key] = fun.env[key]
  # add args to params
  for i in 0..args.high:  # Eval each argument
    case args[i].kind:
    # DO NOT eval lists given as params |16.0518.2231|
    # Broke my head while trying to figure this out
    # Quoted lists passed into functions were eval'd as func's
    of aList:
      result[fun.args[i]] = args[i]
    else:
      result[fun.args[i]] = eval(args[i], outer_env)


proc call(fun: Fun, args: seq[Atom], env: Env): Atom =
  # built-in function
  if fun.body.len < 1:
    return fun.builtin(args)

  # def function
  return case cmp(args.len, fun.args.len):
  # partial application
  # returns a new function, which has some of its args filled
  of -1:
    let ngiven = args.len
    let argsleft = fun.args.drop(ngiven)
    let params = eval_params(fun, args, env)
    atom lambda(argsleft, fun.body, params)

  # normal call
  of 0:
    let params = eval_params(fun, args, env)
    let body = if fun.body.len > 1: atom(fun.body) else: fun.body[0]
    # eval(body, Env(table: params.table, outer: env))
    eval(body, params)

  # curried function call
  else:
    let cfun = fun.call(args.take(fun.args.len), env)
    let leftover = args.drop(fun.args.len)
    case cfun.kind:
    of aFun: cfun.f.call(leftover, env)
    else: eval(atom(@[cfun].concat(leftover)), env)


# PARSE #


template prepareString(str: string): string =
  str.replace("\\\"", "\"")


proc tokenize(input: string): seq[string] {.noSideEffect.} =
  ##  Convert ``input`` string into sequence of tokens.
  result = @[]
  let input = input.replace(
    "(", " ( ").replace(")", " ) ").replace("' ( ", " '( ").replace(
    "\"", " \" ").replace("\\ \" ", "\\\"")

  var
    inStr = false
    seqStr: seq[string] = @[]
  for token in input.split():
    if inStr:
      if token == "\"":
        inStr = false
        result.add("\"" & seqStr.join(" ").prepareString() & "\"")
        seqStr = @[]
      else:
        seqStr.add(token)
    else: # not inStr
      if token.len > 0:
        if token == "\"":
          inStr = true
        else:
          result.add(token)


proc parseRatio(str: string, num, den: var int): bool {.noSideEffect.} =
  ##  Parse string ("num/den") to ratio and write result to ``num`` and ``den``.
  ##
  ##  ``Return`` ``true`` on success, ``false`` otherwise.
  let s = str.split("/")
  if s.len != 2: return false
  if s[0].len < 1 or s[1].len < 1: return false
  if s[0].parseInt(num) != s[0].len: return false
  if s[1].parseInt(den) != s[1].len: return false
  return true


proc toAtom(token: string): Atom =
  ##  Parse a single ``token``.
  ##  If the token is not a number, it is a symbol.
  var
    f: float
    i, num, den: int
  if token.parseRatio(num, den): atom(num // den)         # token is ratio
  elif token.parseInt(i) == token.len: atom(number(i))    # token is int
  elif token.parseFloat(f) == token.len: atom(number(f))  # token is float
  else: Atom(kind: aSymbol, s: token)                     # token is symbol


proc addToStrSeq(strSeq: var seq[string], str: string): bool =
  result = not (str == "\"")
  if result: strSeq.add(str)


proc read(tokens: var seq[string]): Atom =
  ##  Read an expression from a sequence of ``tokens``.
  ##
  ##  ``Return`` parsed ``Atom``.
  if tokens.len == 0: # no tokens
    # return atom error "ERROR: Unexpected EOF while reading"
    return atom false

  var
    token = tokens.pop() # take next token

  if token == "(":
    result = atom()  # start new list

    while tokens.len > 0:
      if tokens[^1] == ")":
        discard tokens.pop()
        return
      result.list.add(tokens.read()) # read list items
    return atom error "Missing )"

  if token == "'(":
    result = atom()
    var lst = atom()
    result.list.add(atom("quote"))

    while tokens.len > 0:
      if tokens[^1] == ")":
        result.list.add(lst)
        discard tokens.pop()
        return
      lst.list.add(tokens.read()) # read list items
    return atom error "Missing )"

  elif token == ")":
    return atom error "Unexpected )"

  else:
    #echo token
    if token == "\"":
      var strSeq: seq[string] = @[]
      while (tokens.len > 0) and strSeq.addToStrSeq(tokens.pop()):
        discard
      token = "\"" & strSeq.join(" ") & "\""
    return token.toAtom


proc reverse[T](input: seq[T]): seq[T] {.noSideEffect.} =
  ## ``Return`` reversed ``input`` sequence.
  result = @[]
  for i in countdown(input.high, 0):
    result.add(input[i])


proc parse(input: string): Atom =
  ##  Read an expression from ``input`` string.
  ##
  ##  ``Return`` parsed ``Atom``.
  var tokens = input.tokenize().reverse()
  tokens.read()


# EVAL #

proc eval(x: Atom, env: Env = global_env): Atom =
  ##  Evaluate an expression ``x`` in the environment ``env``.
  ##
  ##  ``Return`` evaluated ``Atom``.
  case x.kind
  of aNumber, aBool: # constant literal
    return x

  of aSymbol: # variable reference
    return if is_comment(x): atom false
           elif is_quoted(x): atom x.s[1..^1]
           elif is_string(x): atom x.s
           else: env[x.s]

  of aFun:
    return x

  of aError:
    discard x.e.write
    return atom false

  of aList:
    if x.list.len < 1:
      return atom()
    let xcar = x.car
    let xcdr = case x.cdr.kind:
      of aList:
        x.cdr.list
      else: @[]

    case xcar.kind:
    of aSymbol: # special form
      # (quote exp)
      if xcar.s == "quote":
        return x.quote

      # (list exp)
      if xcar.s == "list":
        var list: seq[Atom] = @[]
        for i in xcdr:
          list.add(eval(i, env))
        return atom list

      # (if test conseq alt)
      if xcar.s == "if":
        if xcdr.len != 3:
          return atom error "if needs 3 arguments"
        else:
          let test = eval(xcdr[0], env)
          return case test.kind:
          of aBool:
            if test.b: eval(xcdr[1], env)
            else: eval(xcdr[2], env)
          of aError:
            test
          else:
            atom error "Not a bool: " & $test

      # (def name value)
      elif xcar.s == "def":
        if xcdr.len != 2:
          return atom error "def needs 2 arguments"
        let arg = xcdr[0]
        let value = xcdr[1]
        case arg.kind:
        # (def var value)
        of aSymbol:
          if not is_quoted(arg):
            env[arg.s] = eval(xcdr[1], env)
            return env[arg.s]
          else:
            return atom error "Cannot assign to quoted symbol"
        # (def (fun-name args) body)
        of aList:
          let name = arg.car
          let arglist = arg.cdr
          if is_valid_id(name):
            case value.kind:
            of aList:
              env[name.s] = atom lambda(arglist.toStrList, value.list, env)
              return env[name.s]
            else:
              return atom error "Function body should be a list of s-expressions"
          else:
            return atom error "Invalid id: " & $name
        else:
          return atom error "Def defines either vals or functions"

      # (lambda (args...) body)
      elif xcar.s == "lambda" or xcar.s == "\\":
        if xcdr.len != 2:
          return atom error "lambda needs 2 arguments: (lambda (args...) body)"
        let arglist = xcdr[0]
        let body = xcdr[1]

        return case arglist.kind:
        of aList:
          case body.kind:
          of aList:
            atom lambda(arglist.toStrList, body.list, env)
          else:
            atom error "Function body should be a list of s-expressions"
        else:
          atom error "Lambda arguments error: Must be list"

      # (fn arg body) lambda of one argument
      elif (xcar.s == "fn") or (xcar.s == "fun"):
        if xcdr.len != 2:
          return atom error "fn/fun needs 2 arguments: (fn arg body)"
        let arg = xcdr[0]
        let body = xcdr[1]
        return case body.kind:
        of aList:
          atom lambda(atom([arg]).toStrList, body.list, env)
        else:
          atom error "Fn body must be a list of s-expression"

      # (do (expr1) (expr 2) ...)
      # (begin (expr1) (expr2) ...)
      # sequential evaluation
      elif (xcar.s == "do") or (xcar.s == "begin"):
        if xcdr.len < 1:
          return atom false # empty (do) returns NIL
        # check if we have only sexps
        for sexp in xcdr.items:
          case sexp.kind:
          of aList:
            continue
          else:
            return atom error "Not a S-exp: " & $sexp

        var local_env = newEnv([])
        local_env.outer = env
        for sexp in xcdr:
          result = eval(sexp, local_env)

      # (fun arg...)
      else:
        let fun = eval(xcar, env)
        var args: seq[Atom] = @[]
        let cdr = x.cdr

        case cdr.kind:
        of aList:
          if cdr.list.len > 0:
            for i in cdr.list.items:
              args.add(eval(i, env))
          # just one argument
          else:
            args.add(eval(cdr, env))
        of aError:
          return cdr
        else:
          discard

        if fun.kind == aFun:
          return fun.f.call(args, env)
        else:
          return fun

    # list
    of aList:
      let fst = eval(xcar, env)
      return case fst.kind:
      of aFun:
        fst.f.call(xcdr, env)
      of aError:
        fst
      else:
        atom error "Not a function: " & $fst

    else:
      return atom error "Invalid function name: " & $xcar


proc sanitize(input: seq[string]): seq[string] =
  result = @[]
  for line in input:
    # comments
    let comment = line.find(';')
    if comment > -1:
      result.add line[0 .. <comment]
    # empty strings
    elif line.len > 0:
      result.add line


iterator expressions(input: string): string =
  var
    start = 0
    level = 0
  for i in 0..input.high:
    case input[i]:
    of '(':
      if level < 1:
        start = i
      inc level
    of ')':
      if level < 1:
        echo "ERROR: Unexpected )"
      dec level
      if level < 1:
        yield input[start..i]
    else:
      discard


# INTERFACE #

proc setAtom*(name: string, a: Atom) =
  ##  Set an atom in the global environment.
  global_env[name] = a


proc getAtom*(name: string): Atom =
  ##  Get an atom from the global environment.
  global_env[name]


proc execLine*(input: string): Atom =
  ##  Execute a line of code (single expression).
  return eval parse(input)


proc execCode*(input: string) =
  ##  Execute a code block (multiple expressions).
  let input = input.split(NewLines).sanitize().join()
  for line in input.expressions:
    discard execLine(line)


when isMainModule:

  # MAIN #

  proc main() =
    for i in 1..paramCount():
      try:
        execCode readFile(paramStr(i))
      except IOError:
        echo "IO error while reading from file: " & paramStr(i)
        quit_with(1)

    while true:
      write(stdout, "lisnim> ")
      try:
        writeLine(stdout, execLine readLine(stdin))
      except IOError:
        quit_with(0, true)


  main()

