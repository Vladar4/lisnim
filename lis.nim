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
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source distribution.
# 
# Vladar vladar4@gmail.com

# Plainly marked altered version by Esgorhannoth esgorhannoth{atsign}gmail{dotcom} : |16.0516.1023|


import
  os, parseutils, rationals, strutils, tables, sequtils



# NUMBER #

type
  Number = Rational[int]

template number(i: float): Number = toRational(i, high(int32))
template number[T](i: T): Number = toRational(i)



# ENV, BUILTIN, ERROR, FUN, ATOM #

type
  Env = ref EnvObj  ##  Environment for ``eval()``
  EnvObj = object
    table: TableRef[string, Atom]
    outer: Env

  Builtin = proc(args: openArray[Atom]): Atom {.cdecl.} ##  Built-in proc type

  Error = object   ## Error object
    s: string

  Fun = object  ##  Function object
    args: seq[string] ##  Function arguments (for user-defined functions)
    body: seq[Atom]   ##  Function body (for user-defined functions)
    builtin: Builtin  ##  Function reference (for built-in functions)
    env:  Env         ##  Env the function was defined in (for user-defined functions)

  AtomKind = enum
    aList, aNumber, aSymbol, aBool, aFun, aError

  Atom = object ##  Atom type
    case kind: AtomKind ##  Variant of
    of aList: list: seq[Atom] ##  List of atoms
    of aNumber: n: Number     ##  Number
    of aSymbol: s: string     ##  Symbol
    of aBool: b: bool         ##  Boolean
    of aFun: f: Fun           ##  Function
    of aError: e: Error       ##  Error

  NumberBoolFunc = proc(a, b: Number): bool

# ATOM #

proc reverse(input: seq[Atom]): seq[Atom] {.noSideEffect.} =
  result = @[]
  for i in countdown(input.high, 0):
    result.add(input[i])

template atom(): Atom = Atom(kind: aList, list: @[])
template atom(val: seq[Atom]): Atom = Atom(kind: aList, list: val)
template atom(val: openArray[Atom]): Atom = Atom(kind: aList, list: @val)
template atom(val: Number): Atom = Atom(kind: aNumber, n: val)
template atom(val: string): Atom = Atom(kind: aSymbol, s: val)
template atom(val: bool): Atom = Atom(kind: aBool, b: val)
proc atom(val: Fun): Atom = Atom(kind: aFun, f: val)
proc atom(err: Error): Atom = Atom(kind: aError, e: err)
proc atom(builtin: Builtin): Atom =
  atom(Fun(args: @[], body: @[], builtin: builtin))
proc atom(args: seq[string], body: seq[Atom], env: Env): Atom =
  atom(Fun(args: args, body: body, env: env))
proc err(val: string): Atom = Atom(kind: aError, e: Error(s: val))

proc `$`(obj: Atom): string =
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
  of aSymbol: return obj.s                      # Symbol
  of aBool: result = if obj.b: "T" else: "NIL"  # Boolean
  of aFun:                                      # Function
    if obj.f.body.len > 0:
      let args = if obj.f.args.len > 0:
                   "(" & obj.f.args.foldl(a & ", " & b) & ")"
                 else: "()"
      result = "Fn " & args
    else:
      result = "Built-in function"
  of aError: return "ERROR: " & obj.e.s         # Error


# For NumberBoolFunc
proc `==`(a, b: Number): bool =
  return rationals.`==`(a, b)

proc `!=`(a, b: Number): bool =
  return not rationals.`==`(a, b)


# Error procs
proc `$`(obj: Error) : string =
  return "ERROR: " & obj.s

proc write(err: Error): Error =
  writeLine(stderr, $err)
  return err

# Symbol procs
proc is_quoted(a: Atom): bool =
  return case a.kind:
    of aSymbol: a.s[0] == '\''
    else: false

proc is_comment(a: Atom): bool =
  return case a.kind:
    of aSymbol: a.s[0] == ';'
    else: false

proc is_valid_id(a: Atom): bool =
  return a.kind == aSymbol and not is_quoted(a)

# List procs
proc cons(elem, lst: Atom): Atom =
  ## `cons` an element to the beginning of a list
  var rev = lst.list.reverse()
  rev.add(elem)
  return atom(rev.reverse())

proc car(x: Atom): Atom =
  ##  ``Return`` head of ``Atom.list``.
  case x.kind:
    of aList:
      if x.list.len > 0: x.list[0]
      else: atom(false)
    of aError: x
    else: err("Not a list: " & $x)

proc cdr(x: Atom): Atom =
  ##  ``Return`` tail of ``Atom.list``.
  case x.kind:
    of aList:
      if x.list.len > 1: atom(x.list[1..^1])
      else: atom(false)
    of aError: x
    else: err("Not a list: " & $x)

proc quote(x: Atom): Atom =
  ##  ``Return`` tail of ``x.list`` as list or as single ``Atom``
  ##  (in case of tail's length of 1).
  case x.kind:
    of aList:
      if x.list.len != 2:
        return err("Quote requres 1 argument")
      else:
        return x.list[1]
    else: return err("Illegal argument for quote")

proc toStrList(a: Atom): seq[string] =
  result = @[]
  case a.kind:
    of aList:
      for i in a.list:
        if is_valid_id(i):
          result.add(i.s)
        else: return @[]
    else: return

proc take[T](ls: seq[T], n: int = 1): seq[T] =
  return if n > ls.len: ls
         elif n < 1: @[]
         else: ls[0 .. <n]

proc takelast[T](ls: seq[T], n: int = 1): seq[T] =
  return if n > ls.len: ls
         elif n < 1: @[]
         else: ls[^n .. ^1]

proc drop[T](ls: seq[T], n: int = 1): seq[T] =
  return if n > ls.len: @[]
         elif n < 1: ls
         else: ls[n .. ls.high]

proc droplast[T](ls: seq[T], n: int = 1): seq[T] =
  return if n > ls.len: @[]
         elif n < 1: ls
         else: ls[0 .. <(ls.len - n)]


# Lambda
proc mklambda(args: seq[string], body: seq[Atom], env: Env): Fun =
  return Fun(args: args, body: body, env: env)



# ENV #

proc newEnv(pairs: openArray[(string, Atom)], outer: Env = nil): Env =
  ##  Create new ``Env``.
  Env(table: newTable[string, Atom](pairs), outer: outer)

proc `[]`(obj: Env, key: string): Atom =
  ##  Get ``key`` value from environment.
  let key = key.toLower
  if obj.table.contains(key): return obj.table[key]
  elif obj.outer != nil: return obj.outer[key]
  else:
    return err("No such variable: " & key)

proc `[]=`(obj: Env, key: string, val: Atom) {.inline.} =
  ##  Set ``key`` in environment to given ``val``.
  obj.table[key.toLower] = val



# GLOBAL_ENV #

template fun_isType(typ: AtomKind, args: openArray[Atom]): stmt =
  ##  Template for function of type kind(atom): bool.
  for i in args:
    if i.kind != typ: return atom(false)
  return atom(true)

template fun_numbers(op: expr, args: openArray[Atom]): stmt =
  ##  Template for functions of type op(num1, num2, ...): num.
  case args[0].kind:
    of aNumber:
      result = atom(args[0].n)
    of aError: return args[0]
    else: return err("Not a number: " & $args[0])

  for i in 1..args.high:
    if args[i].kind == aNumber:
      result.n = op(result.n, args[i].n)
    else:
      return err("Not a number: " & $args[i])

template fun_bool(op: expr, args: openArray[Atom]): stmt =
  ##  Template for functions of type op(num1, num2, ...): bool
  result = Atom(kind: aBool)
  if args[0].kind notin {aNumber, aBool}:
    return err("Not a number nor bool: " & $args[0])

  for i in 1..args.high:
    if args[0].kind == aNumber:
      if args[i].kind == aNumber: result.b = op(args[0].n, args[i].n)
      else:
        return err("Not a number: " & $args[i])
    else:
      if args[i].kind == aBool: result.b = op(args[0].b, args[i].b)
      else:
        return err("Not a bool: " & $args[i])

proc fun_isBool(args: openArray[Atom]): Atom {.cdecl.} =
  fun_isType(aBool, args)

proc fun_isNumber(args: openArray[Atom]): Atom {.cdecl.} =
  fun_isType(aNumber, args)

proc fun_plus(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`+`, args)

proc fun_minus(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`-`, args)

proc fun_multiply(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`*`, args)

proc fun_divide(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`/`, args)

proc fun_max(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`max`, args)

proc fun_min(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`min`, args)

proc fun_abs(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len <= 0:
    return err("Abs needs 1 argument")
  return case args[0].kind:
    of aNumber: atom(abs(args[0].n))
    of aError: args[0]
    else: err("Not a number: " & $args[0])

proc fun_round(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len <= 0:
    return err("Round needs 1 argument")
  return case args[0].kind:
    of aNumber: atom(number((args[0].n).toInt))
    of aError: args[0]
    else: err("Not a number: " & $args[0])

proc fun_mod(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 2:
    return err("Mod needs 2 arguments")
  for i in args:
    case i.kind:
      of aNumber: continue
      of aError: return i
      else: return err("Not a number: " & $i)
  let
    a = fun_divide([args[0], args[1]])
    b = fun_round([a])
  return atom(a.n - b.n)

proc odd_or_even(chk: NumberBoolFunc, args: openArray[Atom]): Atom =
  for i in args:
    case i.kind:
      of aNumber:
        if chk( fun_mod([i, atom(number(2))]).n, atom(number(0)).n ):
          return atom(false)
      of aError: return i
      else: return err("Not a number: " & $i)
  return atom(true)

proc fun_odd(args: openArray[Atom]): Atom {.cdecl.} =
  return odd_or_even(`==`, args)

proc fun_even(args: openArray[Atom]): Atom {.cdecl.} =
  return odd_or_even(`!=`, args)

proc fun_eq(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`==`, args)

proc fun_ne(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`!=`, args)

proc fun_gt(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`>`, args)

proc fun_lt(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`<`, args)

proc fun_ge(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`>=`, args)

proc fun_le(args: openArray[Atom]): Atom {.cdecl.} =
  fun_bool(`<=`, args)

proc fun_cons(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 2:
    return err("Cons requires 2 arguments: atom and list")
  let elem = args[0]
  let lst  = args[1]
  return case lst.kind:
    of aList:
      case elem.kind:
        of aSymbol, aNumber, aBool, aFun:
          cons(elem, lst)
        of aError: elem
        else: err("First argument to `cons` must be either a symbol or a number")
    else:
      err("Second argument to `cons` must be a list")

proc fun_car(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return err("Car needs 1 argument")
  case args[0].kind:
    of aList: return car(atom(args[0].list))
    of aError: return args[0]
    else: return err("Car argument must be of list type")

proc fun_cdr(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return err("Cdr needs 1 argument")
  case args[0].kind:
    of aList: return cdr(atom(args[0].list))
    of aError: return args[0]
    else: return err("Cdr argument must be of list type")
  

proc fun_len(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return err("Len needs 1 argument")
  if args[0].kind == aList:
    return atom(number(len(args[0].list)))
  else:
    return atom(number(1))

proc fun_is_null(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len <= 0:
    return err("null? requires 1 argument")
  let fst = args[0]
  return case fst.kind:  # TODO refactor everywhere like this
    of aList: atom(fst.list.len <= 0)
    of aBool: atom(not fst.b)
    of aError: fst
    else: atom(false)

proc fun_echo(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 1:
    return err("Echo requires 1 argument")
  let fst = args[0]
  echo $fst
  return fst

proc quit_with(errorcode: int, newline = false) =
  if newline:
    echo ""
  echo "Good bye!"
  quit(errorcode)

proc fun_quit(args: openArray[Atom]): Atom {.cdecl.} =
  ## Now we can exit properly. Yay!
  if args.len > 0:
    if args[0].kind == aNumber: # error code
      quit_with(args[0].n.toInt())
      return atom(false)
  quit_with(0)
  return atom(false)


var global_env = newEnv([
  ("quit", atom(fun_quit)),
  ("exit", atom(fun_quit)),
  ("t", atom(true)),
  ("nil", atom(false)),
  ("bool?", atom(fun_isBool)),
  ("number?", atom(fun_isNumber)),
  ("pi", atom(number(3.141592653589793))),
  ("e", atom(number(2.718281828459045))),
  ("+", atom(fun_plus)),
  ("-", atom(fun_minus)),
  ("*", atom(fun_multiply)),
  ("/", atom(fun_divide)),
  ("max", atom(fun_max)),
  ("min", atom(fun_min)),
  ("abs", atom(fun_abs)),
  ("round", atom(fun_round)),
  ("mod", atom(fun_mod)),
  ("odd?", atom(fun_odd)),
  ("even?", atom(fun_even)),
  ("=", atom(fun_eq)),
  ("!=", atom(fun_ne)),
  (">", atom(fun_gt)),
  ("<", atom(fun_lt)),
  (">=", atom(fun_ge)),
  ("<=", atom(fun_le)),
  ("cons", atom(fun_cons)),
  ("car", atom(fun_car)),
  ("cdr", atom(fun_cdr)),
  ("len", atom(fun_len)),
  ("null?", atom(fun_is_null)),
  ("nil?", atom(fun_is_null)),
  ("echo", atom(fun_echo)),
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
      of aList: result[fun.args[i]] = args[i]
      else: result[fun.args[i]] = eval(args[i], outer_env)

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
      atom(mklambda(argsleft, fun.body, params))
    
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

proc tokenize(input: string): seq[string] {.noSideEffect.} =
  ##  Convert ``input`` string into sequence of tokens.
  input.replace("(", " ( ").replace(")", " ) ").replace("' ( ", " '( ").split(Whitespace + NewLines)


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
  ##  Parse single ``token``. If token is not a number, it is a symbol.
  var
    f: float
    i, num, den: int
  if token.parseRatio(num, den): atom(num // den)         # token is ratio
  elif token.parseInt(i) == token.len: atom(number(i))    # token is int
  elif token.parseFloat(f) == token.len: atom(number(f))  # token is float
  else: Atom(kind: aSymbol, s: token)                     # token is symbol


proc read(tokens: var seq[string]): Atom =
  ##  Read an expression from a sequence of ``tokens``.
  ##
  ##  ``Return`` parsed ``Atom``.
  if tokens.len == 0: # no tokens
    # return err("ERROR: Unexpected EOF while reading")
    return atom(false)

  var token = tokens.pop() # take next token

  if token == "(":
    result = atom()  # start new list

    while tokens.len > 0:
      if tokens[^1] == ")":
        discard tokens.pop()
        return
      result.list.add(tokens.read()) # read list items
    return err("Missing )")

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
    return err("Missing )")

  elif token == ")":
    return err("Unexpected )")

  else:
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
  ##  Evaluate an expression ``x`` in an environment ``env``.
  ##
  ##  ``Return`` evaluated ``Atom``.
  case x.kind
  of aNumber, aBool: # constant literal
    return x

  of aSymbol: # variable reference
    return if is_comment(x): atom(false)
           elif is_quoted(x): atom(x.s[1..^1])
           else: env[x.s]

  of aFun:
    return x

  of aError:
    discard x.e.write
    return atom(false)

  of aList:
    if x.list.len < 1:
      return atom()
    let xcar = x.car
    let xcdr = case x.cdr.kind:
      of aList:
        x.cdr.list
      else: @[]
    case xcar.kind
    of aSymbol: # special form

      # (quote exp)
      if xcar.s == "quote":
        return x.quote

      # (list exp)
      elif xcar.s == "list":
        var list: seq[Atom] = @[]
        for i in xcdr:
          list.add(eval(i, env))
        return atom(list)

      # (if test conseq alt)
      elif xcar.s == "if":
        if xcdr.len != 3:
          return err("If needs 3 arguments")
        else:
          let test = eval(xcdr[0], env)
          return case test.kind:
            of aBool:
              if test.b: eval(xcdr[1], env)
              else: eval(xcdr[2], env)
            of aError: test
            else: err("Not a bool: " & $test)

      # (def name value)
      elif xcar.s == "def":
        if xcdr.len != 2:
          return err("Def needs 2 arguments")
        let arg = xcdr[0]
        let value = xcdr[1]
        case arg.kind:
        # (def var value)
        of aSymbol:
          if not is_quoted(arg):
            env[arg.s] = eval(xcdr[1], env)
            return env[arg.s]
          else:
            return err("Cannot assign to quoted symbol")
        # (def (fun-name args) body)
        of aList:
          let name = arg.car
          let arglist = arg.cdr
          if is_valid_id(name):
            case value.kind:
              of aList:
                env[name.s] = atom(mklambda(arglist.toStrList, value.list, env))
                return env[name.s]
              else: return err("Function body must be a list of s-expressions")
          else: return err("Invalid id: " & $name)
        else:
          return err("Def defines either vals or functions")

      # (lambda (name args...) body)
      elif xcar.s == "lambda" or xcar.s == "\\":
        if xcdr.len != 2:
          return err("Lambda needs 2 arguments: (lambda (args...) body)")
        let arglist = xcdr[0]
        let body = xcdr[1]
        return case arglist.kind:
        of aList:
            case body.kind:
              of aList: atom(mklambda(arglist.toStrList, body.list, env))
              else: err("Function body must be a list of s-expressions")
        else: err("Lambda arguments error: Must be list")

      # (fn arg body) lambda of one argument
      elif xcar.s == "fn" or xcar.s == "fun":
        if xcdr.len != 2:
          return err("Fn/Fun requires 2 arguments: (fn arg body)")
        let arg = xcdr[0]
        let body = xcdr[1]
        return case body.kind:
        of aList: atom(mklambda(atom([arg]).toStrList, body.list, env))
        else: err("Fn body must be a list of s-expressions")

      # (do (expr1) (expr2) ...)
      # (begin (expr1) (expr2) ...)
      # sequential evaluation
      elif xcar.s == "do" or xcar.s == "begin":
        if xcdr.len < 1:
          return atom(false)  # empty (do) returns NIL
        # check if we have only sexps...
        for sexp in xcdr.items:
          case sexp.kind:
            of aList: continue
            else: return err("Not a S-exp: " & $sexp)
        # let's rock
        var local_env = newEnv([])  # we'll add to it as we traverse the expressions
        local_env.outer = env
        for sexp in xcdr:
          result = eval(sexp, local_env)

      # (customfun arg...)
      else:
        let fun = eval(xcar, env)
        var args: seq[Atom] = @[]
        let cdr = x.cdr
        case cdr.kind:
          of aList:
            if cdr.list.len > 0:
              for i in cdr.list.items:
                args.add(eval(i, env))
            else: # just one argument
              args.add(eval(cdr, env))
          of aError: return cdr
          else: discard
        if fun.kind == aFun:
          return fun.f.call(args, env)
        else:
          return fun

    of aList: # list
      let fst = eval(xcar, env)
      case fst.kind:
        of aFun: return fst.f.call(xcdr, env)
        of aError: return fst
        else: return err("Not a function: " & $fst)

    else:
      return err("Invalid function name: " & $xcar)

# UTILITY #

proc remove_comments(s: seq[string]): seq[string] =
  ## Does not account for ; being inside strings,
  ## because there are no strings for now
  result = @[]
  for i in s:
    let semi = i.find(';') # semicolon position in line, if any
    result.add(if semi > -1: i[0 .. <semi] else: i)

# MAIN #

proc main() =
  if paramCount() > 0:
    for i in 1..paramCount():
      try:
        let ls = readFile(paramStr(i)).split(NewLines).remove_comments().join()
        let wrapped = "(do " & ls & ")"
        discard eval(parse(wrapped))
      except IOError:
        echo "IO error while reading from file: " & paramStr(i)

  else:  # paramCount() == 0
    while true:
      write(stdout, "lisnim> ")
      try:
        writeLine(stdout, eval(parse(readLine(stdin))))
      except IOError:
        quit_with(0, true)


main()
