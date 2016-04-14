import
  os, parseutils, rationals, strutils, tables



# NUMBER #

type
  Number = Rational[int]

template number(i: float): Number = toRational(i, high(int32))
template number[T](i: T): Number = toRational(i)



# ENV, BUILTIN, FUN, ATOM #

type
  Env = ref EnvObj  ##  Environment for ``eval()``
  EnvObj = object
    table: TableRef[string, Atom]
    outer: Env

  Builtin = proc(args: openArray[Atom]): Atom {.cdecl.} ##  Built-in proc type

  Fun = object  ##  Function object
    args: seq[string] ##  Function arguments (for user-defined functions)
    body: seq[Atom]   ##  Function body (for user-defined functions)
    builtin: Builtin  ##  Function reference (for built-in functions)

  AtomKind = enum
    aList, aNumber, aSymbol, aBool, aFun

  Atom = object ##  Atom type
    case kind: AtomKind ##  Variant of
    of aList: list: seq[Atom] ##  List of atoms
    of aNumber: n: Number     ##  Number
    of aSymbol: s: string     ##  Symbol
    of aBool: b: bool         ##  Boolean
    of aFun: f: Fun           ##  Function



# ATOM #

template atom(): Atom = Atom(kind: aList, list: @[])
template atom(val: seq[Atom]): Atom = Atom(kind: aList, list: val)
template atom(val: openArray[Atom]): Atom = Atom(kind: aList, list: @val)
template atom(val: Number): Atom = Atom(kind: aNumber, n: val)
template atom(val: string): Atom = Atom(kind: aSymbol, s: val)
template atom(val: bool): Atom = Atom(kind: aBool, b: val)
proc atom(val: Fun): Atom = Atom(kind: aFun, f: val)
proc atom(builtin: Builtin): Atom =
  atom(Fun(args: @[], body: @[], builtin: builtin))
proc atom(args: seq[string], body: seq[Atom]): Atom =
  atom(Fun(args: args, body: body))


proc `$`(obj: Atom): string =
  ##  Convert ``Atom`` to ``string``.
  case obj.kind
  of aList:                                   # List
    result = "( "
    for i in obj.list: result &= $i & " "
    result &= ")"
  of aNumber:                                 # Number
    let
      f = obj.n.toFloat
      i = obj.n.toInt
    if f - i.float != 0: return $f
    else: return $i
  of aSymbol: return obj.s                    # Symbol
  of aBool: return if obj.b: "T" else: "NIL"  # Boolean
  of aFun: return "Function"                  # Function

proc car(x: Atom): Atom =
  ##  ``Return`` head of ``Atom.list``.
  if x.kind == aList:
    if x.list.len > 0: x.list[0]
    else: atom()
  else:
    writeLine(stderr, "ERROR: Not a list: " & $x)
    atom()

proc cdr(x: Atom): Atom =
  ##  ``Return`` tail of ``Atom.list``.
  if x.kind == aList:
    if x.list.len > 1: atom(x.list[1..^1])
    else: atom()
  else:
    writeLine(stderr, "ERROR: Not a list: " & $x)
    atom()

proc quote(x: Atom): Atom =
  ##  ``Return`` tail of ``x.list`` as list or as single ``Atom``
  ##  (in case of tail's length of 1).
  if x.kind == aList:
    if x.list.len > 2: atom(x.list[1..^1])
    elif x.list.len == 2: x.list[1] # tail is 1 atom long
    else: atom()
  else:
    writeLine(stderr, "ERROR: Not a list: " & $x)
    atom()



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
    writeLine(stderr, "ERROR: no such variable: ", key)
    return atom(key)

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
  if args[0].kind == aNumber:
    result = atom(args[0].n)
  else:
    writeLine(stderr, "ERROR: Not a number: " & $args[0])
    return atom()

  for i in 1..args.high:
    if args[i].kind == aNumber:
      result.n = op(result.n, args[i].n)
    else:
      writeLine(stderr, "ERROR: Not a number: " & $args[i])
      return atom()

template fun_bool(op: expr, args: openArray[Atom]): stmt =
  ##  Template for functions of type op(num1, num2, ...): bool
  result = Atom(kind: aBool)
  if args[0].kind notin {aNumber, aBool}:
    writeLine(stderr, "ERROR: Not a number nor bool: " & $args[0])
    return atom()

  for i in 1..args.high:
    if args[0].kind == aNumber:
      if args[i].kind == aNumber: result.b = op(args[0].n, args[i].n)
      else:
        writeLine(stderr, "ERROR: Not a number: " & $args[i])
        return atom()
    else:
      if args[i].kind == aBool: result.b = op(args[0].b, args[i].b)
      else:
        writeLine(stderr, "ERROR: Not a bool: " & $args[i])
        return atom()

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
  if args.len > 1:
    writeLine(stderr, "ERROR: Abs needs 1 argument")
    return atom()
  if args[0].kind == aNumber:
    return atom(abs(args[0].n))
  else:
    writeLine(stderr, "ERROR: Not a number: ", $args[0])
    return atom()

proc fun_round(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len > 1:
    writeLine(stderr, "ERROR: Abs needs 1 argument")
    return atom()
  if args[0].kind == aNumber:
    return atom(number((args[0].n).toInt))
  else:
    writeLine(stderr, "ERROR: Not a number: ", $args[0])
    return atom()

proc fun_mod(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len != 2:
    writeLine(stderr, "ERROR: Mod needs 2 arguments")
    return atom()
  for i in args:
    if i.kind != aNumber:
      writeLine(stderr, "ERROR: Not a number: ", $i)
      return atom()
  let
    a = fun_divide([args[0], args[1]])
    b = fun_round([a])
  return atom(a.n - b.n)

proc fun_odd(args: openArray[Atom]): Atom {.cdecl.} =
  for i in args:
    if i.kind != aNumber:
      writeLine(stderr, "ERROR: Not a number: ", $i)
      return atom(false)
    if fun_mod([i, atom(number(2))]).n == number(0):
      return atom(false)
  return atom(true)

proc fun_even(args: openArray[Atom]): Atom {.cdecl.} =
  for i in args:
    if i.kind != aNumber:
      writeLine(stderr, "ERROR: Not a number: ", $i)
      return atom(false)
    if fun_mod([i, atom(number(2))]).n != number(0):
      return atom(false)
  return atom(true)

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

proc fun_car(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len > 1:
    writeLine(stderr, "ERROR: Car needs 1 argument")
    return atom()
  car(atom(args[0].list))

proc fun_cdr(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len > 1:
    writeLine(stderr, "ERROR: Cdr needs 1 argument")
    return atom()
  cdr(atom(args[0].list))

proc fun_len(args: openArray[Atom]): Atom {.cdecl.} =
  if args.len > 1:
    writeLine(stderr, "ERROR: Len needs 1 argument")
    return atom()
  if args[0].kind == aList:
    return atom(number(len(args[0].list)))
  else:
    return atom(number(1))


var global_env = newEnv([
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
  ("car", atom(fun_car)),
  ("cdr", atom(fun_cdr)),
  ("len", atom(fun_len)),
  ])



# FUN #

proc eval(x: Atom, env: Env = global_env): Atom # Forward declaration

proc call(fun: Fun, args: seq[Atom], env: Env): Atom =
  if fun.body.len < 1:  # built-in function
    return fun.builtin(args)
  elif args.len == fun.args.len:  # defun function
    var params = newEnv([])
    for i in 0..args.high:  # Eval each argument
      params[fun.args[i]] = eval(args[i], env)
    let body = if fun.body.len > 1: atom(fun.body) else: fun.body[0]
    return eval(body, Env(table: params.table, outer: env))
  else:
    writeLine(stderr, "ERROR: Invalid arguments count: " &
      $args.len & " instead of " & $fun.args.len)



# PARSE #

proc tokenize(input: string): seq[string] {.noSideEffect.} =
  ##  Convert ``input`` string into sequence of tokens.
  input.replace("(", " ( ").replace(")", " ) ").split()


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
    writeLine(stderr, "ERROR: Unexpected EOF while reading")
    return Atom(kind: aList, list: @[]) # return empty list

  var token = tokens.pop() # take next token

  if token == "(":
    result = Atom(kind: aList, list: @[])  # start new list

    while tokens.len > 0:
      if tokens[^1] == ")":
        discard tokens.pop()
        return
      result.list.add(tokens.read()) # read list items
    writeLine(stderr, "ERROR: Missing )")
    return

  elif token == ")":
    writeLine(stderr, "ERROR: Unexpected )")

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
  ##  Evalueate an expression ``x`` in an environment ``env``.
  ##
  ##  ``Return`` evaluated ``Atom``.
  case x.kind
  of aList:
    if x.list.len < 1:
      return atom()
    let car = x.car
    case car.kind
    of aSymbol: # symbol

      if car.s == "quote":  # (quote exp)
        return x.quote

      if car.s == "list": # (list exp)
        var list: seq[Atom] = @[]
        for i in x.cdr.list:
          list.add(eval(i, env))
        return atom(list)

      if car.s == "if": # (if test conseq alt)
        let cdr = x.cdr.list
        if cdr.len != 3:
          writeLine(stderr, "ERROR: If needs 3 arguments")
          return atom(false)
        else:
          let test = eval(cdr[0], env)
          if test.kind != aBool:
            writeLine(stderr, "ERROR: Not a bool: " & $test)
            return atom(false)
          if test.b: return eval(cdr[1], env)
          else: return eval(cdr[2], env)

      elif car.s == "define": # (define name value)
        let cdr = x.cdr.list
        if cdr.len != 2:
          writeLine(stderr, "ERROR: Define needs 2 arguments")
          return atom(false)
        else:
          let name = cdr[0]
          if name.kind != aSymbol:
            writeLine(stderr, "ERROR: Not a symbol: " & $name)
            return atom(false)
          else:
            env[name.s] = eval(cdr[1], env)
        return atom(true)

      elif car.s == "defun": # (defun name (args...) body)
        let cdr = x.cdr.list
        if cdr.len < 3:
          writeLine(stderr, "ERROR: Defun needs at least 3 arguments")
          return atom(false)
        let name = cdr[0]
        if name.kind != aSymbol:
          writeLine(stderr, "ERROR: Not a symbol: " & $name)
          return atom(false)
        let arglist = cdr[1]
        var args: seq[string] = @[]
        if arglist.kind != aList:
          writeLine(stderr, "ERROR: Missing arguments list for ", name.s)
          return atom(false)
        for i in arglist.list:
          if i.kind != aSymbol:
            writeLine(stderr, "ERROR: Not a symbol: " & $i)
            return atom(false)
          args.add(i.s)
        let body = cdr[2..^1]
        env[name.s] = atom(args, body)
        return (atom(name.s.toUpper))

      else: # (fun arg...)
        let fun = eval(car, env)
        var args: seq[Atom] = @[]
        if x.cdr.list.len > 0:
          for i in x.cdr.list.items:
            args.add(eval(i, env))
        else: # just one argument
          args.add(eval(x.cdr, env))
        if fun.kind == aFun:
          return fun.f.call(args, env)
        else:
          return fun

    of aList: # list
      return eval(car, env)

    else:
      return x
      #writeLine(stderr, "ERROR: Invalid function name: " & $car)

  of aNumber, aBool: # constant literal
    return x

  of aSymbol: # variable reference
    return env[x.s]

  of aFun:
    return x.f.call(@[], env)



# MAIN #

proc main() =
  for i in 1..paramCount():
    writeLine(stdout, eval(parse(readFile(paramStr(i)))))

  while true:
    write(stdout, "lisnim> ")
    writeLine(stdout, eval(parse(readLine(stdin))))


main()
