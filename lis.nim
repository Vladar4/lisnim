import
  parseutils, rationals, strutils, tables


type
  Number = Rational[int]

template number(i: float): Number = toRational(i, high(int32))
template number[T](i: T): Number = toRational(i)


type
  Env = ref EnvObj
  EnvObj = object
    table: TableRef[string, Atom]
    outer: Env

  Builtin = proc(args: openArray[Atom]): Atom {.cdecl.}

  Fun = object
    args: seq[string]
    body: seq[Atom]
    builtin: Builtin

  AtomKind = enum
    aList, aNumber, aSymbol, aBool, aFun

  Atom = object
    case kind: AtomKind
    of aList: list: seq[Atom]
    of aNumber: n: Number
    of aSymbol: s: string
    of aBool: b: bool
    of aFun: f: Fun



# ATOM #

template atom(): Atom = Atom(kind: aList, list: @[])
template atom(val: seq[Atom]): Atom = Atom(kind: aList, list: val)
template atom(val: Number): Atom = Atom(kind: aNumber, n: val)
template atom(val: string): Atom = Atom(kind: aSymbol, s: val)
template atom(val: bool): Atom = Atom(kind: aBool, b: val)
proc atom(val: Fun): Atom = Atom(kind: aFun, f: val)
proc atom(builtin: Builtin): Atom =
  atom(Fun(args: @[], body: @[], builtin: builtin))
proc atom(args: seq[string], body: seq[Atom]): Atom =
  atom(Fun(args: args, body: body))


proc `$`(obj: Atom): string =
  case obj.kind
  of aList:
    result = "( "
    for i in obj.list: result &= $i & " "
    result &= ")"
  of aNumber:
    let
      f = obj.n.toFloat
      i = obj.n.toInt
    if f - i.float != 0: return $f
    else: return $i
  of aSymbol: return obj.s
  of aBool: return if obj.b: "T" else: "NIL"
  of aFun: return "Function"

proc car(x: Atom): Atom =
  if x.kind == aList:
    if x.list.len > 0: x.list[0]
    else: atom()
  else:
    writeLine(stderr, "ERROR: Not a list: " & $x)
    atom()

proc cdr(x: Atom): Atom =
  if x.kind == aList:
    if x.list.len > 2: atom(x.list[1..^1])
    elif x.list.len == 2: x.list[1]
    else: atom()
  else:
    writeLine(stderr, "ERROR: Not a list: " & $x)
    atom()



# ENV #

proc newEnv(pairs: openArray[(string, Atom)], outer: Env = nil): Env =
  Env(table: newTable[string, Atom](pairs), outer: outer)


proc `[]`(obj: Env, key: string): Atom =
  let key = key.toLower
  if obj.table.contains(key): return obj.table[key]
  elif obj.outer != nil: return obj.outer[key]
  else: return atom(key)

proc `[]=`(obj: Env, key: string, val: Atom) {.inline.} =
  obj.table[key.toLower] = val



# GLOBAL_ENV #

template fun_numbers(op: expr, args: openArray[Atom]): stmt =
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


proc fun_plus(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`+`, args)

proc fun_minus(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`-`, args)

proc fun_multiply(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`*`, args)

proc fun_divide(args: openArray[Atom]): Atom {.cdecl.} =
  fun_numbers(`/`, args)


var global_env = newEnv([
  ("t", atom(true)),
  ("nil", atom(false)),
  ("pi", atom(number(3.141592653589793))),
  ("e", atom(number(2.718281828459045))),
  ("+", atom(fun_plus)),
  ("-", atom(fun_minus)),
  ("*", atom(fun_multiply)),
  ("/", atom(fun_divide)),
  ])



# FUN #

proc eval(x: Atom, env: Env = global_env): Atom

proc call(fun: Fun, args: seq[Atom], env: Env): Atom =
  if fun.body.len < 1:
    return fun.builtin(args)
  elif args.len == fun.args.len:
    var params = newEnv([])
    for i in 0..args.high:
      params[fun.args[i]] = args[i]
    let body = if fun.body.len > 1: atom(fun.body) else: fun.body[0]
    return eval(body, Env(table: params.table, outer: env))
  else:
    writeLine(stderr, "ERROR: Invalid arguments count: " &
      $args.len & " instead of " & $fun.args.len)



# PARSE #

proc tokenize(input: string): seq[string] {.noSideEffect.} =
  ##  Convert string into sequence of tokens.
  input.replace("(", " ( ").replace(")", " ) ").split()


proc parseRatio(str: string, num, den: var int): bool {.noSideEffect.} =
  ##  Parse string ("num/den") to ratio and write result to ``num`` and ``den``.
  ##  Return ``true`` on success, ``false`` otherwise.
  let s = str.split("/")
  if s.len != 2: return false
  if s[0].len < 1 or s[1].len < 1: return false
  if s[0].parseInt(num) != s[0].len: return false
  if s[1].parseInt(den) != s[1].len: return false
  return true


proc toAtom(token: string): Atom =
  ##  Parse single token. If token is not a number, it is a symbol.
  var
    f: float
    i, num, den: int
  if token.parseRatio(num, den): atom(num // den)         # token is ratio
  elif token.parseInt(i) == token.len: atom(number(i))    # token is int
  elif token.parseFloat(f) == token.len: atom(number(f))  # token is float
  else: Atom(kind: aSymbol, s: token)                     # token is symbol


proc read(tokens: var seq[string]): Atom =
  ##  Read an expression from a sequence of tokens.
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
  ## Reverse given sequence.
  result = @[]
  for i in countdown(input.high, 0):
    result.add(input[i])


proc parse(input: string): Atom =
  ##  Read an expression from ``input`` string.
  var tokens = input.tokenize().reverse()
  tokens.read()



# EVAL #

proc eval(x: Atom, env: Env = global_env): Atom =
  ##  Evalueate an expression in an environment.
  case x.kind
  of aList:
    if x.list.len < 1:
      return atom()
    let car = x.car
    case car.kind
    of aSymbol: # symbol

      if car.s == "quote":  # (quote exp)
        return x.cdr

      if car.s == "list": # (list exp)
        var list: seq[Atom] = @[]
        for i in x.cdr.list:
          list.add(i)
        return atom(list)

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
        return atom(true)

      else: # (fun arg...)
        let fun = eval(car, env)
        var args: seq[Atom] = @[]
        if x.cdr.list.len > 0:
          for i in x.cdr.list.items:
            args.add(eval(i, env))
        if fun.kind == aFun: return fun.f.call(args, env)
        else: return fun

    of aList: # list
      return eval(car, env)

    else:
      echo x
      writeLine(stderr, "ERROR: Invalid function name: " & $car)

  of aNumber, aBool: # constant literal
    return x

  of aSymbol: # variable reference
    return env[x.s]

  of aFun:
    return x.f.call(@[], env)



# MAIN #

proc main() =
  while true:
    write(stdout, "lisnim> ")
    echo eval(parse(readLine(stdin)))


main()
