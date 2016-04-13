import
  parseutils, rationals, strutils, tables


type
  Number = Rational[int]

template number(i: float): Number = toRational(i, high(int32))
template number[T](i: T): Number = toRational(i)


type
  Fun = proc(args: openArray[Atom]): Atom {.cdecl.}

  AtomKind = enum
    aList, aNumber, aSymbol, aBool, aFun

  Atom = object
    case kind: AtomKind
    of aList: list: seq[Atom]
    of aNumber: n: Number
    of aSymbol: s: string
    of aBool: b: bool
    of aFun: f: Fun

template atom(): Atom = Atom(kind: aList, list: @[])
template atom(val: seq[Atom]): Atom = Atom(kind: aList, list: val)
template atom(val: Number): Atom = Atom(kind: aNumber, n: val)
template atom(val: string): Atom = Atom(kind: aSymbol, s: val)
template atom(val: bool): Atom = Atom(kind: aBool, b: val)
template atom(val: Fun): Atom = Atom(kind: aFun, f: val)

proc `$`(obj: Atom): string =
  case obj.kind
  of aList:
    result = "("
    for i in obj.list: result &= $i
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
    if x.list.len > 1: atom(x.list[1..^1])
    else: atom()
  else:
    writeLine(stderr, "ERROR: Not a list: " & $x)
    atom()



# ENV #


type
  Env = ref EnvObj
  EnvObj = object
    table: TableRef[string, Atom]
    outer: Env

proc newEnv(pairs: openArray[(string, Atom)], outer: Env = nil): Env =
  Env(table: newTable[string, Atom](pairs), outer: outer)

proc `[]`(obj: Env, key: string): Atom =
  let key = key.toLower
  if obj.table.contains(key): return obj.table[key]
  elif obj.outer != nil: return obj.outer[key]
  else: return atom(false)

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
    let car = x.car
    case car.kind
    of aSymbol: # function
      if car.s == "quote":  # (quote exp)
        return x.cdr
      else:
        let car = eval(car, env)
        var cdr: seq[Atom] = @[]
        if x.cdr.list.len > 0:
          for i in x.cdr.list.items:
            cdr.add(eval(i, env))
        if car.kind == aFun: echo car.f(cdr)
        else: return car
    of aList:
      return eval(car, env)
    else:
      writeLine(stderr, "ERROR: invalid function name: " & $car)
  of aNumber, aBool: # constant literal
    return x
  of aSymbol: # variable reference
    return env[x.s]
  of aFun:
    echo x.f([])



# MAIN #

proc main() =
  while true:
    write(stdout, "lisnim> ")
    echo eval(parse(readLine(stdin)))


main()
