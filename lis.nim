import
  parseutils, rationals, strutils


type
  Number = Rational[int]

proc number[T](i: T): Number = toRational(i)

type
  AtomKind = enum
    aList, aNumber, aSymbol

  Atom = object
    case kind: AtomKind
    of aList: list: seq[Atom]
    of aNumber: n: Number
    of aSymbol: s: string



# PARSE #

proc tokenize(input: string): seq[string] {.noSideEffect.} =
  ##  Convert string into sequence of tokens.
  input.replace("(", " ( ").replace(")", " ) ").split()


proc parseRatio(str: string, num, den: var int): bool {.noSideEffect.} =
  let s = str.split("/")
  if s.len != 2: return false
  if s[0].parseInt(num) != s[0].len: return false
  if s[1].parseInt(den) != s[1].len: return false
  return true


proc toAtom(token: string): Atom =
  ##  Parse single token. If token is not a number, it is a symbol.
  var
    f: float
    i, num, den: int
  if token.parseRatio(num, den):
    Atom(kind: aNumber, n: num // den)  # token is ratio
  elif token.parseInt(i) == token.len:
    Atom(kind: aNumber, n: number(i))   # token is int
  elif token.parseFloat(f) == token.len:
    Atom(kind: aNumber, n: number(f))   # token is float
  else:
    Atom(kind: aSymbol, s: token)       # token is symbol


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



# MAIN #

proc main() =
  while true:
    write(stdout, "lisnim> ")
    echo parse(normalize(readLine(stdin)))


main()
