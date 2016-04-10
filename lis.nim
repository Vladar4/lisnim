import
  parseutils, strutils


type
  AtomKind = enum
    aList, aInt, aFloat, aSymbol

  Atom = object
    case kind: AtomKind
    of aList: list: seq[Atom]
    of aInt: i: int
    of aFloat: f: float
    of aSymbol: s: string


proc tokenize(input: string): seq[string] {.noSideEffect.} =
  ##  Convert string into sequence of tokens.
  input.replace("(", " ( ").replace(")", " ) ").split()


proc atom(token: string): Atom {.noSideEffect.} =
  ##  Parse single token. If token is not a number, it is a symbol.
  var
    f: float
    i: int
  if token.parseFloat(f) > 0: Atom(kind: aFloat, f: f)  # token is float
  elif token.parseInt(i) > 0: Atom(kind: aInt, i: i)    # token is int
  else: Atom(kind: aSymbol, s: token)                   # token is symbol



proc read(tokens: var seq[string]): Atom =
  ##  Read an expression from a sequence of tokens.
  if tokens.len == 0: # no tokens
    writeLine(stderr, "ERROR: Unexpected EOF while reading")
    return Atom(kind: aList, list: @[]) # return empty list

  var token = tokens.pop() # take next token

  if token == "(":
    result = Atom(kind: aList, list: @[])  # start new list
    while tokens[^1] != ")":
      result.list.add(tokens.read()) # read list items
    discard tokens.pop() # pop off ")"
    return

  elif token == ")":
    writeLine(stderr, "ERROR: Unexpected )")

  else:
    return atom(token)


proc reverse[T](input: seq[T]): seq[T] {.noSideEffect.} =
  ## Reverse given sequence.
  result = @[]
  for i in countdown(input.high, 0):
    result.add(input[i])


proc parse(input: string): Atom =
  ##  Read an expression from ``input`` string.
  var tokens = input.tokenize().reverse()
  tokens.read()


proc main() =
  while true:
    write(stdout, "lisnim> ")
    echo parse(readLine(stdin))


main()
