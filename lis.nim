import
  strutils


proc tokenize(input: string): seq[string] =
  ##  Convert string into sequence of tokens.
  input.replace("(", " ( ").replace(")", " ) ").split()


proc main() =
  while true:
    writeLine(stdout, "lisnim> ")
    echo tokenize(readLine(stdin))


main()
