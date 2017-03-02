import
  terminal,
  unittest,
  lis


styledEcho styleBright, fgWhite,
     "Testing lis.nim:"
echo "================"


test "Arithmetic":
  check execLine("""(+ 1 2)""") == atom number 3
  check execLine("""(- 3 2)""") == atom number 1
  check execLine("""(* 2 3)""") == atom number 6
  check execLine("""(/ 4 2)""") == atom number 2


test "Logic":
  check execLine("""(> 3 2 1)""") == atom true
  check execLine("""(> 3 1 2)""") == atom false


echo "================"
if programResult == 0:
  styledEcho styleBright, fgGreen,
     "     PASSED     "
else:
  styledEcho styleBright, fgRed,
     "     FAILED     "

