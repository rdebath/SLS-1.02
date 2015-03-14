BEGIN {
  name = ""
  num = 0
  printf ".na\n.Ih"
}
name != $1 {
  name = $1
  printf "\n.br\n%s", $1
  num = 0
}
num != $2 {
  if( num != 0 ) printf ","
  if ($3 == "*") printf " \\fB%s\\fP",$2   # defining reference
  else printf " %s", $2                    # additional reference
  num = $2
}
END {
  printf "\n.br\n.ad\n"
}
