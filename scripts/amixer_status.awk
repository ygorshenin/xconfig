#!/usr/bin/awk

/Mono:.*/ {
  if ($6 == "[on]")
    msg=$4
  else
    msg=$4 " " $6
  gsub(/(\[|\])/, "", msg)
  print msg
}
