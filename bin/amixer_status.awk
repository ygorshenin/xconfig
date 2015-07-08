#!/usr/bin/awk

BEGIN {
  volumeLeft = 0;
  leftIsOn = "[on]";

  volumeRight = 0;
  rightIsOn = "[on]";
}

/Front Left/ {
  volumeLeft = $5;
  leftIsOn = $6
}
/Front Right/ {
  volumeRight = $5;
  rightIsOn = $6
}

END {
  gsub(/[\[\]]/, "", volumeLeft)
  gsub(/[\[\]]/, "", volumeRight)
  print(volumeLeft, leftIsOn, volumeRight, rightIsOn)
}
