#!/usr/bin/awk

BEGIN {
  redColor = "#ff0000"
  greenColor = "#00ff00"

  volumeLeft = 0;
  leftIsOn = "[on]";

  volumeRight = 0;
  rightIsOn = "[on]";
}

function colorVolume(volume, status) {
  if (status == "[on]")
    return sprintf("<fc=#00ff00>%s</fc>", volume)
  else
    return sprintf("<fc=#ff0000>%s</fc>", volume)
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
  print(colorVolume(volumeLeft, leftIsOn), colorVolume(volumeRight, rightIsOn))
}
