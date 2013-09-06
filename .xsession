#!/bin/bash
 
# Load resources
xrdb -merge .Xresources
 
# Set the background color
xsetroot -solid black
 
# Fire up apps
killall gnome-screensaver

WALLPAPER="${HOME}/Pictures/wallpaper.png"
if [ -e "${WALLPAPER}" ] && [ -f "${WALLPAPER}" ]; then
    xloadimage -onroot -center -border black "${WALLPAPER}"
fi
xscreensaver -no-splash &

function spawn_pipe() {
    local pipe="$1"
    rm -rf "${pipe}"
    mkfifo "${pipe}"
    echo "--" >"${pipe}" &
}

spawn_pipe "/tmp/.layout"
spawn_pipe "/tmp/.volume"

exec xmonad
