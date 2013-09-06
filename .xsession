#!/bin/bash
 
# Load resources
xrdb -merge .Xresources
 
# Set the background color
xsetroot -solid black
 
# Fire up apps
killall gnome-screensaver 
xscreensaver -no-splash &

exec xmonad
