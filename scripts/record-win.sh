#!/bin/sh

ID=$(xwininfo | grep "Window id" | awk '{print $4}')
UPPER_X=$(xwininfo -id $ID | grep "Absolute upper-left X" | awk '{print $4}')
UPPER_Y=$(xwininfo -id $ID | grep "Absolute upper-left Y" | awk '{print $4}')
WIDTH=$(xwininfo -id $ID | grep "Width:" | awk '{print $2}')
HEIGHT=$(xwininfo -id $ID | grep "Height" | awk '{print $2}')

echo $ID $UPPER_LEFT $UPPER_RIGHT $WIDTH $HEIGHT

ffmpeg -video_size ${WIDTH}x${HEIGHT} -framerate 25 -f x11grab -i :0.0+${UPPER_X},${UPPER_Y} $@
