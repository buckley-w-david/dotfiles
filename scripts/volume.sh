#!/usr/bin/bash
pulseaudio-ctl | grep Volume | awk '{print $4$5}'
