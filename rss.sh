#!/bin/bash

COUNT=`/usr/bin/newsboat -x print-unread | awk '{print $1}'`
echo ${COUNT}
