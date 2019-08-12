#!/bin/bash

content=""
if [[ -f "/tmp/rss-count" ]]; then
    content=$(cat /tmp/rss-count)
fi

COUNT=`/usr/bin/newsboat -x print-unread | awk '{print $1}'`
if [ "$COUNT" != "$content" ]; then
    sh /home/david/bin/rss-unread.sh > /dev/null 2>&1
fi
echo -n ${COUNT} > /tmp/rss-count
echo ${COUNT}
