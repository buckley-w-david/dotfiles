result=$(sqlite3 ~/.newsboat/cache.db "SELECT title FROM rss_item WHERE unread>0" | sed 's/\n/ /')
/home/david/scripts/scroll.py "rss" "$result"
