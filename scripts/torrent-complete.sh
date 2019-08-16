#!/usr/bin/env sh

find "$TR_TORRENT_DIR/$TR_TORRENT_NAME" -type f \( -name '*.epub' -or -name '*.pdf' -or -name '*.mobi' \) -print0 \
    | xargs -0 --no-run-if-empty -I{} cp "{}" "/mnt/data/calibre-tmp/"
