#!/usr/bin/env bash
#-print_format default=noprint_wrappers=1:nokey=1 \
ffprobe \
 -unit \
 -print_format default=noprint_wrappers=1 \
 -select_streams v:0 \
 -show_entries stream=width,height,codec_name:format=duration,bit_rate:format_tags=title \
 "$1" 2>/dev/null
