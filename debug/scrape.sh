#!/usr/bin/env bash
if test "$1" == "--debug"
then debug=true
     shift
fi
ext=$1
urls="$2"
query="$3"

key=$(cat /dev/urandom | base32 | head -c 16)

params=$(head -n 1 ../scrapers/$ext.js | tr -d "\n")

if test $debug
then timeout=""
else timeout=10
     timeout=$(echo -n "$params" | tr -cd [:digit:])
fi

if test "${params/document_start}" != "$params"
then run_at=document_start
elif test "${params/document_end}" != "$params"
then run_at=document_end
else run_at=document_idle
fi

if test "${params/all_frames}" != "$params"
then all_frames=true
else all_frames=false
fi

tmp=$(mktemp --directory)
mkdir -p $tmp/ext

cat > $tmp/ext/manifest.json <<EOF
{
    "manifest_version": 2,
    "name": "scrapeaming-scraper-$ext",
    "version": "1.0",
    
    "content_scripts": [{
    	"matches": ["<all_urls>"],
	"run_at": "$run_at",
	"all_frames": $all_frames,
	"js": ["$ext.js"]
    }]
}
EOF

echo "const QUERY = '$query';" >> $tmp/ext/$ext.js
echo "const KEY   = '$key';"   >> $tmp/ext/$ext.js
cat ../scrapers/head.js        >> $tmp/ext/$ext.js
cat ../scrapers/$ext.js        >> $tmp/ext/$ext.js

if test $debug
then cmd=""
else cmd="xvfb-run timeout"
fi

HOME=$tmp $cmd $timeout chromium 2>&1 \
    --disable-gpu \
    --enable-logging=stderr \
    --load-extension=$tmp/ext \
    $urls \
    | awk -F "$key" '{print $2}' \
    | awk '{$1=$1};1' \
    | awk 'NF' \
    | awk '!x[$0]++'

rm -rf $tmp
