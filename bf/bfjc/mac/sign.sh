#!/bin/sh

# any part of any trusted developer id
DEVID="$1"
test "$DEVID" || {
    echo "usage: $0 <devid>"
    exit 1
}

MYDIR="$(dirname $0)"
TARGETDIR="$MYDIR/../target"

for EXE in "$TARGETDIR/debug/bfjc3" "$TARGETDIR/release/bfjc3" ; do
    test -x "$EXE" || continue
    echo "signing $EXE..."
    codesign -s "$DEVID" \
        --entitlements $MYDIR/entitlements.plist \
        -f -v "$EXE"
done
