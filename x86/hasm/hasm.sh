#!/bin/sh

EXEC=./hasm

if which rlwrap >/dev/null
then exec rlwrap -H .hasm_history $EXEC
else exec $EXEC
fi
