#!/bin/bash
# transport.sh
function tp() {
    # $@ takes all arguments of the shell script and passes it along to `transport-exe`
    OUTPUT=`transport-exe $@`
    # return code 2 tells the shell script to `cd` to what `transport` outputs
    if [ $? -eq 2 ]
        then cd "$OUTPUT"
        else echo "$OUTPUT"
    fi
}

