#! /usr/bin/env bash

set -ueE -o pipefail

function doit {
    echo '==================================================================='
    echo "$@"
    echo ""
    ./atest.sh "$@"
    echo ""
}

{
    doit --version 10.1
    doit --version 10.1 --smp
    doit --version 11.0
    doit --version 11.0 --smp
    exit 0
}
