#! /usr/bin/env bash

set -ueE -o pipefail

prog="$(basename "$0")"

function errordie {
    [ "${*-}" ] && echo "$prog: $*" 1>&2
    exit 1
}

if ! type -p wrk > /dev/null; then
    errordie "Cannot find wrk, please install it before running $prog"
fi

arch=linuxamd64.64
version=10.1
connections=10
threads=10
duration=30
smp=
mlisp=

while [ $# -gt 0 ]; do
    case $1 in
        --arch)
	    [ $# -ge 2 ] || errordie "$1: missing companion argument"
            shift
            arch="$1"
            ;;
        --connections)
	    [ $# -ge 2 ] || errordie "$1: missing companion argument"
            shift
            connections="$1"
            ;;
        --duration)
	    [ $# -ge 2 ] || errordie "$1: missing companion argument"
            shift
            duration="$1"
            ;;
        --mlisp)
	    [ $# -ge 2 ] || errordie "$1: missing companion argument"
            shift
            mlisp="$1"
            ;;
        --smp) smp=smp ;;
        --threads)
	    [ $# -ge 2 ] || errordie "$1: missing companion argument"
            shift
            threads="$1"
            ;;
        --version)
	    [ $# -ge 2 ] || errordie "$1: missing companion argument"
            shift
            version="$1"
            ;;
        --) ;;
        *)  break ;;
    esac
    shift
done

if [ ! "$mlisp" ]; then
    mlisp="/fi/cl/${version}/bin/${arch}${smp}/mlisp"
fi
[ -x "$mlisp" ] || errordie "$mlisp is an invalid executable"

tempfile="/tmp/${prog}temp1$$"
tempfile2="/tmp/${prog}temp2$$"
rm -f "$tempfile"
rm -f "$tempfile2"
# shellcheck disable=SC2329
function exit_cleanup {
    rm -f "$tempfile"
    rm -f "$tempfile2"

    kill_server
}
# shellcheck disable=SC2329
function err_report {
    echo "Error on line $(caller)" 1>&2
}
trap err_report   ERR
trap exit_cleanup EXIT

###############################################################################

export ATEST_PORT_FILE=server.port
port=
export ATEST_PID_FILE=server.pid
pid=

# start the lisp server and as a side effect sets to variables:
#   port
#   pid
function start_server {
    rm -f "$ATEST_PORT_FILE"
    rm -f "$ATEST_PID_FILE"

    echo "Starting $mlisp"
    "$mlisp" -L atest.cl --batch --kill &> "$tempfile" &
    # give it time to start up
    sleep 5

    if [ -f "$ATEST_PORT_FILE" ]; then
        port=$(cat "$ATEST_PORT_FILE")
    else
        errordie "$ATEST_PORT_FILE does not exist"
    fi
    if [ -f "$ATEST_PID_FILE" ]; then
        pid=$(cat "$ATEST_PID_FILE")
    else
        errordie "$ATEST_PID_FILE does not exist"
    fi
    echo ";; port is $port"
    echo ";; pid  is $pid"
}

function kill_server {
    kill -9 "$pid"
}

function run {
    # shellcheck disable=SC2034
    wrk "-c${connections}" "-t${threads}" \
        "-d${duration}s" --latency "http://localhost:${port}${1}"
}

{
    start_server

    run  "/testfile"
    
    kill_server

    exit 0
}
