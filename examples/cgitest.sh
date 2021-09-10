#! /usr/bin/env bash
#
# return various cgi responses based on the first argument
#

# On Windows, the invoking makefile, for 'make stress', uses "set -u"
# and that is propagated to us here.  Be aware of that.

case ${1-} in

    1) # bogus but common lf headers
        echo 'Content-Type: text/plain'
        echo 
        echo "The environment vars are "
        env
        echo "==== end ===="
        ;;

    2) # redirect to franz.com, send some headers
        echo 'Location: https://franz.com'
        echo 'etag: 123hellomac'
	echo
	echo -n 'go to franz'
        ;;

    3) # send back unauthorized request
        echo 'Status: 401 unauthorized request'
	echo
        echo 'this request unauthorized'
        ;;

    4) # send back an ok response and something on the error stream
        echo 'Content-Type: text/plain'
        echo ''
        echo "okay"
        echo stuff-on-error-stream 1>&2
	;;
        
    *) # normal crlf headers
        echo 'Content-Type: text/plain'
        echo ''
        echo "The environment vars are "
        env
        echo "==== end ===="
        ;;
esac
    

