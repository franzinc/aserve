#! /bin/sh
#
# return various cgi responses based on the first argument
#
case $1 in

    1) # bogus but common lf headers
        echo 'Content-Type: text/plain'
        echo 
        echo "The environment vars are "
        env
        echo "==== end ===="
        ;;

    2) # redirect to franz.com, send some headers
        echo 'Location: http://www.franz.com'
        echo 'etag: 123hellomac'
	echo
	echo -n 'go to franz'
        ;;

    3) # send back unauthorized request
        echo 'Status: 401 unauthorized request'
	echo
        echo 'this request unauthorized'
        ;;

    *) # normal crlf headers
        echo 'Content-Type: text/plain'
        echo ''
        echo "The environment vars are "
        env
        echo "==== end ===="
        ;;
esac
    

