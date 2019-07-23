#! /bin/bash

openssl req -newkey rsa:4096 -new -nodes -x509 -days 3650 \
	-keyout private.pem -out temp.pem

cat private.pem temp.pem > server.pem
rm private.pem temp.pem
