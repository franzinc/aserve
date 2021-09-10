#! /bin/bash

openssl req -newkey rsa:4096 -new -nodes -x509 -days 3650 \
	-subj "/C=US/ST=US/L=Lafayette/O=Franz Inc/OU=Com/CN=www.franz.com" \
	-keyout private.pem -out temp.pem

cat private.pem temp.pem > server.pem
rm private.pem temp.pem
