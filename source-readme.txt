				-[Wed Mar 15 12:32:14 2000 by jkf]-

Allegro iServe Source Distribution ReadMe file

The source for Allegro iServe is distributed under version 2.1 of
the Lesser GNU Public License.

This document describes how to us the source to build iServe.

To determine the version number for this software, look
in main.cl for *iserve-version*.



How to build and test iServe:

1. start lisp
    
   This should work in a lisp running in a :case-insensitive-upper
   or :case-sensitive-lower mode, although we do most of our running
   and testing in a :case-sensitive-lower lisp.

2. load in the file load.cl 

   user(1):  :ld <path-to-iserve>/load.cl

   it will compile and and load all of iserve, and it will load in
   the examples file too.


3. start the server

    user(2):  (net.iserve:start :port 8000)

    you can omit the port argument on Windows where any process can
    allocate port 80 (as long as it's unused).

4.  try out the server

    go to a web browser and try http://your-machine-name/




Now that you've verified that it works, you'll want to create
an iserve.fasl that you can load into your application.


5.  change lisp's current directory to be where the iserve source
    is.

    user(3): :cd  <path-to-iserve>

    
6.  make a distribution

    user(4): (make-distribution)


7.  now you'll find iserve.fasl in iserve-dist/




