The AllegroServe Webserver
==========================

Table of contents
-----------------

   * Description
   * Author
   * Author comments
   * Documentation
   * Platforms
   * Dependencies
   * Installation
   * Configuration
   * Licence
   * Notes
   * Examples
   * Open Source 

Description
-----------

AllegroServe has these components:

 * HTTP/1.1 compliant web server capable of serving static and
   dynamic pages
 * HTML generation facility that seamlessly merges html tag
   printing with computation of dynamic content. The HTML
   generator matches perfectly with the HTML parser (which is
   in another project) to allow web pages to be read, modifed
   in Lisp and then regenerated.
 * HTTP client functions to access web sites and retrieve data.
 * Secure Socket Layer (SSL) for both the server and client.
 * Web Proxy facility with a local cache.
 * Comprehensive regression test suite that verifies the
   functionality of the client, server, proxy and SSL
 * high performance for static and dynamic web page delivery
 * Licensed under terms that ensure that it will always be open
   source and that encourages its use in commercial settings.
 * A new publish function that builds a page from static and
   dynamic data and handles caching of the result.
 * Access control mechanisms for publishing directories that
   gives the webmaster the ability to specify which files and
   directories in the tree should be visible.
 * The ability to run external CGI programs.
 * An improved virtual hosting system that supports different
   logging and error streams for each virtual host.

Author
------

John Foderaro, Franz Inc.

Author comments
---------------

The server part of AllegroServe can be used either as a standalone web
server or a module loaded into an application to provide a user
interface to the application. AllegroServe's proxy ability allows it
to run on the gateway machine between a company's internal network and
the internet.  AllegroServe's client functions allow Lisp programs to
explore the web.

AllegroServe was also written and open sourced as a way to demonstrate
network programming in Allegro Common Lisp.  AllegroServe was written 
according to a certain coding standard to demonstrate how Lisp programs 
are more readable if certain macros and special forms are avoided.

Platforms
---------

AllegroServe works on all versions of Allegro Common Lisp since 6.0.

Dependencies
------------

There are no dependences for AllegroServe.  In order to run the
allegroserve test suite you'll need to have [tester]
(<http://opensource.franz.com>) loaded.

Installation
------------

### start lisp
    
This should work in a lisp running in a :case-insensitive-upper or
:case-sensitive-lower mode, although we do most of our running and
testing in a :case-sensitive-lower lisp.  The current case mode is the
value of excl:\*current-case-mode\* 

### load in the file load.cl 

    user(1):  :ld <path-to-aserve>/load.cl

it will compile and and load all of AllegroServe, and it will load in
the examples file too.

### start the server

    user(2):  (net.aserve:start :port 8000)

you can omit the port argument on Windows where any process can
allocate port 80 (as long as it's unused). 

### try out the server

go to a web browser and try http://your-machine-name:8000/.  If the 
web browser is on the same machine as AllegroServe is running you can 
use http://localhost:8000/ as well.  Now that you've verified that it 
works, you'll want to create an aserve.fasl that you can load into your 
application.

### change lisp's current directory to the AllegroServe source

    user(3): :cd  <path-to-aserve>
    
### make a distribution

    user(4): (make-aserve.fasl)

now you'll find aserve.fasl in the aserve source directory.

Configuration
-------------

See the doc/aserve.html file that is part of this project for more
information on configuring AllegroServe.

Documentation
-------------

For complete documentation see the contents of the doc directory, 
which is part of this project or visit the online version of the
[AllegroServe documentation](http://opensource.franz.com/aserve).

### Quick Start Documentation

cd to the directory containing the distribution and start Allegro cl 
(or start Allegro and use the toplevel ":cd" command to cd to the 
directory containing the aserve). 

#### load aserve.fasl

    user(1): :ld aserve.fasl

#### load the examples (either the compiled or source version)

    user(2): :ld examples/examples

#### start the webserver

    user(3):  (net.aserve:start :port 8010)

#### view in a browser

    http://localhost:8010/

#### Usage notes

 * the steps to load the examples and start the server are interchangeable.
 * if you're running on a PC (or running as root on Unix) you
   can allocate port 80, so you don't have to specify a port
   when running the net.aserve:start function.
 * See the doc directory that is part of this project for more
   detailed usage documenation.

License
-------

The aserve source code is licensed under the terms of the
[Lisp Lesser GNU Public License](http://opensource.franz.com/preamble.html), 
known as the LLGPL. The LLGPL consists of a preamble and the
LGPL. Where these conflict, the preamble takes precedence.  This project is
referenced in the preamble as the LIBRARY.

Notes
-----

Webactions is a session-based framework for building web sites mixing
static and dynamic content that builds upon AllegroServe and is part
of this project.

See the webactions/doc/webactions.html file for more information.

For other links that may be of interest are:

 * [Portable AllegroServe](http://sourceforge.net/projects/portableaserve)
 * [Source to the if* macro](http://www.franz.com/~jkf/ifstar.txt)
 * [CL-HTTP](http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html)

Examples and Information
------------------------

See the doc/tutorial.html file and the contents under the examples
directory that are part of this project for more examples on how to
work with AllegroServe.

Franz Inc. Open Source Info
---------------------------
      
This project's homepage is <http://opensource.franz.com>. There is an
informal community support and development mailing list 
[opensource@franz.com](http://opensource.franz.com/mailinglist.html)
for these open source projects. We encourage you to take advantage by
subscribing to the list.  Once you're subscribed, email to
<opensource@franz.com> with your questions, comments, suggestions, and
patches.
