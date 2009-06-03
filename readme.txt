The AllegroServe Webserver

Table of contents:

   1. Description
   2. Author
   3. Author comments
   4. Documentation
   5. Platforms
   6. Dependencies
   7. Installation
   8. Configuration
   9. Licence
  10. Notes
  11. Examples
  12. Open Source 

**********************************************************************

   1. Description:

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

   2. Author:

      John Foderaro, Franz Inc.

   3. Author comments:

      The server part of AllegroServe can be used either as a
      standalone web server or a module loaded into an application to
      provide a user interface to the application. AllegroServe's
      proxy ability allows it to run on the gateway machine between a
      company's internal network and the internet.  AllegroServe's
      client functions allow Lisp programs to explore the web.

      AllegroServe was also written and open sourced as a way to
      demonstrate network programming in Allegro Common Lisp.

      AllegroServe was written according to a certain coding standard
      to demonstrate how Lisp programs are more readable if certain
      macros and special forms are avoided.

   4. Platforms:

      AllegroServe works on all versions of Allegro Common Lisp since 6.0.

   5. Dependencies:

      There are no dependences for AllegroServe.  In order to run the
      allegroserve test suite you'll need to have tester loaded, which
      can be found on http://opensource.franz.com.

   6. Installation:

      i.   start lisp
    
        This should work in a lisp running in a :case-insensitive-upper
        or :case-sensitive-lower mode, although we do most of our
        running and testing in a :case-sensitive-lower lisp.
        The current case mode is the value of excl:*current-case-mode*

      ii.  load in the file load.cl 

        user(1):  :ld <path-to-aserve>/load.cl

        it will compile and and load all of AllegroServe, and it will
        load in the examples file too.

      iii. start the server

        user(2):  (net.aserve:start :port 8000)

        you can omit the port argument on Windows where any process
        can allocate port 80 (as long as it's unused).

      iv.  try out the server

        go to a web browser and try http://your-machine-name:8000/
    
        If the web browser is on the same machine as AllegroServe is
        running you can use http://localhost:8000/ as well

      Now that you've verified that it works, you'll want to create
      an aserve.fasl that you can load into your application.

      v.   change lisp's current directory to be where the
           AllegroServe source is.

        user(3): :cd  <path-to-aserve>
    
      vi.  make a distribution

        user(4): (make-aserve.fasl)

      vii. now you'll find aserve.fasl in the aserve source directory.

   7. Configuration:

      See the doc/aserve.html file that is of this project for more
      information on configuring AllegroServe.

   8. Documentation:

      i.   cd to the directory containing the distribution and start
           Allegro cl (or start Allegro and use the toplevel ":cd"
           command to cd to the directory containing the aserve).

      ii.  load aserve.fasl
    
        user(1): :ld aserve.fasl

      iii. load the examples (either the compiled or source version)

        user(2): :ld examples/examples

      iv.  start the webserver

        user(3):  (net.aserve:start :port 8010)

      v.   go to a web browser and select this machine and the port
           you chose:

        http://localhost:8010/

      Notes:

       * steps 3 and 4 can be interchanged.

       * if you're running on a PC (or running as root on Unix) you
         can allocate port 80, so you don't have to specify a port
         when running the net.aserve:start function.

       * See the doc directory that is part of this project for more
         detailed usage documenation.

   9. License:

      The aserve source code is licensed under the terms of the Lisp
      Lesser GNU Public License, known as the LLGPL. The LLGPL
      consists of a preamble and the LGPL. Where these conflict, the
      preamble takes precedence. aserve is referenced in the preamble
      as the LIBRARY.  (http://opensource.franz.com/preamble.html)

  10. Notes:

      Webactions is a session-based framework for building web sites
      mixing static and dynamic content that builds upon AllegroServe
      and is part of this project.

      See the webactions/doc/webactions.html file for more
      information.

      For other links that may be of interest are:

       Portable AllegroServe:
        http://sourceforge.net/projects/portableaserve/
       
       Source to the if* macro:
        http://www.franz.com/~jkf/ifstar.txt

       CL-HTTP:
        http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html

  11. Examples and Information:

      See the doc/tutorial.html file and the contents under the
      examples directory that are part of this project for more
      examples on how to work with AllegroServe.

  12. Open Source:
      
      This project is hosted on the http://opensource.franz.com
      site. There is an informal community support and development
      mailing list (opensource@franz.com) for these open source
      projects. We encourage you to take advantage by subscribing to
      the list by visiting http://opensource.franz.com/mailinglist.html. 
      Once you're subscribed, send email to opensource@franz.com with
      your questions, comments, suggestions, and patches. 
