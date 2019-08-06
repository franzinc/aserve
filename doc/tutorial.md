# AllegroServe Tutorial

#### Copyright (c) Franz Inc.

This document is a companion to the AllegroServe reference manual. Here we will
take you through various examples and demonstrate how to use the facilities of
the AllegroServe web server. Refer to the reference manual for more details on
the functions we mention here.

## Loading AllegroServe

AllegroServe is distributed as a single fasl file: `AllegroServe.fasl`. If the
file is installed where **`require`** can find it then you need only type

```lisp
(require :aserve)
```

to ensure that it's loaded. Otherwise you'll have to call the **`load`**
function.  In the subsequent steps we've assumed that you've loaded AllegroServe
into Lisp.

## Package setup

AllegroServe consists of two components: a web server and an html generator.
These are located in two Lisp packages: **`net.aserve`** and
**`net.html.generator`**.  These are long package names to type so the first
thing to do is to create a package that *uses* these packages as well as the
normal Lisp packages. Let's create a package called **`tutorial`** and make that
the current package:

```lisp
(defpackage :tutorial
    (:use :common-lisp :excl :net.aserve :net.html.generator))

(in-package :tutorial)
```

## Starting AllegroServe

Normally you would publish all the pages for your site and then start the web
server. That way everyone would see a consistent view of your site. However, for
this tutorial we'll start the server first so that we can immediately see the
pages we're publishing.

Web servers normally listen on port `80`. On Unix port `80` can only be
allocated by the the superuser (called `root`). On Windows any user can open
port `80` as long as it's not yet allocated. In order to make this tutorial work
on both Unix and Windows (and not require that you run as `root` on Unix), we'll
put our web server on port `8000`.

```lisp
tutorial(4): (start :port 8000)
#<wserver @ #x206929aa>
```

Now the web server is up and running. Let's assume that we're running
AllegroServe on a machine named `test.franz.com`. If you now go to a web browser
and ask for `http://test.franz.com` you will contact this AllegroServe server
and it will respond that whatever you asked for wasn't found on the server
(since we haven't published any pages). You can also try `http://test` and get
the same result (although the response message will be slightly different). If
you are running the web browser on test.franz.com as well you can ask for
`http://localhost` and get a similar "not found" response. This demonstrates
that web servers are known by many names. If you choose to take advantage of
that (creating what are known as *Virtual Hosts*) then AllegroServe will support
you . However if you want to create web pages that are served by whatever name
can be used to reach the server, then AllegroServe will allow you to do that as
well.

Type **`:proc`** to Lisp and look at which Lisp lightweight processes are running:

```lisp
tutorial(6): :proc
P Dis Sec dSec Priority State Process Name, Whostate, Arrest
* 8 3 3.2 0 runnable Initial Lisp Listener
* 2 0 0.0 0 waiting Connect to Emacs daemon, waiting for input
* 1 0 0.0 0 inactive Run Bar Process
* 1 0 0.0 0 waiting Editor Server, waiting for input
* 1 0 0.0 0 waiting AllegroServe-accept-6, waiting for input
* 0 0 0.0 0 inactive 1-aserve-worker
* 0 0 0.0 0 inactive 2-aserve-worker
* 0 0 0.0 0 inactive 3-aserve-worker
* 0 0 0.0 0 inactive 4-aserve-worker
* 0 0 0.0 0 inactive 5-aserve-worker
```

We've emboldened the threads that are part of AllegroServe. The thread named
**`aserve-accept-6`** is waiting for an http request. When one arrives it passes
it off to one of the **`aserve-worker`** threads and then loops back to wait for
the next request. The number of worker threads is determined by the
**`:listeners`** argument to the [**`start`**](aserve.md#f-start) function.

## Publishing a file

The simplest way to publish something is to publish files stored on the disk.
Create a file (here we called it `/tmp/sample.txt`) and put some words in it,
and then

```lisp
tutorial(30): (publish-file :path "/foo" :file "/tmp/sample.txt")
#<net.aserve::file-entity @ #x2076e0c2>
```

If you are running on Windows then the file will have a name like
`c:\tmp\sample.txt` When this file name is written in a Lisp string it would be
`"c:\\tmp\\sample.txt"` due to the special nature of the backslash character.

Now if we ask a web browser for `http://test.franz.com:8000/foo` we'll see the
contents of the file in the web browser. Since we didn't specify a content-type
in the call to [**`publish-file`**](aserve.md#f-publish-file) the content-type will be determined by the
"`txt`" file type, which is associated with the "`text/plain`" content-type.

Because we didn't specify a **`:host`** argument to [**`publish-file`**](aserve.md#f-publish-file)
AllegroServe will return this page to any browser regardless of the host name
used to name the machine. So AllegroServe will respond to requests for
`http://test.franz.com:8000/foo`, `http://test:8000/foo` and
`http://localhost:8000/foo`.

If we do

```lisp
tutorial(30): (publish-file :path "/foo" :file "/tmp/sample.txt"
                            :host "test.franz.com")
#<net.aserve::file-entity @ #x2076e0c2>
```

Then AllegroServe will only respond to requests for
`http://test.franz.com:8000/foo`. If we do

```lisp
tutorial(30): (publish-file :path "/foo" :file "/tmp/sample.txt"
                            :host ("test" "test.franz.com"))
#<net.aserve::file-entity @ #x2076e0c2>
```

Then AllegroServe will only respond to `http://test.franz.com:8000/foo` and
`http://test:8000/foo`. This type of restriction is useful if you want to create
the illusion that a single machine is really a set of machines, each with its
own set of web pages. Suppose that the machine `test.franz.com` also had the
name `sales.franz.com`. You could publish two different ways to respond to the
"`/foo`" url, depending on the host name specified in the request

```lisp
tutorial(30): (publish-file :path "/foo" :file "/tmp/test-sample.txt"
                            :host "test.franz.com")
#<net.aserve::file-entity @ #x2076e0c2>
tutorial(31): (publish-file :path "/foo" :file "/tmp/sales-sample.txt"
                            :host "sales.franz.com")
#<net.aserve::file-entity @ #x2076e324>
```

Now you will get different results if you ask for
`http://test.franz.com:8000/foo` and `http://sales.franz.com:8000/foo`.

## Publishing a computed page

The most important reason for using the AllegroServe web server is that you can
compute a web page when a request comes in. This allows your program to display
the most up-to-date information on the page or tailor the page to each browser.
Using the [**`publish`**](aserve.md#f-publish) function, a lisp function called a *response function*
is associated with a **`url`**. When a request comes in that matches that url,
the response function is run and it must generate the correct response which is
sent back to the browser. The simplest response function is published here:

```lisp
(publish :path "/hello"
    :content-type "text/plain"
    :function
    #'(lambda (req ent)
          (with-http-response (req ent)
              (with-http-body (req ent)
                  (princ "Hello World!" *html-stream*)))))

```

Response functions take two arguments: a request object and an entity object.
The request object contains all of the information about the request (such as
the machine from which the request was made, and the headers passed along with
the request). The request object is also used to store information about the
response that is made to the request. The entity object contains the information
passed to the [**`publish`**](aserve.md#f-publish) function. One important item in the entity is the
**`content-type`** which serves as the default content-type for the response (it
can be overridden by an argument to [**`with-http-response`**](aserve.md#f-with-http-response)).

A response function must use the [**`with-http-response`**](aserve.md#f-with-http-response) and
[**`with-http-body`**](aserve.md#f-with-http-body) macros and then send any additional data to the stream
**`*html-stream*`**. Despite the name of the stream, the data need not always be
html. The purpose of [**`with-http-response`**](aserve.md#f-with-http-response) is to allow AllegroServe to
determine how it will setup streams to respond to the request. AllegroServe will
also check to see if the browser already has an up-to-date copy of this page
cached in which case it will not even run the code in the body of the
[**`with-http-response`**](aserve.md#f-with-http-response) macro.  [**`with-http-body`**](aserve.md#f-with-http-body) is responsible for sending
back the response code and headers, and the body of [**`with-http-body`**](aserve.md#f-with-http-body) is
where lisp code can send data which will be the body of the response.

The preceding example sends a very simple plain text string, specifying the
content-type to be `"text/plain"`. Typically you'll want to return an html
page. AllegroServe has a very concise macro for creating html. Here's a
response function that sends html:

```lisp
(publish :path "/hello2"
    :content-type "text/html"
    :function
    #'(lambda (req ent)
         (with-http-response (req ent)
            (with-http-body (req ent)
               (html
                  (:html (:head (:title "Hello World Test"))
                         (:body
                           ((:font :color "red") "Hello ")
                           ((:font :color "blue") "World!"))))))))
```

While both of the preceding response functions generate their response at
request time, they both send back the exact same response every time. That's not
a very good demonstration of dynamic web pages. The following page shows how you
can implement a simple counter for the number of accesses:

```lisp
(publish :path "/hello-count"
    :content-type "text/html"
    :function
    (let ((count 0))
      #'(lambda (req ent)
          (with-http-response (req ent)
            (with-http-body (req ent)
              (html
                (:html
                  (:head (:title "Hello Counter"))
                  (:body
                     ((:font :color (nth (random 5)
                                     '("red" "blue"
                                       "green" "purple"
                                       "black")))
                       "Hello World had been called "
                       (:princ (incf count))
                       " times")))))))))
```

This page counts the number of accesses and also displays the text in a color it
selects randomly.

## Publishing a form

A form displays information, has places for the user to enter information, and
has one or more ways for the user to signal that he is done entering information
and the the form should be processed. There may be more than one form on a web
page but the forms can't be nested or overlap.

When the user clicks on the "Submit" (or equivalent) button and the form data is
sent by the browser to the web server, the web server has to process that data
and return a web page to display on the browser screen. This is an important
situation where using Lisp to process the form data is significantly easier than
the alternatives (such as the shell, or perl or some other primitive scripting
language).

There are three ways that form data is sent to the web browser

  1. **Query string** - the form data is appended to the url, with a question
     mark separating the path of the url from the form data. This is the default
     way that form data is returned. It's fine for small amounts of data, and it
     allows the user to bookmark the result of filling out a form.
  2. **Encoded in the body of the request** - If the form specifies the **POST**
     method is to be used to return the data, then the data is encoded and
     placed in the body of the request after the headers. This allows the form
     data to be huge.
  3. **Multipart body** - in this scheme, the data from the web browser looks
     like a multipart MIME message. This is commonly used when the form data
     consists of complete files, since in this case you want to pass along the
     name of the file (which is in the MIME header) and you don't want to pay
     the cost of encoding the contents of the file.

The three attributes of a **`:form`** tag that determine how data is sent to the
server are:

  1. **`:method`** - this is either "GET" (the default) or "POST". When "GET" is
     used the data will be sent as a query string.
  2. **`:enctype`** - this is either `"application/x-www-form-urlencoded"` (the
     default) or is `"multipart/form-data"` if you want the data sent as a
     multipart body. The value of this attribute only matters if the
     **`:method`** is set to "POST".
  3. **`:action`** - this is the url to which the request with the data is sent.
     With AllegroServe it's often convenient to make this url the same as the
     url of the entity that created the form, and have the entity handling
     function determine whether it is being called to display the form or to
     handle the results of filling out the form.

Let's examine in detail each of the methods for sending form data:

### Form data in a query string

In a url like `http://www.machine.com/foo/bar?name=gen\&age=28` the characters
after the question mark are the **`query-string`**. The query string is **not**
used by AllegroServe to determine the entity to handle the request. When the
entity begins processing the request it can ask for the [**`request-query`**](aserve.md#f-request-query) of
the **`request`** object. [**`request-query`**](aserve.md#f-request-query) will return an assoc list where
the **`car`** of each entry is a string (e.g. "name" in the example) and the
**`cdr`** is also a string (e.g. "gen" in the example). You can ask for the
[**`request-query`**](aserve.md#f-request-query) of any request object and if there is no query string for
the request, [**`request-query`**](aserve.md#f-request-query) will return **`nil`**.

This is a typical entity handler that generates a form and handles the result of
filling out the form:

```lisp
(publish :path "/queryform"
    :content-type "text/html"
    :function
    #'(lambda (req ent)
       (let ((name (cdr (assoc "name" (request-query req)
                               :test #'equal))))
         (with-http-response (req ent)
           (with-http-body (req ent)
             (if* name
               then ; form was filled out, just say name
                    (html (:html
                            (:head (:title "Hi to " (:princ-safe name)))
                            (:body "Your name is "
                                   (:b (:princ-safe name)))))
               else ; put up the form
                    (html (:html
                            (:head (:title "Tell me your name"))
                            (:body
                              ((:form :action "queryform")
                                "Your name is "
                                ((:input :type "text"
                                         :name "name"
                                         :maxlength "20"))))))))))))
```

In the function above we first check to see what value is associated wtih the
tag "name" in the query string. If there is a name then we know that we've been
called after the form was filled out, so we process the form data, which in this
case means just printing out the name. Note that we use **`:princ-safe`** to
display the name in html. It's important to use **`:princ-safe`** instead of
**`:princ`** in situations like this where we are printing a string that may
contain characters special to html. In the **`:action`** attribute for the
**`:form`** we specified "queryform" instead of "/queryform" since it's best to
keep all urls relative rather than absolute in order to make it possible to
access the pages through a proxy server that might prepend a string to the root
of the url. We could have separated the functionality in the above example so
that one entity handler put up the form and another one processed the form. For
simple forms it just seems simpler to do it all with one url and one entity
function.

### Form data encoded in the request body

When the data from the form is very large or it's important to hide it from view
in the url, the typical method to accomplish this is to specify the "POST"
method for the form. In this case the data for the form appears in the body of
the request. There are two supported encodings of the form data in the body.  In
this section we'll describe how to handle the default encoding, called:
`"application/x-www-form-urlencoded"`. First you must call
[**`get-request-body`**](aserve.md#f-get-request-body) to read and return the body of the request. Second you
must call [**`form-urlencoded-to-query`**](aserve.md#f-form-urlencoded-) to convert the encoded body into an
assoc list, where every entry is a cons consisting of a string naming the value
and then the string containing the value.

The following sample shows a single entity handler function that can put up a
form and can process data from the form. It isn't necessary to use the same
handler for putting up and processing the data from a form. In this example we
create a form with a big box for entering text. We invite the user to enter text
in the box and click on a button when he is finished. At that point the entity
handler gets and decodes the body of the request, and finds the data from the
text box. It then generates a table showing how often the characters `a` through
`z` are found in the text selection.

```lisp
(publish :path "/charcount"
    :content-type "text/html"
    :function
    #'(lambda (req ent)
        (let* ((body (get-request-body req))
               (text (if* body
                      then (cdr (assoc "quotation"
                                  (form-urlencoded-to-query body)
                                  :test #'equal)))))
         (with-http-response (req ent)
           (with-http-body (req ent)
             (if* text
               then ; got the quotation, analyze it
                    (html
                     (:html
                       (:head (:title "Character Counts")
                       (:body
                         (:table
                           (do ((i #.(char-code #\a) (1+ i)))
                               ((> i #.(char-code #\z)))
                             (html (:tr
                                     (:td (:princ (code-char i)))
                                     (:td (:princ
                                            (count (code-char i)
                                                    text)))))))))))
               else ; ask for quotation
                    (html
                      (:html
                         (:head (:title "quote character counter")
                         (:body
                            ((:form :action "charcount"
                                    :method "POST")
                              "Enter your favorite quote "
                              :br
                              ((:textarea
                                  :name "quotation"
                                  :rows 30
                                  :cols 50))
                              :br
                              ((:input :type "submit"
                                  :name "submit"
                                  :value "count it")))))))))))))
```

In this example we ask for the body of the request and then the value of the
field named "quotation". If that isn't found then we assume that we are being
called to display the form. We could have checked the value of
**`(request-method req)`** which will be **`:get`** when we should put up the
form and **`:post`** when we should analyze data from the form.


### Form data encoded as a multipart body

The final method of sending form data is as a multipart message. This occurs
when you specify a **`:method`** of "POST" and an **`:enctype`** of
`"multipart/form-data"`. The handler for this must detect when it is being
called from a **`:post`** request and must call a sequence of functions to
retrieve each item from the message body. First it calls
[**`get-multipart-header`**](aserve.md#f-get-multipart-header) to get the next header (or **`nil`** if there are no
more headers). The header data is an assoc list where the values have different
formats as described in the AllegroServe manual. After reading the header the
handler must call [**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence) to read successive chunks of data
associated with this header.

An example demonstrating this is too large to include here but can be found in
the AllegroServe `examples.cl` file (search in that file for "getfile-got")

## Authorizing a request

You don't necessarily want to allow everyone to access every page you publish.
We will describe common ways to check whether someone has permission to access a
page. There are two ways to do the authorization checks. You can write the tests
yourself in the entity function, or you can create an **`authorizer`** object
and attach it to the entity. Below we'll show you how to write the code to do
the checks manually. The [AllegroServe manual](aserve.md) describes the
**`authorizer`** objects.

### Password

One way to control access to a page is to request that the person at the browser
enter a name and password. You can create a form and have the user enter the
information and then click on a button to submit it. Another way is to return a
401 (response unauthorized) code to the request made to access your page. When
given that response, the web browser will pop up a window requesting a name and
password, and after that's entered, the browser resends the request but includes
the name and password in the header.

The method you choose for asking for the name and password may depend on how
secure you want the response to be. Using a form the name and password are sent
to the web server without any encoding at all (other than the simple urlencoding
which only affects unusual characters). If your form uses the "GET" method then
the name and password appear in the url which makes them very easy to spot, so
you at least want to use the "POST" method if you use a form. If on the other
hand you use the 401 response code, then the name and password are sent in a
more encrypted form (using an encoding called `base64`) however anyone
sufficiently motivated can decrypt this without a lot of trouble. AllegroServe
does not yet support `md5` authentication which is the most secure way to do
authentication in the HTTP/1.1 protocol.

One advantage of using the 401 response to cause the user to enter a name and
password is that most web browsers will continue to send the entered name and
password along with future requests to the server until the web browser is
restarted. Thus you can simultaneously unlock a whole group of pages. If you
choose to handle the authentication in a form you may want to use a cookie to
make a record that this web browser is now qualified to access a certain group
of pages. Cookies aren't hard to store, but some users turn off cookie saving
thus you will make your site inaccessible to these people. Another authorization
mechanism is to insert hidden fields in forms with values that tell the server
that this access is authorized for a certain amount of time.

This is an example of using the 401 response to do user authorization.

```lisp
(publish :path "/secret"
    :content-type "text/html"
    :function
    #'(lambda (req ent)
         (multiple-value-bind (name password) (get-basic-authorization req)
            (if* (and (equal name "foo") (equal password "bar"))
               then (with-http-response (req ent)
                      (with-http-body (req ent)
                        (html (:head (:title "Secret page"))
                              (:body "You made it to the secret page"))))
               else ; cause browser to ask name and password
                    (with-http-response (req ent :response
                                                 *response-unauthorized*)
                      (set-basic-authorization req "secretserver")
                      (with-http-body (req ent)))))))
```


### Source address

You can determine the address from which a request was made and restrict access
based on that address. If the request came through a proxy server then you are
really determining the address of the proxy server. The following code only
serves the 'secret' page if the request came from a browser running on the same
machine as the server, and which is made over the loopback network on the
machine. The loopback network is a network that exists entirely inside the
operating system of the machine. The convention is that a loopback network
address has 127 in the most significant byte, and that is what we test for in
the following example:

```lisp
(publish :path "/local-secret"
    :content-type "text/html"
    :function
    #'(lambda (req ent)
    (let ((net-address (ash (socket:remote-host
                              (request-socket req))
                            -24)))
       (if* (equal net-address 127)
          then (with-http-response (req ent)
                 (with-http-body (req ent)
                   (html (:head (:title "Secret page"))
                     (:body (:b "Congratulations. ")
                        "You are on the local network"))))
          else
               (with-http-response (req ent)
                 (with-http-body (req ent)
                   (html
                     (:html (:head (:title "Unauthorized"))
                            (:body
                                "You cannot access this page "
                                "from your location")))))))))
```

To see how this example works differently depending on whether the access is
through the loopback network or the regular network, try accessing it via
`http://localhost:8000/local-secret` and
`http://test.franz.com:8000/local-secret` (where we are assuming that you are
running on `test-franz.com`).

## Multiple servers

AllegroServe can run multiple independent web servers. Each web server listens
for requests on a different port. Because each web server can appear to be
serving pages for different hosts (using the virtual host facility already
described in the discussion of the [**`publish`**](aserve.md#f-publish) functions), it is usually not
necessary to use the multiple server facility we describe here.

All of the information about a web server, including the entities it serves, are
stored in a **`wserver`** object. The *current* wserver object is stored in the
variable **`*wserver*`**. The publish functions use the value of **`*wserver*`**
as the default server into which they publish entities.

## Debugging a computed response handler

We will describe this in detail when the tutorial is updated. For now read the
documentation on [**`net.aserve::debug-on`**](aserve.md#f-debug-on) in the [AllegroServe](aserve.md)
manual.
