| [**ToC**] | [**DocOverview**] | [**CGDoc**] | [**RelNotes**] | [**FAQ**] | [**Index**] | [**PermutedIndex**] |

# AllegroServe - A Web Application Server

##### Version 1.3.75

#### Copyright (c) Franz Inc.

AllegroServe is available for download as part of Allegro CL (see
[www.franz.com][franz] or the [Github][aserve-github] page). Latest available
version of this document can be found [here][latest].

## Table of Contents

* [Introduction](#introduction)
* [Running AllegroServe](#running-AllegroServe)
* [Starting the Server](#starting-the-server)
  * [**`start`**](aserve.md#f-start)
* [Shutting Down the Server](#shutting-down-the-server)
  * [**`shutdown`**](aserve.md#f-shutdown)
* [Publishing Information](#publishing-information)
  * [**`publish-file`**](aserve.md#f-publish-file)
    * [Entity hook function](#entity-hook-function)
  * [**`publish-directory`**](aserve.md#f-publish-directory)
    * [Directory Access Files](#directory-access-files)
  * [**`publish`**](aserve.md#f-publish)
  * [**`publish-prefix`**](aserve.md#f-publish-prefix)
  * [**`publish-multi`**](aserve.md#f-publish-multi)
* [Generating a Computed Response](#generating-a-computed-response)
  * [**`with-http-response`**](aserve.md#f-with-http-response)
  * [**`with-http-body`**](aserve.md#f-with-http-body)
  * [**`get-request-body`**](aserve.md#f-get-request-body)
  * [**`get-request-body-incremental`**](aserve.md#f-get-request-body-incremental)
  * [**`header-slot-value`**](aserve.md#f-header-slot-value)
  * [**`reply-header-slot-value`**](aserve.md#f-reply-header-slot-value)
  * [**`request-query`**](aserve.md#f-request-query)
  * [**`set-trailers`**](aserve.md#f-set-trailers)
  * [**`can-set-trailers-p`**](aserve.md#f-can-set-trailers-p)
  * [**`request-query-value`**](aserve.md#f-request-query-value)
* [Request Variables](#request-variables)
  * [**`request-variable-value`**](aserve.md#f-request-variable-value)
* [Request Object Readers and Accessors](#request-object-readers)
  * [**`request-method`**](aserve.md#f-request-method)
  * [**`request-uri`**](aserve.md#f-request-uri)
  * [**`request-protocol`**](aserve.md#f-request-protocol)
  * [**`request-socket`**](aserve.md#f-request-socket)
  * [**`request-wserver`**](aserve.md#f-request-wserver)
  * [**`request-raw-request`**](aserve.md#f-request-raw-request)
  * [**`request-reply-code`**](aserve.md#f-request-reply-code)
  * [**`request-reply-date`**](aserve.md#f-request-reply-date)
  * [**`request-reply-headers`**](aserve.md#f-request-reply-headers)
  * [**`request-reply-content-length`**](aserve.md#f-request-reply-content-length)
  * [**`request-reply-plist`**](aserve.md#f-request-reply-plist)
  * [**`request-reply-strategy`**](aserve.md#f-request-reply-strategy)
  * [**`request-reply-stream`**](aserve.md#f-request-reply-stream)
* [CGI Program Execution](#cgi-program)
  * [**`run-cgi-program`**](aserve.md#f-run-cgi-program)
* [Form Processing](#form-processing)
  * [**`get-multipart-header`**](aserve.md#f-get-multipart-header)
  * [**`parse-multipart-header`**](aserve.md#f-parse-multipart-header)
  * [**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence)
  * [**`get-all-multipart-data`**](aserve.md#f-get-all-multipart-data)
  * [**`form-urlencoded-to-query`**](aserve.md#f-form-urlencoded-)
  * [**`query-to-form-urlencoded`**](aserve.md#f-query-to)
* [Authorization](#authorization)
  * [**`get-basic-authorization`**](aserve.md#f-get-basic-authorization)
  * [**`set-basic-authorization`**](aserve.md#f-set-basic-authorization)
  * [**`password-authorizer`**](aserve.md#c-password-authorizer)
  * [**`location-authorizer`**](aserve.md#c-location-authorizer)
  * [**`function-authorizer`**](aserve.md#c-function-authorizer)
* [Cookies](#cookies)
  * [**`set-cookie-header`**](aserve.md#f-set-cookie-header)
  * [**`get-cookie-values`**](aserve.md#f-get-cookie-values)
* [Variables](#variables)
  * [**`*aserve-version*`**](aserve.md#v-aserve-version)
  * [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format)
  * [**`*http-response-timeout*`**](aserve.md#v-http-response-timeout)
  * [**`*http-free-worker-timeout*`**](aserve.md#v-http-free-worker-timeout)
  * [**`*mime-types*`**](aserve.md#v-mime-types)
* [AllegroServe Request Processing Protocol](#iseve-request-proc)
  * [**`handle-request`**](aserve.md#f-handle-request)
  * [**`standard-locator`**](aserve.md#f-standard-locator)
  * [**`unpublish-locator`**](aserve.md#f-unpublish-locator)
  * [**`authorize`**](aserve.md#f-authorize)
  * [**`failed-request`**](aserve.md#f-failed-request)
  * [**`denied-request`**](aserve.md#f-denied-request)
  * [**`process-entity`**](aserve.md#f-process-entity)
* [Client Functions](#client-request)
  * [**`do-http-request`**](aserve.md#f-do-http-request)
  * [**`computed-content`**](aserve.md#c-computed-content)
  * [**`file-computed-content`**](aserve.md#c-file-computed-content)
  * [**`stream-computed-content`**](aserve.md#c-stream-computed-content)
  * [**`client-cache`**](aserve.md#c-client-cache)
  * [**`client-request`**](aserve.md#c-client-request)
  * [**`cookie-jar`**](aserve.md#c-cookie-jar)
  * [**`make-http-client-request`**](aserve.md#f-make-http-client-request)
  * [**`read-client-response-headers`**](aserve.md#f-read-client-response)
  * [**`client-request-read-sequence`**](aserve.md#f-client-request-read-sequence)
  * [**`read-response-body`**](aserve.md#f-read-response-body)
  * [**`client-request-close`**](aserve.md#f-client-request-read-close)
  * [**`compute-digest-authorization`**](aserve.md#f-compute-digest-authorization)
  * [**`uriencode-string`**](aserve.md#f-uriencode-string)
* [Proxy](#proxy)
  * [**`proxy-control`**](aserve.md#c-proxy-control)
  * [**`authorize-proxy-request`**](aserve.md#f-authorize-proxy-request)
* [Cache](#cache)
* [Request Filters](#filters)
* [Virtual Hosts](#virtual_hosts)
* [Timeouts](#timeouts)
  * [**`wserver-io-timeout`**](aserve.md#f-wserver-io-timeout)
  * [**`wserver-response-timeout`**](aserve.md#f-wserver-response-timeout)
* [Compression](#compression)
* [SSL/TLS](#ssltls)
* [Miscellaneous](#miscellaneous)
  * [**`ensure-stream-lock`**](aserve.md#f-ensure-stream-lock)
  * [**`map-entities`**](aserve.md#f-map-entities)
  * [**`log-for-wserver`**](aserve.md#f-log-for-wserver)
* [Running AllegroServe as a Service on Windows NT](#asaservice)
* [Using International Characters in AllegroServe](#international-chars-aserve)
* [Debugging](#debugging)
  * [**`net.aserve::debug-on`**](aserve.md#f-debug-on)
  * [**`net.aserve::debug-off`**](aserve.md#f-debug-off)


## <span id="introduction"></span>Introduction

**AllegroServe** is a webserver written at [Franz Inc][franz]. AllegroServe is
designed to work with the [htmlgen](htmlgen.md) system for generating dynamic
html, as one of the big advantages of a web server written in Common Lisp is the
ability to generate html dynamically. In this document we'll consider the web
server and dynamic html generation to be parts of the same product.

The design goals of AllegroServe are:

  - a very small footprint. It should be possible to make AllegroServe a part of
    every application without being concerned about the impact of its size and
    processing requirements.
  - simple configuration. AllegroServe should start automatically with minimal
    input from the user.
  - easy to use. The typical scenarios should be easy to program with just
    knowledge of simple html.
  - usable in commercial applications.
  - support the latest HTTP protocol (currently HTTP/1.1).
  - runnable in multiple configurations. We want to support a program that just
    wants to make some part of it visible or configurable by one user through a
    web server. We also want to support a web site running on a multiprocessor
    taking many hits per second. Finally, we want to support levels in between
    those scenarios.

The links in the navigation bar above are to the documentation of the latest
release of Allegro CL. AllegroServe is supported on earlier releases. See
[www.franz.com/support/documentation/](https://franz.com/support/documentation/)
for links to documentation of earlier releases.



## <span id="running-AllegroServe"></span>Running AllegroServe

Running AllegroServe requires that you

  - *load* `aserve.fasl` into Lisp;
  - *publish* zero or more urls;
  - *start* the server;
  - *publish* zero or more urls.

We mention **publish** twice to emphasize that you can publish urls before and
after you start the server.



## <span id="starting-the-server"></span>Starting the server

The function **`net.aserve:start`** is used to start the server running.

-----

<span id="f-start"></span>
#### **`net.aserve:start`**

```lisp
(start &key port host listeners max-listeners chunking keep-alive server
            setuid setgid debug proxy proxy-proxy cache restore-cache
            accept-hook ssl ssl-password os-processes external-format
            compress ssl-key ssl-password ssl-method test-ssl ca-file
            ca-directory verify max-depth)
```

If no arguments are given then [**`start`**](aserve.md#f-start) starts a multi-threaded web server on
port 80, which is the standard web server port. If you are running this on Unix
then you can only allocate port 80 if you are logged in as root or have made
Lisp a set-user-id root program.

There are quite a few keyword arguments to [**`start`**](aserve.md#f-start), but in practice you only
need be concerned with **`:port`** and **`:listeners`**. The arguments have the
following meanings:

  - **`port`** - the port on which to open the web server. 80 is the default.
  - **`host`** - the host on which to run the server. If you don't specify this then
    the server will listen on all TCP/IP network interfaces on the machine. If
    you specify "localhost" then the server will only accept connections from
    the same machine. Other values for host can be used to run AllegroServe only
    a particular network interface. Host can be a name (like `"foo.franz.com"`),
    a dotted ip address `"192.168.0.1"` or an integer IP address.
  - **`listeners`** - the number of threads to process http requests. If a value
    isn't given for the **`:listeners`** argument then 5 is assumed. If the value
    is **`nil`** or `0` then the server runs in *simple server mode* in which the
    [**`start`**](aserve.md#f-start) function doesn't return - instead it processes the requests
    itself, one at a time. If a positive number is given as the value of
    **`:listeners`** then the server runs in *threaded server mode*. In this mode
    separate lisp threads are started to handle requests from clients and after
    which the [**`start`**](aserve.md#f-start) function returns. The number of request handling threads
    is initially equal to the value of the **`:listeners`** keyword argument. Note
    that under high load more threads may be created to keep the server
    operating smoothly. See also **`:max-listeners`**.
  - **`max-listeners`** - if not **`nil`**, then it is a hard limit on the number of
    threads that process HTTP requests. The default is **`nil`**.
  - **`chunking`** - if true then the server will use the chunked transfer encoding
    when it's possible to do so. This is an optimization and should be left
    enabled unless you suspect that it is the cause of some sort of error. The
    default is true.
  - **`keep-alive`** - if true then the server will keep connections alive if
    requested by the web client, and if there are sufficient free threads to
    handle new requests coming in. This is an optimization and should be left
    on. The default is true.
  - **`server`** - if this is passed a value it must be a **`wserver`** object, which
    denotes a particular instance of a web server. This is for support of
    running multiple independent web servers in the same lisp image. This will
    be described in a later section (eventually).
  - **`setuid`** - after opening the port, change the user id of this process to the
    given number (only numbers are allowed, not names). This will only have an
    effect on Unix and it will only succeed if the current user id is `root`.
    You would want to use this argument if you plan on opening port `80` on
    Unix, as you would have to start the server as `root` but then would want
    to change the user id to an account with fewer privileges before allowing
    possibly malicious people to connect to it.
  - **`setgid`** - after opening the port, change the group id of this process to
    the given number (only numbers are allowed, not names). This will only have
    an effect on Unix
  - **`debug`** - if given a number this will print debugging messages whose
    associated codes are this number or less. This is really an internal switch
    and may be removed in future versions.
  - **`proxy`** - if true then this server will also act as a proxy server and will
    forward http requests to other servers.
  - **`proxy-proxy`** - if **`proxy`** is also given a true value, then this argument
    determines where the proxy will forward requests. If proxy-proxy is nil then
    the requests go directly to the server given in the request. If proxy-proxy
    is given a value of a host and an optional port then the request is
    forwarded to the proxy server at that address. Valid values for proxy-proxy
    look like `"proxy.myfirm.com"` and `"localhost:8000"`. If no port is specified,
    port 80 is assumed.
  - **`cache`** - if true (and if proxy is true as well) cache locally the work
    done by the proxy server. The value of this variable specifies the size of
    the caches, both memory and disk. See the section on caches for more details
    on the format of the argument.
  - **`restore-cache`** - if given a value then this value should be the name of the
    file created by **`net.aserve:shutdown`** when given the **`save-cache`**
    argument. The state of the cache is restored as of when it was saved. This
    will only succeed if the external cache files that were in use when the
    [**`shutdown`**](aserve.md#f-shutdown) was done are in exactly the same state they were when the
    [**`shutdown`**](aserve.md#f-shutdown) was done. When the **`restore-cache`** argument is given, the
    value of the **`cache`** argument is ignored.
  - **`accept-hook`** - this should be a function of one argument, the socket which
    was created when a http request was accepted by AllegroServe. The function
    should return a socket for AllegroServe to use. This hook is normally used
    to turn a regular socket into an SSL socket.
  - **`ssl`** - if true then it should be the name of PEM encoded file containing
    the server certificate and the associated private key. This causes the
    server to listen for SSL connections only. The default value of **`port`** is
    made 443 (rather than 80). This makes use of the **`accept-hook`** argument so
    if **`ssl`** is specified then **`accept-hook`** should not be specified. ssl is
    supported only in certain versions of Allegro CL.
  - **`ssl-password`** - if the private key in the PEM encoded file referenced by
    the **`ssl`** argument is encrypted, then this is the key to decrypt it.
  - **`ssl-method`** - see [SSL/TLS](#ssltls) for the use of this argument.
  - **`test-ssl`** - If the use of SSL is specified by other arguments then this
    will cause an SSL test to immediately be done and if it fails an error will
    be signalled. This allows you to verify the SSL certificate is valid before
    the start function returns. The default value for **`test-ssl`** is nil
    (meaning perform no test).
  - **`os-processes`** - if given it should be an integer number of operating system
    processes in which to run AllegroServe. This is available on Unix only at
    the moment. The AllegroServes in different processes do **not** share a
    common Lisp heap..
  - **`external-format`** - If given it should name the value to which
    [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format) should be bound to when requests are
    processed. The default value is **`:latin1-base`**.
  - **`compress`** - if true then the server will send the body gzip compressed if
    the client can accept it and the entity being returned is enabled for
    compression.
  - **`ssl-key`**, **`ssl-password`**, **`ca-file`**, **`ca-directory`**,
    **`verify`** and **`max-depth`** - these values are passed to
    **`make-ssl-server-stream`** (documented with the ACL documentation) should the
    **`ssl`** argument be given. **`ssl-key`** is passed as the value of the **`:key`**
    argument and **`ssl-password`** is passed as the value of the
    **`:certificate-password`** argument to **`make-ssl-server-stream`**. These value
    are only used in a fully patched ACL 8.0 (or newer). In older versions of
    ACL these values are ignored. By specifiying these values you can have more
    control on how the server does SSL certificate managment.

## <span id="shutting-down-the-server"></span>Shutting down the server

-----

<span id="f-shutdown"></span>
#### **`net.aserve:shutdown`**

```lisp
(shutdown &key server save-cache)
```

This shuts down the web server given (or the most recently started web server if
no argument is passed for **`server`**). If **`save-cache`** is given then it should
be the name of a file to which the current state of the proxy cache will be
written. The **`save-cache`** file will only contain in-memory information about
the cache. The cache usually consists of disk files as well and in order to
maintain the complete state of the cache these files must be saved by the user
as well. The information in the **`save-cache`** file refers to the disk cache
files so those disk cache files must exist and be in the same state and location
should the user choose to restore the state of the cache.

## <span id="publishing-information"></span>Publishing information

Once the server is started it will accept requests from http clients, typically
web browsers. Each request is parsed and then AllegroServe searches for an
object to handle that request. That object is called an *entity*. If an entity
is found, it is passed the request and is responsible for generating and sending
a response to the client. If an entity can't be found then AllegroServe sends
back a response saying that that request was invalid.

*Publishing* is the process of creating entities and registering them in the
tables scanned by AllegroServe after a request is read.

### Components of a request

A request from an http client contains a lot of information. The two items that
determine which entity will handle the request are

  - the **`path`** of the url. This is the part of the url after the host name and
    before the query string (if any). For example in the url
    `http://bar.com:8030/files/foo?xx=3\&yy=4` the part we call the path is
    just `/files/foo`. If the path contains escaped characters
    (e.g. `/foo%20bar`) then we replace the %xx in the path with the actual
    character before processing the request. Thus if you're publishing an entity
    to handle a uri such as `http://www.machine.com/foo%20bar` you should
    publish the path `"foo bar"` and *not* `"foo%20bar"`.
  - the **`host`** to which the request is directed. This is not necessarily the
    host that is receiving the request due to virtual hosts and proxy
    servers. This value comes from the `Host:` header line, if one is given.


A request contains other information and while that information isn't used to
determine which entity will handle the request it can be used by the entity
handling the request in any way it sees fit.

The following functions create entities and specify which requests they will
handle. An entity is distinguished by the **`path`** and **`host`** values passed
to the particular [**`publish`**](aserve.md#f-publish) function. When a [**`publish`**](aserve.md#f-publish) is done for a
**`path`** and **`host`** for which there is already an entity assigned, the old
entity is replaced by the new entity.

-----

<span id="f-publish-file"></span>
#### **`net.aserve:publish-file`**

```lisp
(publish-file &key path host port file content-type class preload
                   cache-p remove authorizer server timeout plist hook
                   headers compress)
```

This creates an entity that will return the contents of a file on the disk in
response to a request. The **`url`** and **`file`** must be given, the rest of the
arguments are optional. The arguments have these meanings:

  - **`path`** - a string that must match the name part of the url as described
    above in **Components of a Request**.
  - **`host`** - normally **`nil`**. If you wish to do virtual hosting read
    [this section](#virtual_hosts) describing how it's done.
  - **`port`** - this argument is currently unused and will likely be removed in
    future versions.
  - **`file`** - the name of the file to return when a request to this entity is
    made. The file doesn't have to exist until the request is made unless
    **`preload`** is specified as true.
  - **`content-type`** - A string describing the content of the file. This is
    often referred to as the MIME type of the file. An example is "text/html" to
    describe an html file. If a content-type value is not provided, then
    AllegroServe checks the pathname-type in the [**`*mime-types*`**](aserve.md#v-mime-types) hash table to
    see if there is a content-type associated with this pathname-type. If it
    fails to find a content-type then it uses the type
    "application/octet-stream".
  - **`class`** - a Clos class name or class object to be used to hold this
    entity. The class must be a subclass of **`file-entity`**.
  - **`preload`** -if true it instructs **AllegroServe** to read the contents of
    the file in immediately and store it in a lisp object. This will speed up
    the response to this request. If the file on disk is updated AllegroServe
    will ignore the preloaded content and will access the content from disk. If
    **`preload`** is true then you most likely want to specify **`cache-p`** true as
    well.
  - **`cache-p`** - if true then **AllegroServe** will cache the last value read
    for this file. When asked for this file **AllegroServe** will check to see
    if the file has changed on disk (using the last modified time as a
    measure). If the file hasn't changed AllegroServe will return the cached
    value, otherwise **AllegroServe** will read in and cache the new contents of
    the file and will return that as a response.
  - **`remove`** - instead of adding an entity, remove the entities that match
    the given **`path`** and **`host`**. This removes all entities, not just file
    entities. If a **`host`** value is not passed in an argument, then this will
    remove all entities for this **`path`**, regardless of their **`host`** values.
  - **`server`** - if this entity should only be served by a particular server,
    then this specifies which server. See the section (to be written) on
    running multiple servers in the same Lisp process.
  - **`timeout`** - specifies the number of seconds AllegroServe has to return
    this file to the http client. If AllegroServe is running in a lisp that
    supports timeouts on each I/O operation (e.g. Acl 6.1 or newer) then the
    default value for this argument is a huge number, meaning in effect that
    there will be no time limit on the transfer. If I/O timeouts are not
    supported then the default value of this argument is **`nil`** meaning ignore
    this value and use the timeout value held in the server object and retrieved
    with [**`wserver-response-timeout`**](aserve.md#f-wserver-response-timeout).
  - **`plist`** - initial property list for this entity.
  - **`hook`** - a function of three arguments: **`req`**, **`ent`** and **`extra`**. See
    [entity hook function](#entity-hook-function).
  - **`headers`** - a list of cons's, with the car being a keyword symbol naming
    a header and the cdr being a string which is the header value. These headers
    are added to the response send back to the browser when this file is
    accessed.
  - **`compress`** - if true then this directs the server to look for a
    compressed version of the file if the client will specifies that it will
    accepts a compressed body. The compressed version must be in the same
    directory as the uncompressed version. Currently we only look for files
    compressed with gzip and we identify those files as ending with ".gz". See
    the [Compression](#compression) section.

The function that handles requests for files will respond correctly to
`If-Modified-Since` header lines and thus minimizes network traffic.

Example (this will work on Unix where the password file is stored in `/etc`):

```lisp
(publish-file :path "/password" :file "/etc/passwd" :content-type "text/plain")
```

### <span id="entity-hook-function"></span>Entity Hook Function

AllegroServe supplies many subclasses of entity which automatically generate a
responses to requests. There are times when user code needs to run during the
generation of a response by one of the built-in entity classes. For example you
may wish to add or modify the headers that will be sent back with the
[**`publish-file`**](aserve.md#f-publish-file)'s response. The entity hook function is called just before the
[**`with-http-body`**](aserve.md#f-with-http-body) in the response function. At this point all the response
headers have been specified but the hook function is free to change them or add
new headers.

The entity hook function takes three arguments: **`req`**, **`ent`** and **`extra`**. **`req`**
and **`ent`** are the familiar http-request and entity arguments. **`extra`** is usually
**`nil`** but will be one of the following symbols to tell the hook function if it's
being called in a special context:


| Symbol                                | Description                                                                                                                                            |
|---------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|
| **<code>:illegal&#8209;range</code>** | request has asked for a range of bytes that is not present in the entity. As a result a `"416 - Illegal Range Specified"` response is being generated. |
| **<code>:in&#8209;range</code>**      | request has asked for a range of bytes and that range is being returned.                                                                               |
| **<code>:not&#8209;modified</code>**  | request contains an "If Not Modified" header and AllegroServe is returning a `"304 - Not Modified"` response.                                          |

-----

<span id="f-publish-directory"></span>
#### **`net.aserve:publish-directory`**

```lisp
(publish-directory &key prefix host port destination remove authorizer server
                        indexes filter timeout plist publisher access-file
                        hook headers compress hidden-index-redirect)
```

[**`publish-directory`**](aserve.md#f-publish-directory) is used to publish a complete directory tree of files. This
is similar to how web servers such as Apache publish files. AllegroServe
publishes the files in the directory tree in a *lazy* manner. As files in the
tree are referenced by client requests entities are created and published.

[**`publish-directory`**](aserve.md#f-publish-directory) creates a mapping from all urls whose name begins with
**`prefix`** to files stored in the directories specified by the
**`destination`**. **`destination`** may either be a single directory or a list of
directories to search. The **`host`**, **`port`**, **`remove`**, **`authorizer`**, **`plist`**,
**`hook`**, **`headers`** and **`server`** arguments are as described above for
[**`publish-file`**](aserve.md#f-publish-file). The **`timeout`** argument defaults as described in
[**`publish-file`**](aserve.md#f-publish-file). The **`hook`** argument specifies what hook function should be put
in the entities that [**`publish-directory`**](aserve.md#f-publish-directory) creates. The **`access-file`** argument
names the [access file](#directory-access-files) name which will be used in this
directory tree. When a request comes in for which there isn't an entity that
matches it exactly, AllegroServe checks to see if a prefix of the request has
been registered. If so, and if the resulting entity is a **`directory-entity`** as
created by this function, then it strips the prefix off the given request and
appends the remaining part of the request to the destination string. It then
publishes that (normally using [**`publish-file`**](aserve.md#f-publish-file) and computing the content-type
from the file type). Next that **`file-entity`** is made to handle the request in
the normal manner.

If a request comes that maps to a directory rather than a file then AllegroServe
takes special action. First AllegroServe ensures that the request uri ends in a
slash. If the request was `http://foo.com/onedir/twodir` then AllegroServe will
return a redirect response so the client now asks for
`http://foo.com/onedir/twodir/`. Next AllegroServe tries to locate an index file
for that directory. The **`indexes`** argument specifies a list of index files to
search for. By default the list consists of two filenames "index.html" and
"index.htm". If an index file is found then the value of the
**`hidden-index-redirect`** argument is consulted. If it is **`nil`** then AllegroServe
returns a redirect to the index file, e.g
`http://foo.com/onedir/twodir/index.html`. If **`hidden-index-redirect`** is true
then AllegroServe will return the contents of the index file from the request,
effectively doing the redirection to the index file internally and invisibly to
the client.

The value of the **`filter`** argument is a function of four values: **`req`**, **`ent`**,
**`filename`** and **`info`**. **`req`** and **`ent`** are the request and entity objects that
describe the current client request. **`filename`** is the name of a known file on
the current machine which is being requested by the current request. **`info`** is
the list of [access information](#directory-access-files) for this file.

If the filter returns **`nil`** then the normal operation is done by the
directory-entity handler: the selected file is published and then the request to
access it processed (and subsequent access using that url will just return the
file and never go through the filter again).

If the filter chooses to handle the request for the file itself it must generate
a response to the request and then return a non-nil value. To avoid subsequent
calls to the filter for this file the filter may choose to publish a handler for
this url. If the filter wants to forbid access to this file a handy way to to
call **`(failed-request req)`** and the standard `"404 Not found"` will be sent back
to the client.

The **`publisher`** argument can be used to specify exactly what happens when a
request comes that's handled by the **`directory-entity`** and a file is located
on the disk that matches the incoming **`url`**. Nomally a [**`publish-file`**](aserve.md#f-publish-file) is
done to add that file. You may want to publish some other kind of entity to
represent that file. The **`publisher`** argument, if non-nil, must be a function
of four arguments: **`req ent filename`** **`info`**. The filename is a string
naming the file that's been matched with the request. **`info`** is the list of
[access information](#directory-access-files) for this file. The **`publisher`**
function must return an entity to be processed to send back a response. The
**`publisher`** function may wish to publish that entity but it need not do so.

> *Note*: [**`publish-directory`**](aserve.md#f-publish-directory) is a more general function than its name
> implies. It looks at each url path for a match for **`prefix`** and if such a
> match is found the **`prefix`** is removed and replaced with **`destination`**. Thus
> if prefix is `"/foo"` and destination is `"/bar"` then a url path of
> `"/foobaz/joe.html"` would be converted to `"/barbaz/joe.html"`. This is
> rarely useful but it does show that you have to be careful about the prefix
> and destination strings. It's usually the case that if the prefix string ends
> in `"/"` then the destination string should end in `"/"` (and vice
> versa). Thus a prefix of "/foo" would have a destination of "/bar" and a
> prefix of `"/foo/"` would have a destination of `"/bar/"`.

The **`compress`** argument specifies the value to pass for the **`:compress`** argument
to [**`publish-file`**](aserve.md#f-publish-file) when an entity is located i the directories and a
[**`publish-file`**](aserve.md#f-publish-file) is done automatically.

### <span id="directory-access-files"></span>Directory Access Files

When files are accessed and automatically published you may wish to set some of
the parameters of the entity that is published. As mentioned above you can
define a **`publisher`** function that has complete control in publishing the
entity. A less powerful but easier to use alternative is to place *access files*
in the directory tree being published. An access file specifies information that
you want passed to the publisher function. You can modify these access files
while the directory tree is published and their latest values will be used for
publishing *subsequent* files. This is similar to they way Apache controls its
publishing with `.htaccess` files (except that in AllegroServe once a file is
published the access files have no effect on it).

The name of an access file in AllegroServe is controlled by the **`:access-file`**
argument to [**`publish-directory`**](aserve.md#f-publish-directory). We'll assume the name chosen is **`access.cl`** in
this document. If no **`:access-file`** argument is given to [**`publish-directory`**](aserve.md#f-publish-directory)
then no access file checking is done. When a file is about to be published all
access files from the **`destination`** directory all the way down to the directory
containing the file to be published are read and used. For example if the
**`destination`** in a [**`publish-directory`**](aserve.md#f-publish-directory) was given as `"/home/joe/html/"` and an
http request comes in which references the file
`"/home/joe/html/pics/archive/foo.jpg"` then AllegroServe will check for access
files at *all* of these locations and in this order:

  - `/home/joe/html/access.cl`;
  - `/home/joe/html/pics/access.cl`;
  - `/home/joe/html/pics/archive/access.cl`.

The information is collected as successive access files are read. The new
information is placed before the existing information thus causing subdirectory
access files to possibly shadow information in access files in directories above
it. Also superdirectory access file information is automatically eliminated if
it isn't marked as being *inherited*.

The **`publisher`** function receives the collected information and can do with it
what it wishes. We'll describe what the built-in publisher function does with
the information.

When we speak of *information* in access files we are purposely being vague. We
define what information must look like and what the standard publisher function
does with certain information but we allow users to define their own kinds of
information and use that in their own publisher function.

Each access file consists of zero or more Lisp forms (and possibly lisp style
comments). Each form is a list beginning with a keyword symbol and then followed
by a property-list-like sequence of keywords and values. Nothing in the form is
evaluated. The form cannot contain **`#.`** or **`#,.`** macros.

One information form is used by AllegroServe's directory publisher code to
decide if it's permitted to descend another directory level:

```lisp
(:subdirectories :allow allow-list :deny deny-list :inherit inherit-value)
```

As AllegroServe descends from the **`destination`** directory toward the directory
containing the file to be accessed it stops at each directory level accumlates
the access information and then tests to see if it can descend further based on
the **`:subdirectories`** information. If it cannot descend into the next
subdirectory it gives up immediately and a **`404 - Not Found`** response is
returned. See the section Allow Deny processing below for a description of how
it uses the **`:allow`** and **`:deny`** values.

These other information forms are used by the standard publisher function. Each
takes an **`:inherit`** argument which defaults to false. Information not given
with **`:inherit t`** will be eliminated as AllegroServe descends directory
levels.

| Name             | Arguments                                                                | Meaning                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|------------------|--------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`:ip`**        | **`:patterns`**<br>**`:inherit`**                                  | specifies a [`location-authorizer`](#c-location-authorizer) restriction on which machines can see published files. The value of the **`:patterns`** argument has the same form as the **`:patterns`** slot of a [**`location-authorizer`**](aserve.md#c-location-authorizer).                                                                                                                                                                                                |
| **`:password`**  | **`:realm`**<br>**`:allowed`**<br>**`:inherit`**                   | specifies a [`password-authorizer`](#c-password-authorizer) restriction on access to published files. See the password-authorizer documentation for a description of the **`:realm`** and **`:allowed`** arguments.                                                                                                                                                                                                                       |
| **`:files`**     | **`:allow`**<br>**`:deny`**<br>**`:inherit`**                      | specifies which files are visible to be published. To be visible a file must be allowed and not denied. What is tested is the filename only (that is the part after the last directory separator in the files's complete name). See below for the rules on how allow and denied is used.                                                                                                                                                  |
| **`:directory`** | **`:list`**<br>**`:hidden`**<br>**`:files`**<br>**`:directories`** | **`:list`** specifies whether or not a directory listing is allowed for the directory in which the access file appears.  The default is to disallow directory listings.  **`:hidden`** can be used to ignore files and directories whose names begin with a dot.  The default is to show them.  **`:files`** and **`:directories`** can be used to show or hide files or directories.  By default, both are shown.  |
| **`:mime`**      | **`:types`**<br>**`:inherit`**                                     | specifies which mime types are to be associated with which file types. This list takes precedence over the built-in list inside AllegroServe. **`:types`** is a list of mime specifiers. A mime specifier is a list beginning with a string giving the mime type followed by the files types that should map to that mime type. A file type in a list (e.g. **`("ReadMe")`**) refers to the whole file name rather than the type component. |


### Allow and Deny Processing

The **`:files`** and **`:subdirectories`** information are used to determine if a file
or subdirectory of a given name is accessible. AllegroServe will collect all the
access file information for the directory containing the file or subdirectory
and for all directories above it up to the directory given as the **`destination`**
argument to [**`publish-directory`**](aserve.md#f-publish-directory). Information from superdirectories will only be
used if **`:inherit t`** is given for that information.

The rule is it that a given name is accessible if it is allowed and not denied.
That is the filename or directory name must match one of the allow clauses and
none of the deny clauses. There may be multiple allow and deny clauses since
there may be multiple information forms of the type **`:files`** or
**`:subdirectories`**. Each allow or deny argument can be a string or a list of
strings or nil (which is the same as that argument not being given). The strings
are regular expressions (which are not exactly like unix shell wildcard filename
expressions). In particular `".\*"` is the regular expression that matches
anything.

The special cases are the following:

  - if **`:allow`** is given as nil or is not given at all then that is the same as
    specifying `".\*"` the regular expression that matches everything.
  - if **`:deny`** is given as nil or is not given then that is the same as
    specifying a regular expression that matches nothing.
  - if AllegroServe is looking for **`:files`** information and there is none to be
    found in the accumulated information, then access is allowed. A similar
    thing is true if AllegroServe is searching for **`:subdirectories`** information
    and none is found.

Here is a sample access file:

```lisp
;; only connections to localhost will be able to access the files
(:ip :patterns ((:accept "127.1") :deny) :inherit t)

(:password :realm "mysite"
           :allowed (("joe" . "mypassword")
                     ("sam" . "secret"))
           :inherit t) ;  applies to subdirectories

;; publish html and cgi files, but not those beginning with a period
(:files :allow ("\\\\.html$" "\\\\.cgi$") :deny ("^\\\\."))

;; specify mime type for non-standard file extensions. Also
;; specify that a file named exactly ChangeLog should be given
;; mime type "text/plain"
(:mime :types (("text/jil" "jil" "jlc") ("text/plain" "cl" ("ChangeLog"))))

;; Allow listing the contents of this directory, but don't list dot
;; files or directories
(:directory :list t :hidden t :directories nil)
```

-----

<span id="f-publish"></span>
#### **`net.aserve:publish`**

```lisp
(publish &key path host port content-type function class format remove
              server authorizer timeout plist hook headers)
```

This creates a mapping from a url to a **`computed-entity`**, that is an entity that
computes its response every time a request comes in. The **`path`**, **`host`**, **`port`**,
**`remove`**, **`server`**, **`authorizer`**, **`hook`**, **`headers`** and **`class`** arguments are as
in the other publish functions. The **`timeout`** argument defaults to **`nil`**
always. The **`content-type`** sets a default value for the response to the request
but this can be overridden. The **`format`** argument is either **`:text`** (the
default) or **`:binary`** and it specifies the kind of value that will be sent back
(after the response headers, which are always in text). This value is only
important if the response is generated in a particular way (described
below). The value of the **`hook`** argument is stored in the entity created however
the hook function will only be run if the **`function`** supplied makes use of it.

The **`function`** argument is a function of two arguments: an object of class
**`http-request`** that holds a description of the request, and an object of class
**`entity`** that holds this entity which is handling the request. This function
must generate a response to the http request, even if the response is only that
the request wasn't found.

-----

<span id="f-publish-prefix"></span>
#### **`net.aserve:publish-prefix`**

```lisp
(publish-prefix &key prefix host port content-type function class format remove
                     server authorizer timeout plist hook headers)
```

This is like [**`publish`**](aserve.md#f-publish) except that it declares **`function`** to be the handler for
all urls that begin with the string **`prefix`**. Note however that prefix handlers
have lower priority than exact handlers. Thus if you declare a prefix handler
for `"/foo"` and also a specific handler for `"/foo/bar.html"` then the specific
handler will be chosen if `"/foo/bar.html"` is found in an http request.
Typically a prefix handler is used to make available a whole directory of files
since their complete names begin with a common prefix (namely the directory in
which the files are located). If you want to publish a whole directory then you
probably want to use [**`publish-directory`**](aserve.md#f-publish-directory) since it has a number of features to
support file publishing. The value of the **`hook`** argument is stored in the
entity created however the hook function will only be run if the **`function`**
supplied makes use of it.

-----

<span id="f-publish-multi"></span>
#### **`net.aserve:publish-multi`**

```lisp
(publish-multi &key path host port content-type items class remove server
                    authorizer timeout hook headers)
```

Some web pages are created from information from various sources.
[**`publish-multi`**](aserve.md#f-publish-multi) allows you to specify a sequence of places that supply data for
the combined web page. The data for each page is cached by [**`publish-multi`**](aserve.md#f-publish-multi) so
that minimal computation is required each time the page is requested.

The **`host`**, **`port`**, **`content-type`**, **`class`**, **`remove`**, **`server`**, **`authorizer`**,
**`hook`**, **`headers`** and **`timeout`** arguments are the same as those of the other
publish functions. The **`items`** argument is unique to [**`publish-multi`**](aserve.md#f-publish-multi) and is a
list of zero or more of the following objects:

  - **`string`** or **`pathname`** - this is a reference to a file on the server. This
    item contributes the contents of the file to the final web page.
  - **`symbol`** or **`function`** - this is a function of four arguments: **`req`**, **`ent`**,
    **`cached-time`**, **`cached-value`**. It returns two values: the new value and the
    last modified time of the value. The function may look at the cached-value
    or cached-time and realize that nothing has changed since that time that
    would cause this function to return a new value. In that case it should
    return the cached-value and cached-time that it received as arguments. If a
    value must always be computed each time the function is called it may return
    **`nil`** for the last modified time. This will result in no LastModified header
    being sent in the response. The value the function returns can either be a
    string or an array of unsigned-byte 8 values. It's preferred to return an
    array of unsigned-byte 8 values. If a string is returned then it will be
    converted to an array of unsigned-byte 8 by using **`(string-to-octets string
    :null-terminate nil)`**. The cached-value argument to the function will be
    **`nil`** or an **`(unsigned-byte 8)`** array.
  - **`(:string string)`** - this item supplies the given string to the web page.
  - **`(:binary vector)`** - vector should be a one dimensional **`simple-array`** of
    **`(unsigned-byte 8)`**. This vector of bytes is added to the web page.

Here's an example where we create a page from a fixed header and trailer page
with a bit of dynamic content in the middle.

```lisp
(publish-multi
 :path "/thetime"
 :items (list "header.html"
              (lambda (req ent old-time old-val)
                (declare (ignore req ent old-time old-val))
                (with-output-to-string (p)
                  (html-stream
                   p :br "The time is "
                   (:princ (get-universal-time))
                   (:b "Lisp Universal Time") :br)))
              "footer.html"))
```

## <span id="generating-a-computed-response"></span>Generating a computed response

There are a variety of ways that a response can be sent back to the http client
depending on whether keep-alive is being done, chunking is possible, whether the
response is text or binary, whether the client already has the most recent data,
and whether the size of the body of the response is known before the headers are
sent. AllegroServe handles the complexity of determining the optimal response
strategy and the user need only use a few specific macros in the computation of
the response in order to take advantage of AllegroServe's strategy computation

Here's a very simple computed response. It just puts `"Hello World\!"` in the
browser window:

```lisp
(publish :path "/hello"
         :content-type "text/html"
         :function (lambda (req ent)
                     (with-http-response (req ent)
                        (with-http-body (req ent)
                           (html "Hello World!")))))
```

This example works regardless of whether the request comes in from an old
HTTP/0.9 browser or a modern HTTP/1.1 browser. It may or may not send the
response back with chunked transfer encoding and it may or may not keep the
connection alive after sending back the response. The user code doesn't have to
deal with those possibilities, it just uses [**`with-http-response`**](aserve.md#f-with-http-response) and
[**`with-http-body`**](aserve.md#f-with-http-body) and the rest is automatic. The [**`html`**](htmlgen.md#f-html) macro is part of the
htmlgen package that accompanies AllegroServe. In the case above we are being
lazy and not putting out the html directives that should be found on every page
of html since most browsers are accommodating. Here's the function that
generates the correct HTML:

```lisp
(publish :path "/hello2"
         :content-type "text/html"
         :function (lambda (req ent)
                     (with-http-response (req ent)
                       (with-http-body (req ent)
                        (html (:html (:body "Hello World!")))))))
```

The function above generates: `<html><body>Hello World!</body></html>`.

The macros and functions used in computing responses are these:

-----

<span id="f-with-http-response"></span>
#### **`net.aserve:with-http-response`**

```lisp
(with-http-response (req ent &key timeout check-modified format response
                                  content-type trailers)
                     &rest body)
```

This macro begins the process of generating a response to an http request and
then runs the code in the **`body`** which will actually send out the
response. **`req`** and **`ent`** are the request and entity objects passed into the
function designated to compute the response for the request. **`timeout`** sets a
time limit for the computation of the response. If **`timeout`** is nil then the
entity **`ent`** is checked for a timeout value. If that value is also nil then the
timeout value is retrieved from the current **`wserver`** object using
[**`wserver-response-timeout`**](aserve.md#f-wserver-response-timeout). If **`check-modified`** is true (the default) then the
**`last-modified`** time stored in the entity object will be compared against the
**`if-modified-since`** time of the request and if that indicates that the client
already has the latest copy of this entity then a **`not-modified`** response will
be automatically returned to the client and the **`body`** of this macro will not be
run. **`response`** is an object containing the code and description of the http
response we wish to return. The default value is the value of **``*response-ok*``**
(which has a code of **`200`** and a string descriptor `"OK"`). **`content-type`** is a
string describing the MIME type of the body (if any) sent after the headers. It
has a form like `"text/html"`. If **`content-type`** isn't given here then the
content-type value in the entity (which is set in the call to [**`publish`**](aserve.md#f-publish)) will be
used.

The **`format`** argument specifies whether the code that writes the body of the
response will want to write **`:text`** (e.g. **`write-char`**) or **`:binary`**
(e.g. **`write-byte`**) when it writes the data of the body of the response. Based
on the value of the **`format`** argument, AllegroServe will create the correct kind
of response stream. If **`format`** is not specified here it will default to the
value specified when [**`publish`**](aserve.md#f-publish) was called to create the entity. If not **`:format`**
argument was passed to [**`publish`**](aserve.md#f-publish) then **`:binary`** format is assumed. If **`:binary`**
is specified then you can write both text and binary to the stream since
Allegro's binary streams also support text calls as well. If you specify **`:text`**
then you may end up with a stream that supports only text operations.

The **`trailers`** specifies which header lines will be found in the trailer
instead. It also allows one to specify a value for the trailer (which may be
changed using the [**`set-trailers`**](aserve.md#f-set-trailers) function). All trailers you intend to send must
be declared here. **`trailers`** is list of strings, symbols or a cons of a string
or symbol and a string. For example: **`(:foo "bar" (:baz . "123123") ("bof"
. "456"))`**. Trailers, like headers, are denoted by keyword symbols, with names
being all in the case of the lisp (lower case for Modern lisps and upper case
for ANSi lisps). The conversion between strings denoting trailer and the keyword
symbol is done by AllegroServe in the appropriate manner

An http response consists of a line describing the response code, followed by
headers (unless it's the HTTP/0.9 protocol in which case there are no headers),
and then followed by the body (if any) of the response. [**`with-http-response`**](aserve.md#f-with-http-response)
doesn't normally send anything to the client. It only does so when it determines
that the **`if-modified-since`** predicate doesn't hold and that it must send back a
**`not-modified`** response. Thus is not enough to just call [**`with-http-response`**](aserve.md#f-with-http-response) in
your response function. You must always call [**`with-http-body`**](aserve.md#f-with-http-body) inside the call to
[**`with-http-response`**](aserve.md#f-with-http-response).

-----

<span id="f-with-http-body"></span>
#### **`net.aserve:with-http-body`**

```lisp
(with-http-body (req ent &key format headers external-format)
                 &rest body)
```

This macro causes the whole http response to be sent out. The macro itself will
send out everything except the body of the response. That is the responsibility
of the code supplied as the **`body`** form of the macro. In cases where there is
no body to the response being sent it is still necessary to call
[**`with-http-body`**](aserve.md#f-with-http-body) so that the other parts of the response are sent out, e.g. at
a minimum you should put **`(with-http-body (req ent))`** in the body of a
with-http-response.

*The **`body`** forms may not be executed!* If the request is an http **`head`**
request then the browser wants only the headers returned. The
[**`with-http-body`**](aserve.md#f-with-http-body) macro will not evaulate the **`body`** forms. You must be
aware of this and should never put code in the **`body`** form that absolutely
must be executed when a request is given.

The **`headers`** argument is a list of conses, where the car is the header name
(a keyword symbol) and the cdr is the header value. These headers are added to
the headers sent as part of this response.

Within the **`body`** forms the code calls **`(request-reply-stream req)`** to obtain a
stream to which it can write to supply the body of the response. The
external-format of this stream is set to the value of the **`external-format`**
argument (which defaults to the value of
[**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format)). The variable **`*html-stream*`** is bound to
the value of **`(request-reply-stream req)`** before the **`body`** is evaluated. This
makes it easy to use the [**`html`**](htmlgen.md#f-html) macro to generate html as part of the response.

*Note: there used to be a **`:format`** argument to [**`with-http-body`**](aserve.md#f-with-http-body). That
argument was never used by [**`with-http-body`**](aserve.md#f-with-http-body). The **`:format`** argument has been
moved to [**`with-http-response`**](aserve.md#f-with-http-response) so that it can now have an effect on the stream
created.*

-----

<span id="f-get-request-body"></span>
#### **`net.aserve:get-request-body`**

```lisp
(get-request-body request &key external-format)
```

Return the body of the request as a string. If there is no body the return value
will be an empty string. The result is cached inside the request object, so this
function can be called more than once while processing a request. The typical
reason for there to be a body to a request is when a web browser sends the
result of a form with a POST method. The octets that make up the body of the
request are converted to a string (and then cached) using the **`:octets`**
external format as this is the appropriate external format if the request body
contains a list of form values.

If an **`external-format`** is specified the body is reconverted to a string using
the given external-format and then returned from this function. This
reconversion does not affect the cached value.

-----

<span id="f-get-request-body-incremental"></span>
#### **`net.aserve:get-request-body-incremental`**

```lisp
(get-request-body-incremental request function &key buffer)
```

Reads the body of a PUT or POST **`request`** and passes it to a **`function`**
given by the caller. The function should take two arguments: vector and
count. The vector is a simple-array of **`(unsigned-byte 8)`** and count is the number
of bytes of data in the vector. The final time the function is called the value
of count will be zero indicating there is no more data to follow. The same
vector will be passed on each call.

The caller can pass in a simple-array of **`(unsigned-byte 8)`** as the value of the
**`:buffer`** argument. If no buffer is supplied by the caller one will be
allocated by the function.

This function or [**`get-request-body`**](aserve.md#f-get-request-body) can be called but not both.

This function treats the body of the request as a sequence of bytes. If you wish
to convert it to a string you'll need to collect the whole body and call
**`octets-to-string`** with the appropriate external format.

-----

<span id="f-header-slot-value"></span>
#### **`net.aserve:header-slot-value`**

```lisp
(header-slot-value request header-name)
```

Return the value given in the request for the given header-name (a keyword
symbol). If the header wasn't present in this request then nil will be
returned. [**`header-slot-value`**](aserve.md#f-header-slot-value) is a macro that will expand into a fast
accessor if the **`header-name`** is a constant naming a known header slot. In
older versions of aserve the **`header-name`** was a string.

-----

<span id="f-reply-header-slot-value"></span>
#### **`net.aserve:reply-header-slot-value`**

```lisp
(reply-header-slot-value request header-name)
```

Return the value associated with the header **`header-name`** in the reply sent back
to the client. This function is **`setf`**'able and this is the preferred way to
specify headers and values to be sent with a reply.

-----

<span id="f-request-query"></span>
#### **`net.aserve:request-query`**

```lisp
(request-query request &key uri post external-format)
```

Decode and return an alist of the query values in the request. Each item in the
alist is a cons where the car is a string giving the name of the argument and
the cdr is a string giving the value of the argument.

The query string is in one or both of two places:

  - it begins at the first question mark in the uri and continues until the end
    of the uri or a sharp sign (**`\#`**), whichever comes first.
  - it is in the body of a POST request from a web client.

[**`request-query`**](aserve.md#f-request-query) will by default look in both locations for the query string and
concatenate the results of decoding both query strings. If you would like it to
not check one or both of the locations you can use the **`:uri`** and **`:post`**
keyword arguments. If **`uri`** is true (and true is the default value) then the
query string in the uri is checked. If **`post`** is true (and true is the default
value) and if the request is a POST then the body of the post form will be
decoded for query values.

The **`external-format`** is used in the conversion of bytes in the form to
characters. It defaults to the value of [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format).

A query is normally a set of names and values. `http://foo.com/bar?a=3\&b=4`
yields a query alist **`(("a" . "3") ("b" . "4"))`**. If a name doesn't have an
associated value then the value in the alist is the empty string.
`http://foo.com/bar?a\&b=\&c=4` yields a query alist **`(("a" . "") ("b" . "") (c
. "4"))`**.

-----

<span id="f-request-query-value"></span>
#### **`net.aserve:request-query-value`**

```lisp
(request-query-value key request &key uri post external-format test)
```

This combines a call to [**`request-query`**](aserve.md#f-request-query) to retrieve the alist of query keys and
values, with a call to **`assoc`** to search for the specific key, and finally with
a call to **`cdr`** to return just the value from the assoc list entry. The **`test`**
argument is the function to be used to test the given key against the keys in
the assoc list. It defaults to **`#'equal`**.

If the given key is *not* present in the query **`nil`** is returned. If the given
key *is* present in the query but doesn't have an associated value then the
empty string is returned.

-----

<span id="f-set-trailers"></span>
#### **`net.aserve:set-trailers`**

```lisp
(set-trailers req-or-stream trailers)
```

Set values for the trailers sent after the response.

**`req-or-stream`** can be a request object or a stream. If a request object is
passed then the reply stream for the request is used.

**`trailers`** is an assoc list. The keys are the trailer names and the values are
strings. For example: **`((:x-frob . "frib") (:content-type "text/plain"))`**. You
can only specify values for trailers that you've already declared with the
trailers argument to with-http-response. Trailers can only be sent if the
response uses chunked transfer encoding but that is the standard encoding for
HTTP/1.1 so this is rarely going to be an issue. You can use the
[**`can-set-trailers-p`**](aserve.md#f-can-set-trailers-p) function to check if it's possible to set the trailers for
this request. If [**`set-trailers`**](aserve.md#f-set-trailers) cannot set the trailers it does nothing.

-----

<span id="f-can-set-trailers-p"></span>
#### **`net.aserve:can-set-trailers-p`**

```lisp
(can-set-trailers-p req-or-stream)
```

Return true if the [**`set-trailers`**](aserve.md#f-set-trailers) function can be used to set the trailers for
this response. Trailers can only be set for chunking streams, or gzip streams
which send their data to chunking streams. It's very likely that trailers can be
set. This function allows you to test be sure.

-----

## <span id="request-variables"></span>Request Variables

Every request has zero or more *request variables*. A request variable is named
by a lisp string. The value of a request variable can be any lisp
object. Initially a request has zero request variables.

-----

<span id="f-request-variable-value"></span>
#### **`net.aserve:request-variable-value`**

```lisp
(request-variable-value req name)
```

Return the value of the request variable named **`name`**. Return **`nil`** if there is
no such request variable.

You can use **`(setf (request-variable req name) value)`** to set the value of a
request variable.


## <span id="request-object-readers"></span>Request Object Reader and Accessors

The request object contains information about the http request being processed
and it contains information about the response that is being computed and
returned to the requestor. The following functions access slots of the request
object. Those with names beginning with **`request-reply-`** are accessing the
slots which hold information about the response to the request. When a function
is listed as an *accessor* that means that it can be **`setf`**'ed as well as used
to read the slot value.


<span id="f-request-method"></span> **`(request-method request)`** - reader - a
keyword symbol naming the kind of request, typically **`:get`**, **`:put`** or **`:post`**.

<span id="f-request-uri"></span> **`(request-uri request)`** - reader - a uri object
describing the request. If the request contains a `"Host:"` header line then
the value of this header is placed in the **`uri-host`** and **`uri-port`** slots of
this uri object.

<span id="f-request-protocol"></span> **`(request-protocol request)`** - reader - a
keyword symbol naming the http protocol requested. It is either **`:http/0.9`**,
**`:http/1.0`** or **`:http/1.1`**.

<span id="f-request-protocol-string"></span> **`(request-protocol-string
request)`** - reader - a string naming the http protocol requested. It is either
`"HTTP/0.9"`, `"HTTP/1.0"` or `"HTTP/1.1"`.

<span id="f-request-socket"></span> **`(request-socket request)`** - reader -
the socket object through which the request was made and to which the response
must be sent. This object can be used to determine the IP address of the
requestor.

<span id="f-request-wserver"></span> **`(request-wserver request)`** - reader - the
wserver object describing the web server taking this request.

<span id="f-request-raw-request"></span> **`(request-raw-request request)`** -
reader - a string holding the exact request made by the client.

<span id="f-request-reply-code"></span> **`(request-reply-code request)`** -
accessor - the value describes the response code and string we will return for
this request. See the value of the argument **`response`** in
[**`with-http-response`**](aserve.md#f-with-http-response) for more information.

<span id="f-request-reply-date"></span> **`(request-reply-date request)`** -
accessor - the date the response will be made (in Lisp's universal time
format). This defaults to the time when the request arrived.

<span id="f-request-reply-headers"></span> **`(request-reply-headers request)`** -
accessor - an alist of some of the headers to send out with the reply (other
headers values are stored in specific slots of the request object). Each entry
in the alist is a cons where the **`car`** is a keyword symbol holding the header
name and the **`cdr`** is the value (it is printed using the `~a` format
directive). Typically [**`request-reply-headers`**](aserve.md#f-request-reply-headers) isn't used, instead the headers
to be sent are passed as the **`:header`** argument to [**`with-http-body`**](aserve.md#f-with-http-body), or **`(setf
reply-header-slot-value)`** is called.

<span id="f-request-reply-content-length"></span> **`(request-reply-content-length
request)`** - accessor - the value to send as the Content-Length of this response.
This is computed automatically by AllegroServe and thus a user program shouldn't
have to set this slot under normal circumstances.

<span id="f-request-reply-plist"></span> **`(request-reply-plist request)`** -
accessor - this slot holds a property list on which AllegroServe uses to store
less important information. The user program can use it as well.

<span id="f-request-reply-strategy"></span> **`(request-reply-strategy request)`** -
accessor - the strategy is a list of symbols which describe how AllegroServe
will build a response stream and will send back a response. More details will be
given about the possible strategies at a future time.

<span id="f-request-reply-stream"></span> **`(request-reply-stream request)`** -
accessor - This is the stream to be used in user code to send back the body of
the response. This stream must be used instead of the value of
[**`request-socket`**](aserve.md#f-request-socket).

-----

## <span id="cgi-program"></span>CGI Program Execution

The [Common Gateway Interface](http://hoohoo.ncsa.uiuc.edu/cgi/interface.html)
(CGI) specification allows web servers to run programs in response to http
requests and to send the results of the execution of those programs back the web
client. The CGI programs finds information about the request in its environment
variables and, in the case of a PUT or POST request, the body of the request is
sent to standard input of the program.

CGI is a clumsy and slow protocol for extending the behavior of a web server and
is falling out of favor. However there are legacy CGI applications you may need
to call from AllegroServe. You invoke an external program using the CGI
protocol with the [**`run-cgi-program`**](aserve.md#f-run-cgi-program) function.

-----

<span id="f-run-cgi-program"></span>
#### **`net.aserve:run-cgi-program`**

```lisp
(run-cgi-program req ent program &key path-info path-translated script-name
                                      query-string auth-type timeout error-output
                                      env terminate)
```

In response to an http request, this runs **`program`** which must be a string
naming an exectuable program or script followed optionally by command line
arguments to pass to that program. Before the **`program`** is run the environment
variables are set according to the CGI protocol. The **`timeout`** argument is how
long AllegroServe should wait for a response from the **`program`** before giving
up. The default is 200 seconds. The **`error-output`** argument specifies what
should be done with data the cgi program sends to its standard error. This is
described in detail below. The other keyword arguments allow the caller to
specify values for the CGI environment variables that can't be computed
automatically. **`path-info`** specifies the `PATH_INFO` environment variable, and
similarly for **`path-translated, script-name, query-string`** and **`auth-type`**. If
**`query-string`** is *not* given and the **`uri`** that invoked this request contains a
query part then that query part is passed in the `QUERY_STRING` environment
variable. If **`script-name`** is not given then its value defaults to the path of
the uri of the request. If you wish to add or modify the environment variables
set for the cgi process you can specify a value for **`env`**. The value of **`env`**
should be a list of conses, the car of each cons containing the environment
variable name (a string) and the cdr of each cons containing the environment
variable value (a string). **`env`** is checked after all the standard environment
variables are computed and the value given in **`env`** will override the value
computed automatically. On Unix if **`terminate`** is true then after the cgi
program finishes responding (or the timeout period expires) the cgi program will
be forceably killed (first with SIGTERM and then SIGKILL) if remains alive. 

CGI programs send their result to standard output (file descriptor 1 on
Unix). If they encounter problems they often send informative messages to
standard error (file descriptor 2 on Unix). The **`error-output`** argument to
run-cgi-program allows the caller to specify what happens to data sent to
standard error. The possibile values for **`error-output`** are:

| Value              | Description                                                                                                                                                                                                                                                   |
|--------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`nil`**          | The cgi program's standard error is made the same as the Lisp process' standard error. This standard error may not be the same as the current binding of **`*standard-error*`**.                                                                              |
| pathname or string | A file with the given name is opened and standard error is directed to that file.                                                                                                                                                                             |
| **`:output`**      | Standard error is directed to the same place as standard output thus the error messages will be mixed into the result of running the cgi program.                                                                                                             |
| symbol or function | The function is run whenever there is data available to be read from standard error. It must read that data. It must return a true value if it detected an end of file during the read and **`nil`** otherwise.  The function takes arguments: req ent stream |

A typical way of publishing a CGI page is this:

```lisp
(publish :path "/cgi/myprog"
         :function (lambda (req ent)
                      (run-cgi-program req ent "/server/cgi-bin/myprog")))
```

If you're concerned about capturing the error output then here's an example
where we supply a function to collect all the error output into a string. Once
collected we simply print it out here but in a real web server you would want to
store it in a log file.

```lisp
(defun cgierr (req ent)
  (let ((error-buffer (make-array 10
                                  :element-type 'character
                                  :adjustable t
                                  :fill-pointer 0)))
    (net.aserve:run-cgi-program
     req ent
     "aserve/examples/cgitest.sh 4"
     :error-output
     (lambda (req ent stream)
       (declare (ignore req ent))
       (let (eof)
         (loop
           (let ((ch (read-char-no-hang stream nil :eof)))
             (if* (null ch) then (return))
             (if* (eq :eof ch)
                then (setq eof t)
                     (return))
             (vector-push-extend ch error-buffer)))
         eof)))
    (format t "error buffer is ~s~%" error-buffer)))
```

> *Note*: The ability to run CGI programs from AllegroServe was due to features
> added in Allegro Common Lisp version 6.1. This will not work in earlier
> versions of Allegro CL.

## <span id="form-processing"></span>Form Processing

Forms are used on web pages in order to allow the user to send information to
the web server. A form consists of a number of objects, such as text fields,
file fields, check boxes and radio buttons. Each field has a name. When the
user takes a certain action, the form data is encoded and sent to the web
server. There are three ways that data can be sent to the web server. The
method used is determined by the attributes of the `<form>` tag that defines the
form

  - `<form method="get">` - The data is made part of the **`url`** that is sent to
    the web server and is separated from the url itself by a question mark. The
    AllegroServe url handler code uses **`(request-query req)`** to retrieve the
    alist of form names and values. This method has a few disadvantages - the
    amount of data that can be sent is limited since the size of urls is
    limited. Also the data is visible to everyone seeing the url and that may
    not be desirable.
  - `<form method="post">` - The data is sent in the body of the request. The
    AllegroServe url handler should call **`(request-query req)`** to retrieve and
    decode the data posted. In this case [**`request-query`**](aserve.md#f-request-query) calls
    **`(get-request-body req)`** to retrieve the body from the web browser and then
    **`(form-urlencoded-to-query body)`** to turn it into an alist that associates
    form field names with values.
  - `<form method="post" enctype="multipart/form-data">` - The data is sent in
    the body of the request in MIME format, with each field in its own separate
    MIME entity. This method is only necessary when one of the fields in the
    form is a `<input type="file">` since that causes the whole contents of a
    file to be sent from the browser to the web server. When sending a file you
    would like to include information such as the filename and content-type of
    the file, and by sending it in MIME format there is room for this
    information in the MIME header. We describe how to retrieve data from such
    a form next.

### Retrieving multipart/form-data information

If you create a form with `<form method="post" enctype="multipart/form-data">`
then your url handler must do the following to retrieve the value of each field
in the form:

  1. Call **`(get-multipart-header req)`** to return the MIME headers of the next
     field. If this returns nil then there are no more fields to
     retrieve. You'll likely want to call [**`parse-multipart-header`**](aserve.md#f-parse-multipart-header) on the result
     of [**`get-multipart-header`**](aserve.md#f-get-multipart-header) in order to extract the important information
     from the header.
  2. Create a buffer and call **`(get-multipart-sequence req buffer)`** repeatedly
     to return the next chunk of data. When there is no more data to read for
     this field, [**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence) will return nil. If you're willing to
     store the whole multipart data item in a lisp object in memory you can call
     [**`get-all-multipart-data`**](aserve.md#f-get-all-multipart-data) instead to return the entire item in one Lisp
     object.
  3. Go back to step 1.

It's important to retrieve all of the data sent with the form, even if that data
is just ignored. This is because there may be another http request following
this one and it's important to advance to the beginning of that request so that
it is properly recognized.

Details on the functions are given next.

-----

<span id="f-get-multipart-header"></span>
#### **`net.aserve:get-multipart-header`**

```lisp
(get-multipart-header request)
```

This returns nil or the MIME headers for the next form field in alist form. If
nil is returned then there is no more form data. See [**`parse-multipart-header`**](aserve.md#f-parse-multipart-header)
for a simple way to extract information from the header.

For an input field such as `<input type="text" name="textthing">` the value
returned by [**`get-multipart-header`**](aserve.md#f-get-multipart-header) would be

```lisp
((:content-disposition (:param "form-data" ("name" . "textthing"))))
```

For an input field such as `<input type="file" name="thefile">` the value
returned by [**`get-multipart-header`**](aserve.md#f-get-multipart-header) would be something like

```lisp
((:content-disposition
      (:param "form-data" ("name" . "thefile")
                          ("filename" . "C://down//550mhz.gif")))
 (:content-type "image/gif"))
```

Note that the filename is expressed in the syntax of the operating system on
which the web browser is running. This syntax may or may not make sense to the
Lisp pathname functions of the AllegroServe web server as it may be running on a
totally different operating system.

-----

<span id="f-parse-multipart-header"></span>
#### **`net.aserve:parse-multipart-header`**

```lisp
(parse-multipart-header header)
```

This takes the value of get-multipart-header and returns values that describe
the important information in the header.

The first value returned is

  - **`:eof`** - this header says that there are no more multipart items. This
      value is returned when the value of **`header`** is **`nil`**.
  - **`:data`** - the next multipart item is a simple form value. The second value
    returned is a string naming the value. You can retrieve the value itself
    using repeated calls to [**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence) or one call to
    [**`get-all-multipart-data`**](aserve.md#f-get-all-multipart-data).
  - **`:file`** - the next multipart item is a file the user is uploading to the
    server. The second value returned in the name of the form item for which
    this file was given. The third value is the name of the file as specified by
    the user to his browser. The fourth value returned is the MIME Content-Type
    that the browser is guessing applies to this contents of the file. The
    contents of the file can be retrieved using repeated calls to
    [**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence) or one call to [**`get-all-multipart-data`**](aserve.md#f-get-all-multipart-data).
  - **`:nofile`** - If a form contains a place for a filename but no filename was
    entered before the form was submitted then this type of header is sent. The
    values returned are the same as those for **`:file`** except that the third
    value (the filename) will always be the empty string. Just like in the
    **`:file`** case you retrived the contents with [**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence) or
    **`get-multipart-data`**.
  - **`nil`** - This header has a form not recognized by [**`parse-multipart-header`**](aserve.md#f-parse-multipart-header).
    If you encounter this please let us know about it since we would like to
    enhance [**`parse-multipart-header`**](aserve.md#f-parse-multipart-header) to understand this type of header. If you
    encounter this type of header you still have to read the contents of the
    data item that follows the header in order to read the next header. A call
    to **`(get-all-multipart-data req :limit 1000)`** will read and throw away the
    following value so you can then read the next header.

-----

<span id="f-get-multipart-sequence"></span>
#### **`net.aserve:get-multipart-sequence`**

```lisp
(get-multipart-sequence request buffer &key start end external-format)
```

This retrieves the next chunk of data for the current form field and stores it
in **`buffer`**. If [**`start`**](aserve.md#f-start) is given then it specifies the index in the buffer at
which to begin storing the data. If **`end`** is given then it specifies the index
just after the last index in which to store data.

The return value is **`nil`** if there is no more data to return, otherwise it is
the index one after the last index filled with data in **`buffer`**.

The buffer can be a one dimensional array of **`character`** or of **`(unsigned-byte
8)`**. For the most efficient transfer of data from the browser to AllegroServe,
the program should use a 4096 byte **`(unsigned-byte 8)`** array.

If the buffer is a character array then the data is converted from
get-multipart-sequence's **`(unsigned-byte 8)`** array to characters using the given
**`external-format`** (which defaults to the value of
[**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format)).

[**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence) may return before filling up the whole
buffer, so the program should be sure to make use of the index returned
by [**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence).

-----

<span id="f-get-all-multipart-data"></span>
#### **`net.aserve:get-all-multipart-data`**

```lisp
(get-all-multipart-data request &key type size external-format limit)
```

This retrieves the complete data object following the last multipart header. It
returns it as a lisp object. If **`type`** is **`:text`** (the default) then the result
is returned as a lisp string. If **`type`** is **`:binary`** then the result is
returned as an array of element-type **`(unsigned-byte 8)`**. **`size`** (which defaults
to 4096) is the size of the internal buffers used by this function to retrieve
the data. You usually won't need to specify a value for this but but if you
know the values retrieved are either very small or very large it may make the
operation run faster to specify an appropriate **`size`**. **`external-format`** is
used when **`type`** is **`:text`** to convert the octet stream into characters. It
defaults to the value of [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format). **`limit`** can be
given an integer value that specifies the maximum size of data you're willing to
retrieve. By default there is no limit. This can be dangerous as a user may try
to upload a huge data file which will take up so much Lisp heap space that it
takes down the server. If a **`limit`** is given and that limit is reached,
[**`get-all-multipart-data`**](aserve.md#f-get-all-multipart-data) will continue to read the data from the client until it
reaches the end of the data, however it will *not* save it and will return the
symbol **`:limit`** to indicate that the data being sent to the sever exceeded the
limit. It will return a second value which is the size of the data the client
tried to upload to the server. If your application intends to handle very large
amounts of data being uploaded to the server you would be better off using
[**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence) since with that you can write the data buffer by buffer
to the disk instead of storing it in the Lisp heap.

------

In AllegroServe the information sent to the web server as a result of filling
out a form is called a **`query`**. We store a query as a list of
**`cons`**&#8203;es, where the **`car`** of the **`cons`** is the name (a
string) and the **`cdr`** of the cons is the value (another string). When a
query is transmitted by the web browser to AllegroServe it is sent as string
using the encoding `application/x-www-form-urlencoded`. We provide the following
functions to convert between the encoding and the query list:

-----

<span id="f-form-urlencoded-"></span>
#### **`net.aserve:form-urlencoded-to-query`**

```lisp
(form-urlencoded-to-query string &key external-format)
```

Decodes the string and returns the query list. The default value for
**`external-format`** is the value of [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format).

-----

<span id="f-query-to"></span>
#### **`net.aserve:query-to-form-urlencoded`**

```lisp
(query-to-form-urlencoded query &key external-format)
```

Encodes the query and returns a string. The default value for **`external-format`**
is the value of [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format).



Examples:

```lisp
user(4): (query-to-form-urlencoded '(("first name" . "joe")
                                     ("last name" . "smith")))
"first+name=joe&last+name=smith"

user(5): (form-urlencoded-to-query "first+name=joe&last+name=smith")
(("first name" . "joe") ("last name" . "smith"))

user(6): (query-to-form-urlencoded
            (("last name" . ,(coerce '(#\hiragana_letter_ta
                                       #\hiragana_letter_na
                                       #\hiragana_letter_ka)
                                     'string)))
            :external-format :euc)
"last+name=%a4%bf%a4%ca%a4%ab"

user(7): (query-to-form-urlencoded
            (("last name" . ,(coerce '(#\hiragana_letter_ta
                                       #\hiragana_letter_na
                                       #\hiragana_letter_ka)
                                     'string)))
            :external-format :shiftjis)
"last+name=%82%bd%82%c8%82%a9"

user(8): (coerce
           (cdr
              (assoc "last name"
                (form-urlencoded-to-query "last+name=%82%bd%82%c8%82%a9"
                                      :external-format :shiftjis)
                :test #'equalp))
           'list)
(#\hiragana_letter_ta #\hiragana_letter_na #\hiragana_letter_ka)
```

## <span id="authorization"></span>Authorization

You may want to restrict certain entities to be accessible from only certain
machines or people. You can put the test for authorization in the entity
response function using one of the following functions, or you can have the
check done automatically by storing a list of **`authorizer`** objects in the
entity.

### Functions

These two functions invoke and process the *Basic* Authorization Method defined
by the http specification. The [**`password-authorizer`**](aserve.md#c-password-authorizer) class described below make
use of these functions.

-----

<span id="f-get-basic-authorization"></span>
#### **`net.aserve:get-basic-authorization`**

```lisp
(get-basic-authorization request)
```

This function retrieves the Basic authorization information associated with this
request, if any. The two returned values are the name and password, both
strings. If there is no Basic authorization information with this request, **`nil`**
is returned.

-----

<span id="f-set-basic-authorization"></span>
#### **`net.aserve:set-basic-authorization`**

```lisp
(set-basic-authorization request realm)
```

This adds a header line that requests Basic authorization in the given realm (a
string). This should be called between [**`with-http-response`**](aserve.md#f-with-http-response) and
[**`with-http-body`**](aserve.md#f-with-http-body) and only for response of type 401
(i.e. **`*response-unauthorized*`**). The realm is an identifier, unique on this
site, for the set of pages for which access should be authorized by a certain
name and password.

This example manually tests for basic authorization where the name is
`foo` and the password is `bar`.

```lisp
(publish :path "/secret"
    :content-type "text/html"
    :function
    (lambda (req ent)
      (multiple-value-bind (name password) (get-basic-authorization req)
         (if* (and (equal name "foo") (equal password "bar"))
           then (with-http-response (req ent)
                  (with-http-body (req ent)
                    (html (:head (:title "Secret page"))
                          (:body "You made it to the secret page"))))
           else ; this will cause browser to put up a name/password dialog
                (with-http-response (req ent :response *response-unauthorized*)
                   (set-basic-authorization req "secretserver")
                   (with-http-body (req ent)))))))
```

### **`authorizer`** classes

The authorizer slot of an entity object can contain a **`authorizer`** object or a
list of zero or more **`authorizer`** objects. When a request arrives for this
entity the **`authorizer`** objects are consulted to see if this request should be
permitted. In order for the request to be permitted, *all* authorizer objects
must permit the request. AllegroServe supplies three interesting subclasses of
**`authorizer`** and users are free to add their own subclasses to support their own
authorization needs.

The protocol followed during authorization is this:

 1. An entity object is selected that matches the request. The value of the
    entity's authorizer slot is retrieved from the entity object.
 2. If the list of pending authorizer objects is **`nil`** then it is considered
    authorized.
 3. Otherwise the [**`authorize`**](aserve.md#f-authorize) generic function is called on the first
    **`authorizer`** object, passing [**`authorize`**](aserve.md#f-authorize) the **`authorizer`** object, the
    http-request object and the entity object
 4. The return value from [**`authorize`**](aserve.md#f-authorize) can be
    - **`t`** - meaning this request is authorized to access this entity. In this
      case the first authorizer object is popped from the list of pending
      authorizer objects and we go back to step 2. 
    - **`nil`** - meaning that this request isn't authorized. The response from
      AllegroServe will be the standard "failed request" response so the user
      won't be able to distinguish this response from one that would be received
      if the entity didn't exist at all.
    - **`:deny`** - a denied request response will be returned. It will **`not`** use
      the 401 return code so this will not cause a password box to be displayed
      by the browser. 
    - **`:done`** - the request is denied, and a response has already been sent to
      the requestor by the [**`authorize`**](aserve.md#f-authorize) function so no further response should be
      made.

-----

<span id="c-password-authorizer"></span>
#### **`net.aserve:password-authorizer`** \[class\]

This subclass of **`authorizer`** is useful if you want to protect an entity using
the Basic authorization scheme that asks for a name and a password. When you
create this class of object you should supply values for the two slots:

| Slot Name     | initarg        | Description                                                                                                                                                                  |
|---------------|----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`allowed`** | **`:allowed`** | list of conses, each cons having the form **`("name" . "password")`** where any of the listed name password pairs will allow access to this page.                            |
| **`realm`**   | **`:realm`**   | A string which names the protection space for the given name and password. The realm will appear in the dialog box the browser displays when asking for a name and password. |

An example of its use is the following where we allow access only if the user
enters a name of **`joe`** and a password of **`eoj`** or a name of **`fred`** and a
password of **`derf`**.

```lisp
(publish :path "/foo"
  :content-type "text/html"
  :authorizer (make-instance 'password-authorizer
                     :allowed '(("joe" . "eoj")
                                ("fred" . "derf"))
                     :realm "SecretAuth")

  :function
  (lambda (req ent)
    (with-http-response (req ent)
       (with-http-body (req ent)
          (html (:head (:title "Secret page"))
                (:body "You made it to the secret page"))))))
```

-----

<span id="c-location-authorizer"></span>
#### **`net.aserve:location-authorizer`** \[class\]

This authorizer class checks the IP address of the request to see if it is
permitted access to the entity. The authorizer can specify a sequence of
patterns and for each pattern a command of **`:accept`** (permit the access) or
**`:deny`** (forbid the access). The first pattern that matches determines if the
request is accepted or denied. If the pattern list is empty or if no pattern
matches, then the request is accepted.

The single slot of an object of class [**`location-authorizer`**](aserve.md#c-location-authorizer) is

| Slot Name      | initarg         | Description                                                                                |
|----------------|-----------------|--------------------------------------------------------------------------------------------|
| **`patterns`** | **`:patterns`** | a list of patterns and commands, where the syntax of a pattern-command is described below. |

A pattern can be

  - **`:accept`** - this is a pattern that matches all IP addresses and causes the
    access to be authorized
  - **`:deny`** - this is a pattern that matches all IP addresses and causes the
    access to be rejected
  - **`(:accept ipaddress [bits])`** - if the request's IP address matches the most
    significant **`bits`** of **`ipaddress`** then this access is accepted. **`bits`** is
    optional and defaults to 32 (the whole address). The ipaddress can be an
    integer (the 32 bit IP address) or it can be a string in either dotted form
    `"123.23.43.12"` or a host name `"foo.bar.com"`. In the case of a host
    name, a lookup must be done to map the host name to an IP address. If this
    lookup fails then it is assumed that the pattern doesn't match. If
    **`ipaddress`** is a string, then the first time it is examined during
    authorization it is converted to an integer IP address and that value
    replaces the string in the pattern (thus caching the result of the
    conversion to an IP address).
  - **`(:deny ipaddress [bits])`** - just like the case above except the request is
    rejected if it matches the **`ipaddress`**. One difference is this: if the
    **`ipaddress`** is a host name and that host name cannot be translated to an IP
    address, then it is assumed to match, and thus the request will be denied.

The example of using a [**`location-authorizer`**](aserve.md#c-location-authorizer) only permits connections coming in
via the loopback network (which occurs if you specify
`http://localhost/whatever`) or if they come from one particular machine
(tiger.franz.com). Note that we end the pattern list with **`:deny`** so that
anything not matching the preceding patterns will be denied.

```lisp
(publish :path "/local-secret-auth"
    :content-type "text/html"
    :authorizer (make-instance 'location-authorizer
                         :patterns '((:accept "127.0" 8)
                                     (:accept "tiger.franz.com")
                                     :deny))

    :function
    (lambda (req ent)
      (with-http-response (req ent)
         (with-http-body (req ent)
             (html (:head (:title "Secret page"))
                   (:body (:b "Congratulations. ")
                     "You made it to the secret page"))))))
```

-----

<span id="c-function-authorizer"></span>
#### **`net.aserve:function-authorizer`** \[class\]

This authorizer contains a function provided by the user which is used to test
if the request is authorized. The function take three arguments, the
http-request object, the entity and the authorizer object. It must return one
of the four values that the [**`authorize`**](aserve.md#f-authorize) function returns, namely **`t`**, **`nil`**,
**`:deny`** or **`:done`**.

A function-authorizer is created as follows

```lisp
(make-instance 'function-authorizer ;; always authorize
    :function (lambda (req ent auth) t))
```

The function slot can be set using **`(setf function-authorizer-function)`** if you
wish to change it after the authorizer has been created.

## <span id="cookies"></span>Cookies

Cookies are name value pairs that a web server can direct a web browser to save
and then pass back to the web server under certain circumstances. Some users
configure their web browsers to reject cookies, thus you are advised against
building a site that depends on cookies to work.

Each cookie has these components:

  1. **`name`** - a string. Since you can get multiple cookies sent to you by a web
      browser, using a unique name will allow you to distinguish the values.
  2. **`value`** - a string.
  3. **`path`** - a string which must be the prefix of the request from the web
      browser for this cookie to be sent. The string "/" is the prefix of all
      requests.
  4. **`domain`** - a string which must be the suffix of the name of the machine
      where the request is being sent in order for this cookie to be sent.
  5. **`expiration`** - a time when this cookie expires.
  6. **`secure`** - either true or false. If true then this cookie will only be sent
      if the connection is through a secure socket.
  7. **`http-only`** - either true or false. If true then this cookie will only be
      sent with the HttpOnly flag in the cookie specification.

-----

<span id="f-set-cookie-header"></span>
#### **`net.aserve:set-cookie-header`**

```lisp
(set-cookie-header request &key name value expires domain path secure http-only
                                encode-value external-format)
```

This function must be called before [**`with-http-body`**](aserve.md#f-with-http-body). It can be called more
than once. Each call will cause one Set-Cookie directive to be sent to the web
browser. The **`name`** and **`value`** arguments should be given (and they should be
strings). They will be automatically encoded using the same encoding used in
urls (we call it *uriencoding*). The purpose of this encoding is to convert
characters that are either unprintable or those that have a special meaning into
a printable string. The web browser doesn't care about the **`name`** and **`value`**,
it just stores them and sends them back to the web server. If you use the
[**`get-cookie-values`**](aserve.md#f-get-cookie-values) function to retrieve the cookie **`name`** and **`value`** pairs,
then it will automatically decode the uriencoding.

You can disable the encoding of the value by specifying a **`nil`** value to
**`encode-value`**. This should only be necessary if you are working with buggy
http client applications.

If the **`path`** argument isn't given, it will default to `"/"` which will allow
this cookie to match all requests. If the **`domain`** argument isn't given then it
will default to the host to which this request was sent. If you wish to specify
this you are only allowed to specify a subsequence of the host to which this
request was sent (i.e the name of the machine running the webserver). The
**`domain`** should have at least two periods in it (i.e. `".foo.com"`). **`expires`**
can be a lisp universal time or it can be the symbol **`:never`** meaning this
should never expire. If **`expires`** isn't given or is **`nil`** then this cookie will
expire when the user quits their web browser. **`secure`** should be true or
false. Any non-nil value is interpreted as true. The default value is false.
**`http-only`** should be true or false. Any non-nil value is interpreted as
true. The default value is false. The **`external-format`** is used to convert
bytes to characters. It defaults to the value of
[**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format).

-----

<span id="f-get-cookie-values"></span>
#### **`net.aserve:get-cookie-values`**

```lisp
(get-cookie-values request &key external-format)
```

Return the cookie **`name`** and **`value`** pairs from the header of the request. Each
**`name`** **`value`** pair will be in a cons whose **`car`** is the **`name`** and whose **`cdr`**
is the **`value`**. The names and values will be decoded (in other words the
decoding done by [**`set-cookie-header`**](aserve.md#f-set-cookie-header) will be undone). The **`external-format`** is
used to convert bytes to characters. It defaults to the value of
[**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format).

-----

## <span id="variables"></span>Variables

These special variables contain information about AllegroServe or help control
AllegroServe:

<span id="v-aserve-version"></span> **`net.aserve:*aserve-version*`** - a list
of three values: **`(major-version minor-version sub-minor-version)`** which is
usually printed with periods separating the values (i.e. X.Y.Z).

<span id="v-default-aserve-external-format"></span>
**`net.aserve:*default-aserve-external-format*`** - a symbol or external format
object which is the default value for those AllegroServe functions that take an
external-format argument. http requests are normally run in separate lisp
threads and those threads bind [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format) to the
value of the external-format argument to the start function. Thus changing the
value of [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format) in one thread will not affect
its value in other threads. You should decide the default external format before
you start AllegroServe running.

<span id="v-http-response-timeout"></span>
**`net.aserve:*http-response-timeout*`** - the default value for the timeout
argument to with-http-response. \[in future versions of AllegroServe we'll treat
this value like [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format) and bind it in each
worker thread\]

<span id="v-http-free-worker-timeout"></span>
**`net.aserve:*http-free-worker-timeout*`** - the number of seconds that
AllegroServe will wait for a worker thread to become available (see
[AllegroServe request processing protocol](#iseve-request-proc) for more
details).

<span id="v-mime-types"></span>
**`net.aserve:*mime-types*`** - a hash table where the keys are the file types
(e.g. `"jpg"`) and the values are the MIME types (e.g. `"image/jpeg"`).

-----

## <span id="iseve-request-proc"></span>AllegroServe request processing protocol

We'll describe here the steps AllegroServe goes through from the time it
receives a request until a response to that request has been sent back to the
browser. We want the protocol to be open so that users can extend
AllegroServe's behavior to suit their needs. However given that AllegroServe is
a new program and will be undergoing extensive review from its users, we expect
that the protocol will change. It shouldn't lose any of its current
extensibility but the names and argument lists of generic functions may change.

When a client connects to the port on which AllegroServe is listening,
AllegroServe passes that connected socket to a free worker thread which then
wakes up and calls the internal function **`net.aserve::process-connection`**. If
there are no free worker threads then AllegroServe waits for one to be
available. The wait time can be configured either globally
via the [**`*http-free-worker-timeout*`**](aserve.md#v-http-free-worker-timeout) variable which defaults to 3
seconds, or on a per-server basis via the setting

<span id="f-wserver-free-worker-timeout"></span>
**`(wserver-free-worker-timeout wserver)`** \[accessor\]

which is an accessor to the **`wserver`** slot that can be set with
**`:free-worker-timeout`** initialization argument and is by default initialized
with the value of [**`*http-free-worker-timeout*`**](aserve.md#v-http-free-worker-timeout).

In each worker thread the variable **`*wserver*`** is bound to the **`wserver`** object
that holds all the information about the webserver on which the connection was
made (remember that one AllegroServe process can be running more than one
webserver). **`process-connection`** reads the request from the socket (but doesn't
read past the header lines). If the request can't be read within
**`*read-request-timeout*`** seconds (currently 20) then the request is rejected.
The request is stored in an object of class **`http-request`**. Next
**`process-connection`** calls [**`handle-request`**](aserve.md#f-handle-request) to do all the work of the request
and then **`log-request`** to log the action of the request. Finally if the
response to the request indicated that the connection was to be kept open rather
than being closed after the response, then **`process-connection`** loops back to
the top to read the next request.

<span id="f-handle-request"></span>
**`(handle-request (req http-request))`** \[generic function\]

This generic function must locate the entity to handle this request and then
cause it to respond to the request. If there is no matching entity then
[**`handle-request`**](aserve.md#f-handle-request) must send a response back to the client
itself. [**`handle-request`**](aserve.md#f-handle-request) uses locators to find the entity (more on this below),
and then if an entity is found and that entity has an authorizer, it calls
[**`authorize`**](aserve.md#f-authorize) to see if this request is allowed to access the selected entity. If
the entity passes the authorization then [**`process-entity`**](aserve.md#f-process-entity) is called to cause the
entity to respond to the request. [**`process-entity`**](aserve.md#f-process-entity) returns true if it processed
the entity, and nil if did not in which case the search continues for an
entity. If there is no entity to respond then [**`failed-request`**](aserve.md#f-failed-request) is called to send
back a failure message.

A **`locator`** is an object used to map requests into entities. The value of
**`(wserver-locators *wserver*)`** is a list of locator objects. [**`handle-request`**](aserve.md#f-handle-request)
calls

<span id="f-standard-locator"></span>
**`(standard-locator (req http-request) (loc locator))`** \[generic function\]

on each successive locator in that list until one returns an entity object.
AllegroServe has two built-in locator classes, **`locator-exact`** and
**`locator-prefix`**, that are subclasses of **`locator`**. When you call [**`publish`**](aserve.md#f-publish) or
[**`publish-file`**](aserve.md#f-publish-file) you are adding the entity to locator of class **`locator-exact`**
found in the **`wserver-locators`** list. When you call [**`publish-directory`**](aserve.md#f-publish-directory) you are
adding to the locator of class **`locator-prefix`**. Users are free to define new
locator classes. Locators should define the [**`standard-locator`**](aserve.md#f-standard-locator) method as well
as

<span id="f-unpublish-locator"></span>
**`(unpublish-locator (loc locator))`** \[generic function\]

which if called should remove all published entities from the locator.

Let's return to [**`handle-request`**](aserve.md#f-handle-request). It has called [**`standard-locator`**](aserve.md#f-standard-locator) and found an
entity. Next it checks to see if the entity has an authorizer value and if so
calls

<span id="f-authorize"></span>
**`(authorize (auth authorizer) (req http-request) (ent entity))`** \[generic function\]

whose return value will be one of

  - **`t`** - The request is authorized, call [**`process-entity`**](aserve.md#f-process-entity) to make the entity
    respond.
  - **`nil`** - The request is not authorized, call [**`failed-request`**](aserve.md#f-failed-request) to send back a
    response.
  - **`:deny`** - The request is denied and we want the user to know that it was
    denied rather than sending a generic failed message, call [**`denied-request`**](aserve.md#f-denied-request)
    to send back a response.
  - **`:done`** - The [**`authorize`**](aserve.md#f-authorize) function has sent back a response, there is
    nothing more for [**`handle-request`**](aserve.md#f-handle-request) to do for this request.

If there is no authorizer for this entity then we just call [**`process-entity`**](aserve.md#f-process-entity).
If there is no entity, then we call

<span id="f-failed-request"></span>
**`(failed-request (req http-request))`** \[generic function\]

which sends back a response to the effect that the url request doesn't exist on
this server.

<span id="f-denied-request"></span>
**`(denied-request (req http-request))`** \[generic function\]

sends back a response to the effect that access to the requested url was denied.

<span id="f-process-entity"></span>
**`(process-entity (req http-request) (ent entity))`** \[generic function\]

sends back a response appropriate to the given entity. The macros
with-http-response and with-http-body should be used in the code that sends the
response.

## <span id="client-request"></span>Client functions

AllegroServe has a set of functions that perform http client-side actions.
These functions are useful in generating computed pages that reflect the
contents of other pages. We also use the client-side http functions to test
AllegroServe.

The client-side functions described in this section are exported from the
**`net.aserve.client`** package.

The function [**`do-http-request`**](aserve.md#f-do-http-request) sends a request and retrieves the whole response.
This is the most convenient function to use to retrieve a web page.

If you need more control over the process you can use the functions:
[**`make-http-client-request`**](aserve.md#f-make-http-client-request), [**`read-client-response-headers`**](aserve.md#f-read-client-response) and
[**`client-request-read-sequence`**](aserve.md#f-client-request-read-sequence).

-----

<span id="f-do-http-request"></span>
#### **`net.aserve.client:do-http-request`**

```lisp
(do-http-request uri &key method protocol accept
                          content content-type query format cookies
                          redirect redirect-methods
                          basic-authorization
                          digest-authorization no-proxy
                          headers proxy proxy-basic-authorization
                          user-agent external-format ssl ssl-method
                          skip-body timeout compress keep-alive
                          connection
                          certificate key certificate-password ca-file
                          ca-directory verify max-depth)
```

Sends a request to **`uri`** and returns four values:

  1. The body of the response. If there is no body the empty string returned.
  2. The response code (for example, 200, meaning that the request succeeded).
  3. An alist of headers where the **`car`** of each entry is a keyword symbol or a
     lowercase string with the header name and the **`cdr`** is a string with the
     value of that header item. A keyword symbol is returned for standard
     headers.
  4. The uri object denoting the page accessed. This is normally computed from
     the **`uri`** value passed in but if redirection was done then this reflects
     the target of the redirection. If you plan to interpret relative html links
     in the **`body`** returned then you must do so with respect to *this* uri value
  5. If a true value for the **`:keep-alive`** argument was given then this will be
     socket connection to the server if the server chose to accept the
     keep-alive request. This socket can be used as the value of the
     **`:connection`** argument in a subsequent call to [**`do-http-request`**](aserve.md#f-do-http-request) to the
     same server.

The **`uri`** can be a uri object or a string. The scheme of the **`uri`** must be **`nil`**
or `"http"`. The keyword arguments to [**`do-http-request`**](aserve.md#f-do-http-request) are

| Name                                                                                                                       | Default                            | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|----------------------------------------------------------------------------------------------------------------------------|------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`method`**                                                                                                               | **`:get`**                         | The type of request to make. Other possible values are **`:put`**, **`:post`**, **`:patch`** and **`:head`**. **`:head`** is useful if you just want to see if the link works without downloading the data.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| **`protocol`**                                                                                                             | **`:http/1.1`**                    | The other possible value is **`:http/1.0`**. Modern web servers will return the response body in chunks if told to use the **`:http/1.1`** protocol. Buggy web servers may do chunking incorrectly (even Apache has bugs in this regard but we've worked around them). If you have trouble talking to a web server you should try specifying the **`:http/1.0`** protocol to see if that works.                                                                                                                                                                                                                                                                                                                                                              |
| **`accept`**                                                                                                               | `"*/*"`                            | A string listing of MIME types that are acceptable as a response to this request. The type listed can be simple such as `"text/html"` or more complex like `"text/html, audio/*"`.  The default is to accept anything which is expressed `"*/*"`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| **`content`**                                                                                                              | **`nil`**                          | If the method is **`:put`**, **`:patch`** or **`:post`** then the request should include something to be sent to the web server. The value of this argument is either a string, a vector of type **`(unsigned-byte 8)`**, a pathname, a stream or an instance of a subclass of [**`computed-content`**](aserve.md#c-computed-content).  It may also be a list containing strings, vectors, pathnames, streams or instances of a subclass of [**`computed-content`**](aserve.md#c-computed-content). See the **`query`** argument for a more convenient way to **`:post`** data to a form.                                                                                                                                                                                                                                                  |
| **`content-type`**                                                                                                         | **`nil`**                          | A string which is to be the value of the Content-Type header field, describing the format of the value of the **`content`** argument.  This is only needed for **`:put`**, **`:patch`** and **`:post`** requests.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| **`query`**                                                                                                                | **`nil`**                          | This is a query alist of the form suitable for [**`query-to-form-urlencoded`**](aserve.md#f-query-to). If the method is a **`:get`** then the value of  this argument is **`urlencoded`** and made the query string of the uri being accessed. If the method is **`:post`** then the query string is **`urlencoded`** and made the **`content`** of the request. Also the **`content-type`** is set to **`application/x-www-form-urlencoded.`**                                                                                                                                                                                                                                                                                                              |
| **`format`**                                                                                                               | **`:text`**                        | The body of the response is returned as a string if the value is **`:text`** or as an array of type (unsigned-byte 8) if the value is **`:binary`**.  When the body is a string the external-format argument is important.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| **`cookies`**                                                                                                              | **`nil`**                          | If you wish the request to include applicable cookies and for returned cookies to be saved, then a [**`cookie-jar`**](aserve.md#c-cookie-jar) object should be passed as the value of this argument.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| **`redirect`**                                                                                                             | **`5`**                            | If the response is a redirect (code 301, 302, 303), and the method is one given by the value of **`redirect-methods`** then if this argument is true (and, if an integer, positive), [**`do-http-request`**](aserve.md#f-do-http-request) will call itself to access the page to which the redirection is pointed. If **`redirect`** is an integer then in the recursive call the value passed for **`redirect`** will be one less than the current value. This prevents infinite recursion due to redirection loops.                                                                                                                                                                                                                                        |
| **`redirect-methods`**                                                                                                     | **<code>(:get&nbsp;:head)</code>** | List of http methods which will be redirected if **`redirect`** is true.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| **`basic-authorization`**                                                                                                  | **`nil`**                          | If given, it is a cons whose **`car`** is the name and whose **`cdr`** is the password to be used to get authorization to access this page.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| **`digest-authorization`**                                                                                                 | **`nil`**                          | If given it is a **`digest-authorization`** object with values in the **`username`** and **`password`** slots. The digest-authorization object will be modified by do-http-request in order to store the information needed to send as authorization credentials.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| **`headers`**                                                                                                              | **`nil`**                          | An alist of conses **`(header-name . "header-value")`** for additional headers to send with the request. The header-name should be a keyword symbol naming the header, but it can be a string as well.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| **`proxy`**                                                                                                                | **`nil`**                          | The name and optionally the port number of a proxying web server through which this request should be made. The form is of the argument is `"www.machine.com"` or `"www.machine.com:8000"` if the web server is listening on port 8000 rather than 80. Proxying web servers are often used when clients are behind firewalls that prevent direct access to the internet. Another use is to centralize the page cache for a group of clients.                                                                                                                                                                                                                                                                                                                 |
| **`no-proxy`**                                                                                                             | **`nil`**                          | A list of strings naming the host names that will be directly accessed rather than going through a proxy. An entry in this list of **`"foo.com"`** will match any host name with **`"foo.com"`** as a suffix, such as **`"www.foo.com"`**. If the value of no-proxy is a string then it is treated as if it were a list of that single string.                                                                                                                                                                                                                                                                                                                                                                                                               |
| **<code>proxy&#8209;basic&#8209;authorization</code>**                                                                     | **`nil`**                          | A cons of **`("name" . "password")`** used to authenticate this request to the proxy being used.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| **`user-agent`**                                                                                                           | **`nil`**                          | If given it specifies the value of the User-Agent header to be sent with the request. Some sites respond differently based on the user-agent they believe has made the request. The lack of a User-Agent header may cause a server to ignore a request since it believes that it is being probed by a robot. The value of user-agent can be a string or one of the keywords **`:aserve`**, **`:netscape`** or **`:ie`** in which case an appropriate user agent string is sent.                                                                                                                                                                                                                                                                              |
| **`external-format`**                                                                                                      | see description                    | This determines the socket stream's external format. Default is the value of **<code>\*default&#8209;aserve&#8209;external&#8209;format\*</code>**.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| **`keep-alive`**                                                                                                           | **`nil`**                          | If true then the client will request that the server keep alive the connection. If the server agrees then that socket connection is returned as the fifth value.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| **`connection`**                                                                                                           | **`nil`**                          | If non nil then this is a socket connected to the server for which this request is made. If the socket is not valid (likely due to the server closing its end) then a new socket will be created. Thus it is not an error.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| **`ssl`**                                                                                                                  | **`nil`**                          | If true then the connection is made using the Secure Sockets Layer protocol. If the uri uses the **`https`** scheme then **`ssl`** is assumed to be true and the **`ssl`** argument need not be specified. NOTE: if the underlying Lisp implementation supports it, Service Name Indication (SNI) will automatically be be used. [**`do-http-request`**](aserve.md#f-do-http-request) will use the **`:server-name`** keyword to [**`acl-socket:make-ssl-client-stream`**]("https://franz.com/support/documentation/current/doc/operators/socket/make-ssl-client-stream.htm"), to indicate the hostname the client is attempting to connect to. This information is passed to the server to allow it to select the SSL certificate to present to the client. |
| **`ssl-method`**                                                                                                           | **`nil`**                          | see [SSL/TLS](#ssltls) for the use of this argument.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| **`skip-body`**                                                                                                            | **`nil`**                          | If the value is a function (satisifies **`functionp`**) then the value is funcalled passing the [**`client-request`**](aserve.md#c-client-request) object as an argument. At this point the client-request object contains the information on the headers of the response. The function should return true if the body of the response should be skipped and **`nil`** returned as the first value from do-http-request. If skip-body is not a function and if its value is true then reading the body is skipped and **`nil`** returned in its place.                                                                                                                                                                                                       |
| **`timeout`**                                                                                                              | **`nil`**                          | If given this is the number of seconds this function will block waiting to connect to the server and also to write or read to the socket connected to the web server. If an I/O request blocks for more than timeout seconds an error of class **`socket-error`** is signalled and the function **`stream-error-identifier`** on the error condition object will return **`:read-timeout`** or **`:write-timeout`**.                                                                                                                                                                                                                                                                                                                                         |
| **`compress`**                                                                                                             | **`nil`**                          | If true then tell the server in the request that we are able to accept a compressed body. If the server decides to send a compressed body then the body will be uncompressed by do-http-request before being returned to the caller.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| **`certificate`**, **`key`**, **`certificate-password`**, **`ca-file`**, **`ca-directory`**, **`verify`**, **`max-depth`** | **`nil`**                          | These values are passed as the arguments to make-ssl-client-stream (documented in the ACL documentation). Specifying these values is optional but it does give you control of the client's SSL certificate management. These values are used in a fully patched ACL 8.0 (or newer). In older versions of ACL they are ignored.                                                                                                                                                                                                                                                                                                                                                                                                                               |
| **`cache`**                                                                                                                | **`nil`**                          | A instance of class **`net.aserve.client:client-cache`** if you wish to consult and update this cache with this request. See the documentation on [**`client-cache`**](aserve.md#c-client-cache) below.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |


For example:

```lisp
user(5): (do-http-request "https://franz.com")

"<HTML>
    <HEAD>
        <TITLE>Franz Inc: Allegro Common Lisp and Common Lisp Products</TITLE>
        <BASE FONTFACE=\"helvetica, arial\" FONTSIZE=\"1\">
.....

"
200
(("content-type" . "text/html") ("transfer-encoding" . "chunked")
("server" . "Apache/1.3.9 (Unix) PHP/3.0.14")
("date" . "Mon, 24 Apr 2000 11:00:51 GMT"))
```


It's easy to use [**`do-http-request`**](aserve.md#f-do-http-request) to fill in form objects on a page. If the
form has input elements named **`width`** and **`height`** then you can send a request
that specifies that information in this way:

```lisp
(do-http-request "http://www.foo.com/myform.html"
                 :query '(("width" . 23) ("height" . 45)))
```

The above assumes that the method on the form is GET. If the method is POST
then a similar call will work:

```lisp
(do-http-request "http://www.foo.com/myform.html"  :method :post
                 :query '(("width" . 23) ("height" . 45)))
```

If the page uses digest authorization then you would call with arguments like

```lisp
(do-http-request "http://www.secret.com/whatever.html"
      :digest-authorization (make-instance 'digest-authorization
                                           :username "joe" :password "secret"))
```

-----

## <span id="c-computed-content"></span>
#### **`net.aserve.client:computed-content`** \[class\]

[**`computed-content`**](aserve.md#c-computed-content) is a class that is used to allow one to send data
to a webserver when doing a PUT or POST operation without having to
collect that data first in an object in Lisp memory.
[**`computed-content`**](aserve.md#c-computed-content) should be subclassed and these two methods written
over it:

```lisp
(get-content-length (cc my-computed-content-subclass))
```

returns the number of bytes in the content.

```lisp
(write-content (cc my-computed-content-subclass) stream)
```

writes the bytes of the computed content to the stream.

```lisp
(get-content-headers (cc my-computed-content-subclass) &key protocol headers)
```

is an optional method (default implementation returns `nil`, effectively doing
nothing) that allows the programmer to specify additional headers for a given
content type as well as check for protocol/header conflicts and signal an error
if that is the case.

-----

## <span id="c-file-computed-content"></span>
#### **`net.aserve.client:file-computed-content`** \[class\]

We have a subclass of [**`computed-content`**](aserve.md#c-computed-content) called [**`file-computed-content`**](aserve.md#c-file-computed-content).
Instances of [**`file-computed-content`**](aserve.md#c-file-computed-content) will send the contents of a file without
first reading all of the file into memory at once. In order to create an
instance of [**`file-computed-content`**](aserve.md#c-file-computed-content) you do:

```lisp
(make-instance 'net.aserve.client:file-computed-content :filename "/a/b/myfile.txt")
```

If the **`:content`** argument to [**`do-http-request`**](aserve.md#f-do-http-request) or
[**`make-http-client-request`**](aserve.md#f-make-http-client-request) is a `pathname`, it is implicitly converted into
an instance of [**`file-computed-content`**](aserve.md#c-file-computed-content).

-----

## <span id="c-stream-computed-content"></span>
#### **`net.aserve.client:stream-computed-content`** \[class\]

Another handy subclass of [**`computed-content`**](aserve.md#c-computed-content) is [**`stream-computed-content`**](aserve.md#c-stream-computed-content).
Instances of [**`stream-computed-content`**](aserve.md#c-stream-computed-content) will send the data read from inner stream,
using the chunking mechanism. Required `"Transfer-Encoding"` header is automatically
set via specialized **`get-content-headers`** method. In order to create an instance
of [**`stream-computed-content`**](aserve.md#c-stream-computed-content) you do:

```lisp
(make-instance 'net.aserve.client:stream-computed-content :stream input-stream)
```

If the **`:content`** argument to [**`do-http-request`**](aserve.md#f-do-http-request) or
[**`make-http-client-request`**](aserve.md#f-make-http-client-request) is a `stream`, it is implicitly converted into an
instance of [**`stream-computed-content`**](aserve.md#c-stream-computed-content).

Before we describe the lower level client request functions we will describe two
classes of objects used in that interface.

-----

## <span id="c-client-cache"></span>
#### **`net.aserve.client:client-cache`** \[class\]

The [**`do-http-request`**](aserve.md#f-do-http-request) function will cache responses if a [**`client-cache`**](aserve.md#c-client-cache) object
is passed as the value of the **`:cache`** argument.

Only **`:get`** and **`:head`** requests meeting certain criteria are cacheable: The
request cannot have a query, basic or digest authentication, or new headers
passed in.

You can specify the maximum size of the data in the cache by passing the
**`:max-cache-size`** argument to make-instance of client-cache e.g.

```lisp
(make-instance 'client-cache :max-cache-size 1000000)
```

When the number of bytes stored in the cache exceeds the **`max-cache-size`** plus
**`*cache-size-slop*`** (default 100000) entries in the cache are removed so that
the size is less than the maximum size minus **`*cache-size-slop*`**. The least
recently used cache entries are removed first.

The caching code will strictly follow the HTTP caching specification but you may
wish to do more caching than would be permitted by the specification. You may
know for a fact that a certain url's response can be cached for a certain period
of time despite the returned headers from that cache not indicating that
fact. There are two additional initargs to make-instance of client-cache you can
specify **`:auto-cache-codes`** **`:auto-cache-seconds`**

If an HTTP response is returned with no caching headers and if the response code
is one of the **`auto-cache-codes`** list then the response will be cached with an
expiration date **`auto-cache-seconds`** in the future.

*Both **`auto-cache-codes`** and **`auto-cache-seconds`** must be given or auto caching
will not occur.*

Accessors for the [**`client-cache`**](aserve.md#c-client-cache) object (in the **`net.aserve.client`** package):
  - **`client-cache-max-cache-entry-size`** - if a response to be cached exceeds
    this number of bytes the response will not be cached (default 1,000,000
    bytes). 
  - **`client-cache-max-cache-size`** - when the cache size exceeds this value plus
    **`*cache-size-slop*`** the cache is automatically reduced in size.
  - **`client-cache-lookups`** - the number of times the cache was checked to see if
    it contained the uri passed to do-http-request.
  - **`client-cache-alive`** - the number of times the cache contains the desired
    response and the cache entry hadn't expired. In this case the response is
    returned from the cache.
  - **`client-cache-revalidate`** - the number of times the cache contains the
    desired uri but the cache entry had expired so the cache entry must be
    revalidated.
  - **`client-cache-validated`** - the number of times the cache contains the
    desired uri but the cache entry had expired so it was revalidated and found
    to be still valid.

Associated function:

```lisp
(net.aserve.client:flush-client-cache cache &key expired all)
```

Specify which entries in the cache should be removed immediately. Passing
**`:all`** a true value will remove all entries. Passing **`:expired`** a true value
will remove entries whose expiration date is in the past. Note that just
because a cache entry has expired it doesn't mean it's worthless. When an
expired entry is encountered AllegroServe will do a revalidation request which
may return an indication that the entry is still valid thus saving the cost of
returning the body of the request again. Also the cache entry will get a new
expiration date.

Associated variable:

```lisp
net.aserve.client:*cache-size-slop*
```

Specify how many more bytes beyond the **`(client-cache-max-cache-size
cache)`** value are permitted before the cache size is reduced to
**`*cache-size-slop*`** less than the **`(client-cache-max-cache-size cache)`**
value.

-----

## <span id="c-client-request"></span>
#### **`net.aserve.client:client-request`** \[class\]

A [**`client-request`**](aserve.md#c-client-request) object includes the information about the request and the
response.

The public fields of a [**`client-request`**](aserve.md#c-client-request) that are filled in after a call to
[**`make-http-client-request`**](aserve.md#f-make-http-client-request) are:

| Accessor                     | Description                                                 |
|------------------------------|-------------------------------------------------------------|
| **`client-request-uri`**     | uri object corresponding to this request                    |
| **`client-request-socket`**  | socket object open to the web server denoted by the uri     |
| **`client-request-cookies`** | the cookie-jar object (if any) passed in with this request. |



After [**`read-client-response-headers`**](aserve.md#f-read-client-response) is called, the following fields of the
[**`client-request`**](aserve.md#c-client-request) objects are set:

| Accessor                                                           | Description                                                                                                                                                                                                                             |
|--------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **<code>client&#8209;request&#8209;response&#8209;code</code>**    | the integer that is the response code for this request. The most common codes are 200 for Success and 404 for Not Found.                                                                                                                |
| **<code>client&#8209;request&#8209;headers</code>**                | an alist of header values in the response. Each entry is a cons of the form **`("header-name" . "header-value")`** or **`(:header-name . "header-value")`** for well known slots. The header names, if strings,  are in all lower case. |
| **<code>client&#8209;request&#8209;protocol</code>**               | A keyword symbol naming the protocol  that the web server returned (which may be different that the protocol given in the request). A typical return value is **`:http/1.1`**                                                           |
| **<code>client&#8209;request&#8209;response&#8209;comment</code>** | A string giving a textual version of the response code. The string is arbitrary and you should not depend on all web servers returning the same string for any given response code.                                                     |

-----

## <span id="c-cookie-jar"></span>
#### **`net.aserve.client:cookie-jar`** \[class\]

A [**`cookie-jar`**](aserve.md#c-cookie-jar) is a respository for cookies. Cookies are stored in a jar when a
response from a client request includes Set-Cookie headers. Cookies from a jar
are sent along with a request when they are applicable to the given request. We
won't describe the rules for cookie applicability here, you need only know that
if you use our client functions to access a site that uses cookies to implement
persistence, then you should create a [**`cookie-jar`**](aserve.md#c-cookie-jar) object and pass that same
object in with each request. More information on cookies can be found
[here](http://developer.netscape.com:80/docs/manuals/js/client/jsref/cookies.htm).

A [**`cookie-jar`**](aserve.md#c-cookie-jar) is created with **`(make-instance 'cookie-jar)`**.

```lisp
(cookie-jar-items  cookie-jar)
```

returns an alist of the cookies in the jar where each item has the form:

```lisp
(hostname cookie-item ...)
```

The **`hostname`** is a string which is matched against the suffix of the name of
the host in the request (that is, a hostname of `".foo.com"` matches
`"a.foo.com"` and `"b.foo.com"`. ). The hostname should have at least two
periods in it. The following **`cookie-item`** objects in the list all apply to
that hostname. A **`cookie-item`** is a defstruct object and has these fields

| Accessor                                                 | Description                                                                                                    |
|----------------------------------------------------------|----------------------------------------------------------------------------------------------------------------|
| **<code>cookie&#8209;item&#8209;path</code>**            | A string that must be the prefix of the path of the request for it to match. The prefix "/" matches all paths. |
| **<code>cookie&#8209;item&#8209;name</code>**            | The name of the cookie. A string.                                                                              |
| **<code>cookie&#8209;item&#8209;value</code>**           | The value of the cookie. A string.                                                                             |
| **<code>cookie&#8209;item&#8209;expires</code>**         | A string holding the time the cookie expires \[in a future release we may make this a universal time\]         |
| **<code>cookie&#8209;item&#8209;secure</code>**          | true if this cookie should only be sent over a secure connection.                                              |
| **<code>cookie&#8209;item&#8209;http&#8209;only</code>** | true if this cookie should only be sent with the HttpOnly flag.                                                |

-----

<span id="f-make-http-client-request"></span>
#### **`net.aserve.client:make-http-client-request`**

```lisp
(make-http-client-request uri &key method protocol keep-alive
                                   accept cookies headers proxy
                                   proxy-basic-authorization
                                   basic-authorization
                                   digest-authorization query
                                   content content-type content-length
                                   user-agent external-format ssl
                                   timeout)
```

This function connects to the web server indicated by the **`uri`** and sends the
request. The arguments are the same as those for [**`do-http-request`**](aserve.md#f-do-http-request) and are
documented there. There is one additional argument: **`content-length`**. This
argument can be used to set the **`content-length`** header value in the
request. After setting the content-length the caller of
[**`make-http-client-request`**](aserve.md#f-make-http-client-request) would then be responsible for sending that many bytes
of data to the socket to serve as the body of the request. If **`content-length`**
is given, then a value for **`content`** should not be given.

If [**`make-http-client-request`**](aserve.md#f-make-http-client-request) succeeds in contacting the web server and sending
a request, a [**`client-request`**](aserve.md#c-client-request) object is returned. If [**`make-http-client-request`**](aserve.md#f-make-http-client-request)
fails, then an error is signalled.

The returned [**`client-request`**](aserve.md#c-client-request) object contains an open socket to a web server,
thus you must ensure that client-request object isn't discarded before
[**`client-request-close`**](aserve.md#f-client-request-read-close) is called on it to close the socket and reclaim that
resource.

After calling [**`make-http-client-request`**](aserve.md#f-make-http-client-request) the program will send the body of the
request (if any), and then it will call [**`read-client-response-headers`**](aserve.md#f-read-client-response) to
partially read the web server's response to the request.

The default value for **`external-format`** is the value of
[**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format)

-----

<span id="f-read-client-response"></span>
#### **`net.aserve.client:read-client-response-headers`**

```lisp
(read-client-response-headers client-request)
```

This function reads the response code and response headers from the web server.
After the function returns the program can use the [**`client-request`**](aserve.md#c-client-request) accessors
noted above to read the web server's response. The body of the response (if any)
has not been read at this point. You should use [**`client-request-read-sequence`**](aserve.md#f-client-request-read-sequence)
to read the body of the response part by part, or
[**`read-response-body`**](aserve.md#f-read-response-body) to read it as a whole.

-----

<span id="f-client-request-read-sequence"></span>
#### **`net.aserve.client:client-request-read-sequence`**

```lisp
(client-request-read-sequence buffer client-request &key start end)
```

This fills the **`buffer`** with the body of the response from the web server. The
buffer should either be a character array or an array of **`(unsigned-byte 8)`**.
If given, [**`start`**](aserve.md#f-start) specifies the index of the *first* element in the buffer in
which to store, and **`end`** is one plus the index of the *last* element in which
to store.

The return value is one plus the last index in the buffer filled by this
function. The caller of the function must be prepared for having the buffer only
partially filled. If the return value is zero then it indicates an End of File
condition.

-----

<span id="f-read-response-body"></span>
#### **`net.aserve.client:read-response-body`**

```lisp
(read-response-body client-request &key format)
```

Should only be called after
[**`read-client-response-headers`**](aserve.md#f-read-client-response) has been called. Reads
the whole response body (if any) and returns it. If the **`format`** parameter has a
value of **`:text`** (the default) the body is returned as a string, if given
**`:binary`**, it will be an array of **`(unsigned-byte 8)`** elements. When reading
text, the external format given when creating the request is used to decode
characters.

-----

<span id="f-client-request-read-close"></span>
#### **`net.aserve.client:client-request-close`**

```lisp
(client-request-close client-request)
```

The client-request object returned by [**`make-http-client-request`**](aserve.md#f-make-http-client-request) is closed.
This returns the resources used by this connection to the operating system.

-----

<span id="f-compute-digest-authorization"></span>
#### **`net.aserve.client:compute-digest-authorization`**

```lisp
(compute-digest-authorization client-request digest-authorization)
```

Given a [**`client-request`**](aserve.md#c-client-request) object after [**`read-client-response-headers`**](aserve.md#f-read-client-response) has been
called on it, determine if digest authorization is being requested and if so
compute the authorization values based on the username and password in the
digest-authorization object. Returns true if Digest Authorization was requested
by the server and if the authorization values were successfully computed.

Digest authorization is more difficult since the authorization key passed by the
client is based on values passed by the server when it rejected the request.
Thus you need to make the same request twice. The [**`do-http-request`**](aserve.md#f-do-http-request) function
handles this retry automatically. If you need to use
[**`client-request-read-sequence`**](aserve.md#f-client-request-read-sequence) to read the data then you will have to do the
retry as well. This function demonstrates how you use the above functions to do
the retry and then to retrieve the data:

```lisp
(defun getpage ()
  ;;
  ;; demonstrate how to add digest authorization to the
  ;; page retrieval loop that uses client-request-read-sequence
  ;;
  (let ((creq (make-http-client-request "http://www.secret.com/whatever.html")))
    (read-client-response-headers creq)
    (if* (eq 401 (client-request-response-code creq))
       then ; try digest authorization
            (let ((da (make-instance 'digest-authorization
                                     :username "joe"
                                     :password "secret")))
              (if* (compute-digest-authorization creq da)
                 then ;; successfully computed digest authorization
                      ;; values
                      ;; end request
                      (client-request-close creq)
                      ;; and create a new one
                      (setq creq (make-http-client-request
                                  "http://www.secret.com/whatever.html"
                                  :digest-authorization da))
                      (read-client-response-headers creq))))

    (if* (not (eql 200 (client-request-response-code creq)))
       then (error "get failed with code ~s"
                   (client-request-response-code creq)))

    (let ((buffer (make-array 2048 :element-type 'character)))
      (loop
         (let ((length (client-request-read-sequence buffer creq)))
           (if* (zerop length)
              then (return))

           (format t "got buffer of length ~s:~%~s~%" length
                   (subseq buffer 0 length))))

      (client-request-close creq))))
```

-----

<span id="f-uriencode-string"></span>
#### **`net.aserve:uriencode-string`**

```lisp
(uriencode-string string &key external-format)
```

Convert the string into a format that would be safe to use as a component of a
url. In this conversion most printing characters are not changed All non
printing characters and printing characters that could be confused with
characters that separate fields in a url are encoded a %xy where xy is the
hexadecimal representation of the char-code of the character. external-format
defaults to the value of [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format).

## <span id="proxy"></span>Proxy

AllegroServe can serve as an http proxy. What this means is that web clients
can ask AllegroServe to fetch a URL for them. The two primary uses for a proxy
server are (but see the warning below):

  1. You have web clients on a local network and you would prefer that the web
     clients don't send messages out to the internet. You run AllegroServe on a
     machine that has access both to the internal network and to the
     internet. You then configure the web clients to proxy through AllegroServe
     (directions for doing this are given below).
  2. You wish to use AllegroServe's caching facility to store copies of pages
     locally to improve responsiveness. In this case you must start AllegroServe
     as a proxy server for the web clients who will use the cache.

In order to run AllegroServe as a proxy server you should specify **`:proxy t`** in
the arguments to the **`net.aserve:start`** function. With this specified
AllegroServe will still act as a web server for pages on the machine on which
AllegroServe is running. AllegroServe will act as a proxy for requests to other
machines.

Each web browser has it's own way of specifying which proxy server it should
use. For Netscape version 4 select the **Edit** menu, then **Preferences**, and
then click on the plus sign to the left of **Advanced**. Then select **Proxies**
and click on **Manual Proxy Configuration** and the click on **View** and
specify the name of the machine running AllegroServe and the port number on
which AllegroServe is listening. Then click **OK** on all the dialog boxes.

For Internet Explorer 5 select the **Tools** menu, and then **Internet Options**
and then the **Connections** tab, and then **LAN Settings**. Click on **Use a
Proxy Server** and then click on **Advanced** and specify the machine name and
port number for AllegroServe. Then click on **OK** to dismiss the dialog
windows.

> *Warning*: if you specify **`:proxy t`** to **`net.aserve:start`**, AllegroServe will
> proxy any http request on behalf of any browser that can access the
> server. This is known as an *open proxy* and this is a service which might be
> abused if accessible to strangers. This danger can be mitigated with
> proxy-control objects, described next.

### [**`proxy-control`**](aserve.md#c-proxy-control) objects

-----

<span id="c-proxy-control"></span>
#### **`net.aserve:proxy-control`** \[class\]

In order to control who gets to use the proxy and what sites AllegroServe will
proxy, you can pass a [**`proxy-control`**](aserve.md#c-proxy-control) object as the value of the **`:proxy`**
argument instead of **`t`**.

The proxy-control object has two fields:

  1. **`location`** - controls which machines on the internet or the local net can
     use the proxy
  2. **`destinations`** - controls which sites AllegroServe will contact as a proxy.

Values for either or both of the fields can be specified. An unspecified field
means "allow everything".

**`location`**: The location slot is set with the :location initarg or with the
**`proxy-control-location`** accessor. The value of the location field is nil or a
[location-authorizer](#c-location-authorizer) object described in detail in
another part of this document.

**`destinations`**: this slot is set with the **`:destinations`** initarg or the
**`proxy-control-destinations`** accessor. The value can be:

  1. **`nil`**, which means all destinations are allowed. This the default.

  2. A list of strings or lists. Strings should be the names of the host to
     which proxying is allowed. It will be compared in a case insensitive way to
     the host part of the URI in the proxy request. Just specifying a hostname
     allows proxying to a server running on any port of that host.

     Lists should be of the form **`(hostname port1 port2 ..)`** where the portN are
     integers. This allows access to servers running on only those ports of the
     host. Example: **`("internal.franz.com" 80 8080)`** allows proxying to ports 80
     and 8080 on `internal.franz.com`. If no ports are specified in the list
     format, e.g. **`("www.franz.com")`**, then no ports are opened and the effect
     is the same (when **`destinations`** is non-nil) as not listing that hostname
     at all.

  3. An equalp hash table where the keys are the hostnames and the values have
     the form (1) **`t`** (meaning all ports are proxyable) or (2) a list of
     integers (meaning that only proxying to those ports is allowed).

Proxy requests which do not specify any ports, e.g.
**`("http://internal.franz.com/blah/blah")`**, are interpreted as requesting port
80.

The host specified in a proxy request must match exactly (except that case is
ignored). Thus specifying `"franz.com"` will only match requests for `"franz.com"`
and not `"www.franz.com"`.

Example:

```lisp
(net.aserve:start
 :proxy (make-instance 'proxy-control
          :location (make-instance 'location-authorizer
                         :patterns '((:accept "127.1")
                                     (:accept "192.168.1.0" 24)
                                     :deny))
          :destinations '("www.cnn.com" "edition.cnn.com" "svcs.cnn.com" "ads.cnn.com")))
```

The proxy will accept connections from localhost (which is always IP address
`127.1`) and it will accept any IP in the range `192.168.1.0` to `192.168.1.255`
which is presumably the local network on which the machine is located.

The proxy will only accept proxy requests that begin `"http://www.cnn.com/"`,
`"http://edition.cnn.com/"`, etc.

If the list of allowed sites to proxy is large it's best to use a hash table to
store them:

```lisp
(setq ht (make-hash-table :test #'equalp))

(dolist (site '("www.cnn.com" "edition.cnn.com" "svcs.cnn.com" "ads.cnn.com"
                ("i.cdn.turner.com" 80 8000 9300)
                "i2.cdn.turner.com"))
  (setf (gethash (if* (consp site) then (car site) else site)  ht)
    (if* (consp site) then (cdr site) else t)))

;; followed by

(make-instance 'proxy-control :destinations ht)
```

You can alter the proxy-control object while AllegoServe is running in order to
change the authorization of the proxy.

-----

<span id="f-authorize-proxy-request"></span>
#### **`net.aserve:authorize-proxy-request`**

```lisp
(authorize-proxy-request ((req http-request) (ent entity) (pc proxy-contol))
(authorize-proxy-request ((req http-request) (ent entity) (pc (eql nil)))
```

For each HTTP request that has the form of a proxy request the generic function
[**`authorize-proxy-request`**](aserve.md#f-authorize-proxy-request) is called, passing the request and entity objects and
the value of **`(wserver-proxy-control wserver)`**.

**`(wserver-proxy-control wserver)`** is set to the value of the **`:proxy`** argument
to **`net.aserve:start`** if that value is of type proxy-control, otherwise it is
set to **`nil`**. There are two primary methods, one for **`pc`** being a proxy-control
and one for **`pc`** being nil. TThe first uses the location and description slots
of the proxy-control object as described above to return true if proxying is
authorized.

The second simply returns true since without a proxy-control object all proxying
is permitted.

-----

## <span id="cache"></span>Cache

The AllegroServe cache is a facilty in development. We'll describe here the
current status of the code.

The cache consists of a memory cache and a set of zero or more disk caches.
Items initially live in the memory cache and are moved to the disk caches when
the memory cache fills up. Items enter the memory cache due to a page being
accessed via the proxy server. Items in the disk cache move back to the memory
cache if the data portion must be sent back to the requesting client (some
requests can be answered without sending back the contents of the page and for
these the item stays in the disk cache).

You specify the sizes of each cache. The disk caches will never grow beyond the
size you specified but the memory cache can exceed the specified size for a
short time. A background thread moves items from the memory cache to the disk
caches and we will allow you to control how often that thread wakes up and
ensures that the memory cache is within the desired constraints.

When **`net.aserve:start`** is called you specify if you want caching and if so what
size caches you want. A sample argument pair passed to **`net.aserve:start`** is

```lisp
    :cache '(:memory 10000000 :disk ("/tmp/mycache" 30000000) :disk (nil 20000000))
```

This says that the memory cache should be 10,000,000 bytes and that there should
be two disk caches. One disk cache is the file `/tmp/mycache` and can grow to
30,000,000 bytes and the other cache will have a name chosen by AllegroServe and
it can grow to 20,000,000 bytes. We should note here that one thing that
distinguishes the AllegroServe caching facilty from that found in many other
http proxy-caches is that AllegroServe uses a few large cache files rather than
storing each cached item in a separate file in the filesystem.

A few other ways of specifying caching at startup is:

```lisp
    :cache t
```

This will create a memory case of the default size (currently 10 megabytes) and
it will create no disk caches.

```lisp
    :cache 20000000
```

This will create a memory cache of 20,000,000 byte and no disk caches.

When caching is enabled we publish two links to pages showing cache information.
This is useful during debugging and is likely to change in the future. The two
pages are `/cache-stats` and `/cache-entries`.

-----

## <span id="filters"></span>Request Filters

After AllegroServe reads a request and before it checks the locators to find an
entity to handle the request, AllegroServe runs the request through a set of
filters.

> A filter is a function of one argument: the http-request object. The filter
> examines and possibily alters the request object. The idea is that filters can
> do large scale and simple url rewriting, such as changing all requests for one
> machine to another machine. The filtering occurs before the test to see if
> this is a proxy request so a filter can change a proxy request to a non proxy
> request or vice versa.
>
> The currently active filters are found in two places. First the
> **`vhost-filters`** function of the applicable **`vhost`** returns a set of **`vhost`**
> specific filters. Next the **`wserver-filters`** function on the current
> **`wserver`** object returns a set of server global filters. Both of these
> functions are **`setf`**'able to change the set of filters.

A filter function returns **`:done`** if no more filters should be run after this
one. If the filter returns anything else then subsequent filters in the list are
run as well. If a filter in the **`vhost`** list returns **`:done`** then the server
global filters are not even checked.

When a filter function runs it's most likely going to be looking at two slots in
the request object, which are accessed via these functions:

  - **`request-raw-uri`** - the actual uri given in the http command;
  - [**`request-uri`**](aserve.md#f-request-uri) - a constructed uri starting with the raw uri and adding
    information from the Host header field. This value is used to find the
    entity to run thus it has all the information about the request.

Also the value of **`(header-slot-value request :host)`** is important to check and
possibly change.

If the browser is setup to access the internet directly then a request from the
user for `http://foo.bar.com:23/whatever` will cause the request to be sent to
the server at `foo.bar.com` port 23 and the request will have:

  1. The **`request-raw-uri`** is `"/whatever"`.
  2. The [**`request-uri`**](aserve.md#f-request-uri) is `"http://foo.bar.com:23/whatever"`.
  3. The Host header value is `"foo.bar.com:23"`.


If the browser is setup to send all requests through a proxy at `proxy.blop.com`
then a request for `http://foo.bar.com:23/whatever` will come to
`proxy.blop.com` and will have a different raw uri:

  1. The **`request-raw-uri`** is now `"http://foo.bar.com:23/whatever"`.
  2. The [**`request-uri`**](aserve.md#f-request-uri) is still `"http://foo.bar.com:23/whatever"`.
  3. The Host header value is still `"foo.bar.com:23"`.

If the filter wants to alter the destination of request it should ensure that
the three values mentioned above are set appropriately for the destination. If
the new destination is not served by the current Allegroserve wserver, then the
filter will have to make sure to turn it into a proxy request (and this will
only work if this AllegroServe was started with proxying enabled).

-----

## <span id="virtual_hosts"></span>Virtual Hosts

It is possible for a single web server to act like two or more indepenent web
servers. This is known as *virtual hosting*. AllegroServe supports the ability
to run any number of virtual hosts in a single instance of AllegroServe.

AllegroServe runs on a single machine and listens for requests on one port on
one or more more IP addresses. When a request arrives there is usually a header
line labelled Host whose value is the specific hostname typed into the browser
by the user. Thus if hostnames www.foo.com and www.bar.com both point to the
same machine then it's possible for the webserver on that machine to distinguish
a request for `http://www.foo.com` from a request for `http://www.bar.com` by
looking at the Host header.

In order to make AllegroServe easy to use you can ignore the virtual hosting
facility until you plan to use it. As long as you don't specify a **`:host`**
argument to any of the publish functions when adding content to your site,
everything you publish will be visible from your web server no matter which
hostname the web browser uses to access your site. If you decide you want to
make use of virtual hosting, then read on.

### **`vhost`** class

In AllegroServe a virtual host is denoted by an instance of class **`vhost`**. The
contents of a vhost object are:

| Accessor Function                               | What                                                                                                              | initarg                              |
|-------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|--------------------------------------|
| **`vhost-log-stream`**                          | Stream to which to write logging information on requests to this virtual host                                     | **`:log-stream`**                    |
| **<code>vhost&#8209;error&#8209;stream</code>** | Stream to which AllegroServe sends informational and error messages that are generated during request processing. | **<code>:error&#8209;stream</code>** |
| **`vhost-names`**                               | A list of all the names for this virtual host.                                                                    | **`:names`**                         |
| **`vhost-filters`**                             | list of [filter functions](#filters)                                                                              | **`:filters`**                       |


The defaults values for the two streams in a vhost object is the
**`wserver-log-stream`** from the server object.

Every instance of AllegroServe has a default vhost object that can be retrieved
from the **`wserver`** object via the function **`wserver-default-vhost`**. If a request
comes in for a virtual host that's not known, then it's assumed to be for the
default virtual host.

There are two ways to create virtual hosts in AllegroServe: implicitly or
explicitly. If a publish function is called with a **`:host`** value that names a
host not known to be a virtual host then a **`vhost`** instance will be created
automatically and stored in the **`wserver`**'s hash table that maps names to
**`vhost`** objects. This is implicit virtual host creation.

If you know ahead of time the virtual hosts you'll be serving then it's better
to setup all the virtual hosts explicitly. You create a **`vhost`** instance with
**`make-instance`** and you register each virtual host in the **`wserver-vhosts`** table
using **`gethash`**. Following is an example of setting up a server to have two
virtual hosts, one that responds to three names and one that responds to two
names. Since we are using the default vhost to represent the first virtual
host, this virtual host will also receive requests for names we haven't
mentioned explicitly.

```lisp
    (defun setup-virtual-hosts (server)
      (let ((vhost-table (wserver-vhosts server))
       (foo-names '("localhost" "www.foo.com" "foo.com"))
      (bar-names '("www.bar.com" "store.bar.com")))

        (let ((default-vhost (wserver-default-vhost server)))
          (setf (vhost-names default-vhost) foo-names)
          (dolist (name foo-names)
      (setf (gethash name vhost-table) default-vhost)))

        (let ((bar-vhost (make-instance 'vhost :names bar-names)))
          (dolist (name bar-names)
       (setf (gethash name vhost-table) bar-vhost)))))
```

When a request comes in, AllegroServe will determine which vhost is the intended
target and if none is found it will select the default vhost as the intended
target. The vhost so determined will be stored in the **`http-request`** object in
the slot accessed by **`request-vhost`** function.

### <span id="host_arg"></span>**`host`** argument to publish functions

We now are in a position to describe what values the **`:host`** argument to the
publish functions can take on. The **`:host`** argument can be **`nil`** or one of:

  1. A string naming a virtual host. If there is no virtual host with this name
     a new virtual host object is created.
  2. A vhost object.
  3. The symbol **`:wild`**.
  4. A list of items of the above items.

If the value of the **`:host`** argument is **`nil`**, then its value is assumed to be
**`:wild`**.

The value of the **`:host`** argument is converted into a list of one or more vhost
objects and/or the symbol **`:wild`**. The meaning of a vhost is clear: it means
that this entity will be visible on this virtual host. The meaning of **`:wild`**
is that this entity will be visible on *all* virtual hosts, except it can be
shadowed by an entity specified for a particular virtual host. Thus you could
publish an entity for **`:path "/"`** and **`:host :wild`** and it will be used for all
virtual hosts that don't specify an entity for **`:path "/"`**. Note that when a
request comes in and the search is done for an entity to match the request every
step of the way we look first for a vhost specific handler and then a **`:wild`**
handler It is *not* the case that we first do a complete search for a vhost
specific handler and then restart the search this time looking for a **`:wild`**
handler.

-----

## <span id="timeouts"></span>Timeouts

A web server is a program that provides resources to a client program connecting
over the network. The resources a web server has to offer is limited and it's
important that network problems or buggy clients don't cause those resources to
be unavailable to new clients. AllegroServe uses timeouts to ensure that no
client can hold a web server resource for more than a certain amount of time.

Three common ways for a resource to be held are

  1. A client stops sending a request in the middle of the request. This can
     happen if the client machine crashes or if the client's machine loses
     network connectivity with the machine running AllegroServe.
  2. A client stops reading the response to its request. The networking code
     will automatically stop the sender from writing new data if the receiver
     has a lot of existing data to read.
  3. The response function to an http request can take a very long time, or may
     even be in an infinite loop. This could be due to a bug in a http response
     function or something unexpected, like a database query taking a long time
     to finish.


### ACL 6.0 or older

For AllegroServe running in Acl 6.0 or *older* timeouts are done this way:

**`net.aserve::*read-request-timeout*`** - number of seconds AllegroServe allows for
the request line (the first line) and all following header lines. The default
is 20 seconds.

**`net.aserve::*read-request-body-timeout*`** - number of seconds AllegroServe
allows for the body of the request (if any) to be read. The default is 60
seconds.

**`(wserver-response-timeout wserver)`** - the number of seconds AllegroServe allows
for an http request function to be run and finished sending back its
response. The initial value for this slot of the wserver object is found in
[**`*http-response-timeout*`**](aserve.md#v-http-response-timeout) which defaults to 120 seconds. You can alter this
timeout value with the **`:timeout`** argument to [**`with-http-response`**](aserve.md#f-with-http-response) or by
specifying a **`:timeout`** when publishing the entity.

### ACL 6.1 or newer

In Acl 6.1 we added the capability of having each I/O operation to a socket
stream time out. This means that we don't have to predict how long it should
take to get a request or send a response. As long as we're making progress
reading or writing we know that the client on the other end of the network
connection is alive and well. We still need a timeout to handle case (3) above
but we can allow a lot more time for the http response since we aren't using
this timer to catch dead clients as well. Thus we have these timeout values:

<span id="f-wserver-io-timeout"></span>**`(wserver-io-timeout wserver)`** - the
number of seconds that AllegroServe will wait for any read or write operation to
the socket to finish. The value is initialized to the value of
**`*http-io-timeout`** which defaults to 60 seconds.

<span id="f-wserver-response-timeout"></span>**`(wserver-response-timeout
wserver)`** - the number of seconds AllegroServe allows for an http request
function to be run and finished sending back its response. The initial value for
this slot of the wserver object is found in [**`*http-response-timeout*`**](aserve.md#v-http-response-timeout) which
defaults to 300 seconds. You can alter this timeout value with the :timeout
argument to [**`with-http-response`**](aserve.md#f-with-http-response) or by specifying a **`:timeout`** argument to the
publish function creating the entity.

[**`publish-directory`**](aserve.md#f-publish-directory) and [**`publish-file`**](aserve.md#f-publish-file) default their **`timeout`** argument in a way
that makes sense based on whether the Lisp supports I/O timeouts. If I/O
timeouts are supported then there is no reason to do a global timeout for the
whole response if you're just sending back a file. Thus in this case the
**`timeout`** argument defaults to a huge number.

-----

## <span id="compression"></span>Compression

The HTTP protocol allows the client to request an entity be returned by the
server. The server can simply send that entity as is or the client and server
an agree that an encoded version of the entity should be transported. The server
does the encoding, the client does the decoding and the caller of the client
gets what it expects and is totally unaware that the entity was encoded while
being transported.

The encoding supported by AllegroServer is called gzip. Gzip is a compression
algorithm. A gziped text file can be substantially smaller than the original.
gzip will not shink (and it may even grow) files that already compressed such as
image files (jpeg, gif, png), movie files (mp4, mov, avi). or compressed archive
file (zip, tar.gz, tgz).

The are two types of compression handled by AllegroServe

  1. On the fly compression - when a computed entity is generated AllegroServe
     can compress the generated entity as it is being generated.
  2. Static file compression - if a web site contains files that might compress
     well the webmaster can arrange to compress those files and leave the
     compressed version of those files on the web site as well for AllegroServe
     to find.

One action that AllegroServe will never take is to compress a static file on the
fly.

In order for AllegroServe to encode an entity's body using gzip compression the
following must be true

  1. The server must be started with **`:compress t`**. Alternatively the
     enable-compression slot of the **`*wserver*`** object must be true. This slot
     is a server wide switch that controls whether compressed encodings are sent
     from the server.
  2. The client request must include a header `Accept-Encoding: gzip`.
  3. The entity being published must have be specified with **`:compress t`**.
  4. The zlib compression library must be on the server machine.

If the entity is computed then the above four conditions are enough to cause a
compressed result to be sent. If the entity is a file entity (perhaps created
due to a directory entity being searched) then you also need

  1. A compressed version of the file (the same file name but with ".gz"
     appended).
  2. The compressed version must be as young or younger than the original
     version (comparing last modified times).

The only way to tell if a compressed version was sent in place of the original
version is to note a smaller number of bytes transmitted as recorded in the log
file.

-----

## <span id="ssltls"></span>SSL/TLS

The SSL protocol used for secure communcation has gone through a sequence of
revisions. The public revisions are SSL v2.0, SSL v3.0 and SSL v3.1. There was a
renaming as well so SSL v3.1 is officially known as TLS v1.0.

When an SSL client connects to an SSL server they will communicate using the
most recent protocol that both support.

By default AllegroServe's SSL server and client declare that they are willing to
communicate using SSL v2.0, v3.0 or v3.1 (TLS v1.0). If you wish to restrict the
server or client to a particular protocol you can pass the **`:ssl-method`**
argument to **`net.aserve:start`** or **`net.aserve.client:do-http-request`**. At
present the only meaningful value you would want to pass is **`:tlsv1`** meaning
that you only want to communicate using TLS v1.0 (SSL v3.1), the most modern and
secure of the protocols.

-----

## <span id="miscellaneous"></span>Miscellaneous

<span id="f-ensure-stream-lock"></span>
#### **`net.aserve:ensure-stream-lock`**

```lisp
(ensure-stream-lock stream)
```

The function adds a [process
lock](https://franz.com/support/documentation/current/doc/multiprocessing.htm#process-locks-1)
to **`stream`**'s property list (under the indicator **`:lock`**) if no such lock is
present. Then it returns the object **`stream`**.

The AllegroServe logging functions make use of the stream's lock to ensure that
only one thread at a time write log information to the stream. If the logging
functions find that a a log stream doesn't have a lock associated with it then
the log information will still be written to the stream but under heavy load the
log information from multiple threads will become intermixed.

-----

<span id="f-map-entities"></span>
#### **`net.aserve:map-entities`**

```lisp
(map-entities function locator)
```

When one of the publish functions is called entities are placed in locator
objects. The locator objects are then checked when http requests come in to
find the appropriate entity. [**`map-entities`**](aserve.md#f-map-entities) will apply the given **`function`** of
one argument to all the entities in the given **`locator`**. One common use of
[**`map-entities`**](aserve.md#f-map-entities) is to find entities that you no longer wish to be published. For
that reason [**`map-entities`**](aserve.md#f-map-entities) will remove the entity passed to the **`function`** if
the **`function`** returns the keyword symbol **`:remove`** as its value.

-----

<span id="f-log-for-wserver"></span>
#### **`net.aserve:log-for-wserver`**

```lisp
(log-for-wserver wserver message format)
```

This is a method that is called by AllegroServe whenever it wants to log
something. The **`wserver`** argument can be specialized on for your own server
class, in order to make log messages go to your own log stream, formatted using
your own logging conventions. **`message`** will always be a string, and **`format`**
can be one of **`:long`** or **`:brief`**, which AllegroServe itself uses to distinguish
between messages that get a full date timestamp, and those that get only a
time. Your custom method can choose to ignore this if it doesn't support
multiple levels of verbosity.

-----

## <span id="asaservice"></span>Running AllegroServe as a Service on Windows NT

On Windows NT (and Windows 2000 and Windows XP) when you log off all the
programs you are running are terminated. If you want to run AllegroServe on
your machine after you log out you have to start it as a **`Windows Service`**.
This is easy to do thanks to code contributed by Ahmon Dancy.

The first step is to download the [ntservice code and
documentation](http://opensource.franz.com/ntservice) from the Franz [opensource
site](http://opensource.franz.com). Read the documentation carefully especially
as regards the different capabilities of the accounts under which you may choose
to run AllegroServe.

You'll probably want to build an AllegroServe application that can run either
normally or as a service,. You can run it normally to debug it and then start it
as a service when you're satisfied that it works.

Following is an example of how this can be done. I've decided that if the
`"/service"` argument is given on the command line when I start my application
then I'll start my application as a service, otherwise I start it normally.
Here is the **`restart-init-function`** (to **`generate-application`**) that I define:

```lisp
(defun start-aserve-application ()
  (flet ((start-application ()
      (net.aserve:start :port 8020)
       (loop (sleep 100000))))
    (if* (member "/service" (sys:command-line-arguments) :test #'equalp)
     then ; start as  a service
     (ntservice:start-service #'start-application)
     else ; start as a normal app
      (start-application)))))
```

I use **`(loop (sleep 100000))`** to ensure that the **`restart-init-function`** never
returns.

In order to register my application as a service to the operating system I call
**`ntservice:create-service`** like this:

```lisp
(ntservice:create-service "aservetest" "Aserve Test Service"
     "c:\\acl62\\testservice\\testapp\\testapp.exe -- /service")
```

Note that I use `"--"` before the `"/service"`. This is *very* important. The
`"--"` separates the arguments used to start up the program from the arguments
passed to the program itself. The call to **`ntservice:create-service`** is done
only once and need not be done from within your application.

Once an application is registered as a service you can start it by going to the
Control Panel, selecting Administrative Tools and then Services. Locate the
service you just added, right click on it and select [**`start`**](aserve.md#f-start). You can stop the
service with a right click as well.

-----

## <span id="international-chars-aserve"></span>Using International Characters in AllegroServe

A *character set* is a collection of characters and a rule to encode them as a
sequence of octets. The default character set for web protocols is Latin1 (also
known as ISO 8859-1). The Latin1 character set represents nearly every
character and punctuation needed for western European languages (which includes
English).

If you want to work with characters outside the Latin1 set you'll want to use
the [International version of Allegro
CL](https://franz.com/support/documentation/6.1/doc/iacl.htm) which represents
characters internally by their 16-bit [Unicode](http://www.unicode.org) value.
In this section we'll assume that you're using International Allegro CL.

What the web protocols refer to as **`charset`** (character set) Allegro CL refers
to as an **`external-format`**. Allegro CL uses a different term since it always
uses 16-bit Unicode to represent characters internally. 16 bit unicode can
represent nearly all characters on the planet. It's only when those characters
are read from or written to devices outside of Lisp that the actual encoding of
those characters into octets matters. Thus the **`external-format`** specifies how
characters are encoded and specifies which Unicode characters are part of the
character set that the external-format defines. Attempts to write a Unicode
character that's not part of the character set results in a question mark being
written.

External-formats are also used in Allegro CL to do certain character to
character transformations. In particular on the Windows platform external
formats are used to convert the lisp end of line (a single **`#\newline`**
character) to the **`#\return #\linefeed`** character that is standard on Windows.
Thus an external format such as **`:utf-8`** has a different effect on Windows than
on Unix, and this is not desirable for web applications. The function call
**`(crlf-base-ef :utf-8)`** returns an external format on Windows and on Unix that
simply does the character encoding part of the external format, and thus this is
the external format you would want to use in a web application.

### Server to client (browser) character transfer

When a web server returns a response to a client it sends back a response line,
a header and optionally a body. The response line and header are always sent
using a subset of the Latin1 character set (the subset corresponding the US
ASCII character set). The body is sent using the full Latin1 character set,
unless otherwise specified. To specify the character set of the body you add an
extra parameter to the Content-Type header. Instead of specifying a content
type of `"text/html"` you might specify `"text/html; charset=iso-8859-2"`. This
alerts the http client that it must interpret the octets comprising the body of
the response according to the iso-8859-2 character set. This however is *not*
enough to make AllegroServe encode the Unicode characters it's sending to the
client using the approrpriate external format. You would have to do this:

```lisp
(with-http-response (req ent)
  (with-http-body (req ent :external-format (crlf-base-ef :iso8859-2))
     ... generate and write page here..
    ))
```

Note that the charset names and external format name are similar but not
identical. Check [here](http://www.iana.org/assignments/character-sets) for the
charset names and check
[here](https://franz.com/support/documentation/6.1/doc/iacl.htm#external-formats-1)
for the Allegro CL external format names.

In order to make it easier to specify external formats in AllegroServe you can
specify a default external format when you start the server (with the
**`:external-format`** argument to the [**`start`**](aserve.md#f-start) function). The variable
[**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format) will then be bound to this external format in
each of the threads that processes http requests. It's the value of
[**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format) that is used as the default argument to the
:external-format argument to [**`with-http-body`**](aserve.md#f-with-http-body).

The default value of the **`:external-format`** argument to the [**`start`**](aserve.md#f-start) function,
and thus the default value of [**`*default-aserve-external-format*`**](aserve.md#v-default-aserve-external-format), is
**`(crlf-base-ef :latin1-base)`**. This means that regardless of the locale in
which you run AllegroServe, AllegroServe will by default use the Latin1
character set, which is what is expected by web clients..

A very useful character set is **`utf-8`** which is the whole Unicode character set
and thus comprises all of the characters you can store inside Lisp. The
corresponding Allegro CL external format is the value of **`(crlf-base-ef
:utf-8)`**. Specifying this character set allows you to write web pages that can
use characters from nearly every language in the world (whether the web browser
can find the glyphs to display all those characters is another matter).

### Client (browser) to server character transfer

The brower sends characters to the web server when the user enters data into a
form and submits the form. The important thing to remember is that the browser
will encode characters using the character set that was specified for the web
page containing the form. If you fail to specify a **`charset`** when the page was
given to the web browser then the web browser will decide on its own how to
encode characters that aren't part of the default character set (which is of
course Latin1). The browser will *not* tell you which encoding it chose.
Therefore if you ever plan on allowing non-Latin1 characters to be specified in
your forms you'll want to specify a **`charset`** for the page containing the form.

You can specify the charset in the Content-Type field of the header that's sent
with the page (as we described above) or you can put it in the page itself using
a meta tag:

```http
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
```

Retrieving form data in AllegroServe is done with the [**`request-query`**](aserve.md#f-request-query) function
and that function takes an **`:external-format`** argument so you can specify how
the form data can be decoded. If your form sends multipart data then you can
use the **`:external-format`** argument to [**`get-multipart-sequence`**](aserve.md#f-get-multipart-sequence) to retrieve the
form data and decode the data.

### Examples

The AllegroServe test page has links to a few pages that show how international
characters work with AllegroServe. One of these is the International Character
Display page. This page shows what happens when the **`charset`** and
**`external-format`** are set to different values and a page containing
international characters is displayed. It demonstrates how it important it is
that those two character set specifications be kept in sync, and it shows that
`utf-8` is most likely the best choice for a character set for your web pages.

-----

## <span id="debugging"></span>Debugging

Debugging entity handler functions is difficult since these are usually run on a
separate lisp thread. Also AllegroServe catches errors in entity handler
functions, thus preventing you from interactively diagnosing the problem.

You can put AllegroServe in a mode that makes debugging easier with the
[**`net.aserve::debug-on`**](aserve.md#f-debug-on) function. Note that this is not an exported function to
emphasize the fact that you are working with the internals of AllegroServe.

-----

<span id="f-debug-on"></span>
#### [**`net.aserve::debug-on`**](aserve.md#f-debug-on)

```lisp
(net.aserve::debug-on &rest debugging-features-to-enable)
```

We've classified the debugging features and given each a keyword symbol name.
This function turns on those named features. If no arguments are given, then
**`debug-on`** prints the list of debugging features and whether each is enabled.

-----

<span id="f-debug-off"></span>
#### [**`net.aserve::debug-off`**](aserve.md#f-debug-off)

```lisp
(net.aserve::debug-off &rest debugging-features-to-disable)
```

This function turns off the given list of features.

The list of debug features are given below. We flag three of particular
interest:

| Name          | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|---------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`:info`**   | AllegroServe prints information at certain places while doing its processing.                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| **`:xmit`**   | AllegroServe prints what it receives from and sends to the client. In some cases the body of a request or response will not be printed.                                                                                                                                                                                                                                                                                                                                                                                                              |
| **`:notrap`** | When enabled, this prevents AllegroServe from catching errors in entity handler functions. If an error occurs and you're running in an evironment where background processes automatically create new windows (such as the emacs-lisp interface) then you'll be given a chance to :zoom the stack and diagnose the problem. Note that if a timeout has been established to limit the amount of time that a certain step is allowed (and this is done by default) then the interactive debugging session will be aborted when the timeout is reached. |

Here are all features showing their parent features. Turning on a parent feature
enables the child features. Some features have multiple parents. Note there are
no **`:xmit-proxy-server-request-*`** categories, because at the time of reading the
request it's not yet known whether it's the going to be proxied so these show up
as **`:xmit-server-request-*`**.

```text
    :all
        The parent of all debug features.
    :notrap
        If set than errors in handlers cause a break loop to be entered.
        (parent categories: :all)
    :zoom-on-error
        If set then print a zoom to the vhost-error-stream when an error occurs in a handler.
        (parent categories: :all)
    :log
        Category of features that write some kind of log.
        (parent categories: :all)
    :xmit
        Category of features that log the traffic between clients, servers.
        (parent categories: :log)
    :info
        General information.
        (parent categories: :log)
    :client
        Category of features that log client communication.
        (parent categories: :all)
    :server
        Category of features that log server communication.
        (parent categories: :all)
    :proxy
        Category of features that log proxy communication.
        (parent categories: :all)
    :request
        Category of features that log requests.
        (parent categories: :all)
    :response
        Category of features that log responses.
        (parent categories: :all)
    :command
        Category of features that log http request commands.
        (parent categories: :all)
    :headers
        Category of features that log request/response headers.
        (parent categories: :all)
    :body
        Category of features that log request/response bodies.
        (parent categories: :all)
    :xmit-client-request-command
        If set then print the client request commands.
        (parent categories: :xmit, :client, :request, :command)
    :xmit-client-request-headers
        If set then print the client request headers.
        (parent categories: :xmit, :client, :request, :headers)
    :xmit-client-request-body
        If set then print the client request bodies.
        (parent categories: :xmit, :client, :request, :body)
    :xmit-client-response-headers
        If set then print the client response headers.
        (parent categories: :xmit, :client, :response, :headers)
    :xmit-client-response-body
        If set then print the client response bodies.
        (parent categories: :xmit, :client, :response, :body)
    :xmit-server-request-command
        If set then print the server request commands.
        (parent categories: :xmit, :server, :request, :command)
    :xmit-server-request-headers
        If set then print the server request headers.
        (parent categories: :xmit, :server, :request, :headers)
    :xmit-server-request-body
        If set then print the server request bodies.
        (parent categories: :xmit, :server, :request, :body)
    :xmit-server-response-headers
        If set then print the server response headers.
        (parent categories: :xmit, :server, :response, :headers)
    :xmit-server-response-body
        If set then print the server response bodies.
        (parent categories: :xmit, :server, :response, :body)
    :xmit-proxy-client-request-command
        If set then print the proxy request command sent to the real server.
        (parent categories: :xmit, :proxy, :client, :request, :command)
    :xmit-proxy-client-request-headers
        If set then print the proxy request headers sent to the real server.
        (parent categories: :xmit, :proxy, :client, :request, :headers)
    :xmit-proxy-client-request-body
        If set then print the proxy request bodies sent to the real server.
        (parent categories: :xmit, :proxy, :client, :request, :body)
    :xmit-proxy-client-response-headers
        If set then print the proxy response headers sent by the real server.
        (parent categories: :xmit, :proxy, :client, :response, :headers)
    :xmit-proxy-client-response-body
        If set then print the proxy response bodies sent by the real server.
        (parent categories: :xmit, :proxy, :client, :response, :body)
    :xmit-proxy-server-response-headers
        If set then print the proxy response headers sent to the client.
        (parent categories: :xmit, :proxy, :server, :response, :headers)
    :xmit-proxy-server-response-body
        If set then print the proxy response bodies sent by the client.
        (parent categories: :xmit, :proxy, :server, :response, :body)
```

[**ToC**]: https://franz.com/support/documentation/current/doc/contents.htm
[**DocOverview**]: https://franz.com/support/documentation/current/doc/introduction.htm
[**CGDoc**]: https://franz.com/support/documentation/current/doc/cgide.htm
[**RelNotes**]: https://franz.com/support/documentation/current/doc/release-notes.htm
[**FAQ**]: https://franz.com/support/faqs/
[**Index**]: https://franz.com/support/documentation/current/doc/index.htm
[**PermutedIndex**]: https://franz.com/support/documentation/current/doc/permuted-index.htm

[franz]: https://franz.com
[aserve-github]: https://github.com/franzinc/aserve
[latest]: https://franz.com/support/documentation/current/doc/aserve/aserve.html
