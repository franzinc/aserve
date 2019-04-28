# Using Allegro Webactions

## Introduction

The web sites of the past began as a way to display static data. Today's web
sites, however are graphical user interfaces to services or collections of data.
They are no longer simply web sites, but rather *web applications*. From banking
tasks to airline reservations to news sites, consumers are accessing
content-specific data via an internet browser.

Creating a web application is, in many ways, more difficult than creating a
standalone application - and it will become even more so as users demand
increasingly customized features/services. This paper will discuss some of the
key challenges web application developers face, and how *Allegro Webactions*
solves these problems. Allegro Webactions sits on top of AllegroServe (Franz
Inc.'s Lisp-based web server) and provides a framework for building web
applications that are easy to maintain and update.

## Skyscrapers can't be built on the foundation of a Log Cabin

Customers are continuously demanding more, better, and faster services from web
applications. Unfortunately, many developers are still attempting to layer
dynamic features on top of a static structure. In order to successfully cater
to user demands, we must change the paradigm of how web sites are built and
structured. What worked for a 10 page web site two years ago, will not work for
a 100 page site that includes numerous databases and an e-commerce application.
Some key challenges that need to be addressed include the following:

### SessionControl

The web server must track individual visitors as they navigate through the web
site. The first time a visitor comes to the web site the server assigns that
visitor a *session object*. Subsequent requests by the same visitor must be
assigned the same session object so that the code processing the request does
work for that specific visitor. For example a visitor entering a shopping site
is assigned a virtual shopping cart. As the visitor adds items to the cart he
expects to see previously added items still in the cart. Without a session
object the visitor would find that his shopping cart was always empty which,
needless to say, would destroy the concept of online shopping.

The *http* protocol on which the web is built is a connectionless protocol. A
visitor does not connect to a web site and stay connected until moving to
another web site. Thus there needs to be some means for each request to
identify the visitor behind the request. *Cookies* were designed for this
purpose. A web server can ask a web browser to store a value on the visitor's
machine that will be returned to the web server on subsequent requests.

Cookies are a great tool for tracking sessions however some visitors disable
cookies due to privacy concerns. Therefore, a web application must be prepared
to fall back on other methods for tracking sessions.

### Web Designer vs. Programmer

Building a web application requires two types of talent: a web page designer and
a programmer. Once the web site is up and running the daily maintenance can be
done by the web page designer. If significant new functionality is needed the
programmer may have to return to write new code.

A successful web application framework should *allow* for the web site to be
modified by either the designer or the programmer, depending on the task.
Requiring a programmer to be involved every time a product or text change occurs
is inefficient and expensive.

### Dynamic Site Enhancement

Web sites evolve constantly - some even need to be updated hourly. And, if a
site is being accessed many times a second, the site can't go down while these
modifications are made. A site's framework must support changing the static
content and the dynamic content generation functions.

### Dealing With Complexity

A web application usually consists of a large set of pages with various links
between them. When new features are added, new pages are also added, which
cause links to be added or modified. Plus, there are often several paths
through the web site that must yield equivalent results. For example, in a
shopping site a visitor can just start adding items to the shopping cart, and
then identify himself at check-out. Alternatively, a visitor can first identify
himself and then start adding items to his cart. In either case, the same
products are ordered..

Many web sites are like unstructured programs with "goto's" all over the
place. As more products and features are added, the complexity can become
overwhelming - the site becomes slower, more brittle, and less robust with each
change. Something has to be done to organize and simplify the process.

## Model View Controller

Suppose a web site is selling clothing. A user sees a nice sweater and clicks
on the button to add that sweater to his cart. The webserver gets the request.
How should it handle it?

One way would be to write a function that identifies the user sending the
request, adds the sweater to that user's shopping cart, and then writes out a
page showing the current contents of the cart. This type of *monolithic
function* approach works, but is not very flexible. Perhaps a change needs to
be made that requires an inventory check first, and then displays a different
message if the item is not in inventory. That would make the programmer have to
rewrite the monolithic function. Maybe the cart contents need to be displayed
from a link on every store page. Since the cart displayer code is in the
handler function we just described, the code must be duplicated or pulled out to
be a separate routine. In the monolithic function approach, the logic to choose
what to display next is intermixed with the code to manipulate the store
database and to display store items. This makes it very hard to figure out all
the links through the web site.

A better way to design a web application is to separate out these three types of
code:

  - Model - this is code to manipulate the data of the website. If the web site
    is a store then this category includes the code to create carts and orders,
    compute shipping costs and charge credit cards.
  - View - this is the code to display the data of the web site on a web page.
    The data is not modified by the code that causes it to be displayed.
  - Controller - when a link or button is clicked on a web page this causes the
    action (if any) to be performed and then causes something to be sent back to
    the browser. The action is performed by calling on Model functions. The
    browser received data due to a call on a View function. The controller is
    the manager of the web site, it directs requests to model functions and to
    view functions.

Using the MVC paradigm when the user asks to add a sweater to his cart a Model
function is called. The Model function manipulates the store's databases to add
the sweater to the user's cart. The View function then displays the cart.

## A Flexible Foundation

The paradigm described above, can easily be executed using Allegro WebActions
and AllegroServe. Although there exist a number of web application frameworks,
Webactions' simplicity, transparency and power distinguish it from other tools.
Allegro WebActions uses Lisp as the web extension language. Lisp is not a
simple scripting language (like Perl or PHP) or a byte interpreted
strongly-typed language (like Java). Lisp is a flexible runtime-typed language
that compiles down to machine code. As a result, code written in the Lisp
extension language runs at machine speed with no interpreter overhead.

Webactions borrows ideas from the
[Struts](http://jakarta.apache.org/struts/index.html) web application framework
for Java. Struts has good ideas for mapping web applications into the
Model-View-Controller paradigm (more on this below). Due to the more static
design of the Java language, Struts is not as easy to use as Webactions.

## Webactions components

Unlike many other tools, which were cobbled together as web site requirements
increased, Allegro WebActions was designed specifically to support complex web
applications. It can easily support the key challenges outlined in this paper:

### Session Control

Webactions supports *url rewriting* in addition to *Cookies*. In url rewriting
the links to other pages in the site are modified to include a session
identifier. Webactions automatically does the check for disabled cookies and
then switches to url rewriting, if necessary.

Webactions is also careful to create session identifiers that are unique and
virtually impossible to guess. This prevents a malicious user from guessing a
valid session identifier and hijacking another user's session.

### Separation of Text and Code

Webactions provides a clear distinction between web design and programming,
allowing web designers to work independently with their choice of tools.
Webactions introduces a syntax of special tags that display dynamic content in
static pages. This syntax was designed specifically so that existing "What you
see is what you get" (WYSIWYG) html editors such as FrontPage and Mozilla will
accept them. Webactions does not permit programming constructs to appear in
html pages, so web designers won't see code they don't understand. This is not
to say that Webactions forbids other scripting languages (such as javascript)
from appearing in web pages. Webactions simply does not add its *own* set of
programming constructs to html (as Java Server Pages do for example).

### Dynamic Updates

Webactions automatically notices when static pages are updated on the disk and
begins serving them immediately. One can load in new definitions of dynamic
functions and have them in use right away (this is a benefit of all Lisp
programs).

### Complexity

Webactions adds a layer of abstraction which greatly simplifies the code. Web
pages are denoted by symbolic names. Links from one web page to another are
made using the symbolic name, as well. There is a single project description
that lists all symbolic page names and describes how each is rendered. This
project description allows a programmer to see a complete overview of the web
site. Also, symbolic page names can be changed without having to edit any file
that refers to it.

## Components of an Allegro WebActions Web Site

  1. **Project Description** - Describes the pages in the project and where
     static pages are found and gives some parameters values. The project
     description can be modified and loaded in while the web site is running,
     thus allowing the site to change dynamically (in true Lisp fashion).
  2. **Static Pages** -Created by the web designer, they can be changed at any
     time. The next time an updated or new page is referenced, the new value
     will be used.
  3. **clp-functions** - these are Lisp functions that display html (generally
     these are used to show the dynamic parts of pages).
  4. **Action functions** - these are Lisp functions that interact with the
     application behind the web site (e.g. the store).

In MVC terms, the project description is input to the Controller. Items 2 and 3
are View components. The Model functions are item 4. Allegro Webactions itself
implements the Controller.

## Simple Example

Let's examine a very simple Webactions web site. The project description is:

```lisp
(webaction-project "simpleproject"
        :destination "site/"
        :index "home"
           :map
        '(("home"   "pageone.clp")
            ("second" "pagetwo.clp")))
```

Let's skip right to the :map argument. Its value is a list of the pages that
make up this project. Each page is described by its symbolic page name followed
by an object which describes how the page is to be rendered. In this example we
have two pages, one named "home" and the other "second". The render value for
each of these pages is a string which in Webactions means that each is rendered
by serving a file on disk with the given name.

There are two files that accompany this project description:

`site/pageone.clp`:

```xml
<html>
<body>
This is page one.
<br>
Go to <a href="second">page two<a>
</body>
</html>
```

`site/pagetwo.clp`:

```xml
<html>
<body>
This is page two.
<br>
Go to <a href="home">page one<a>
</body>
</html>
```

You'll note that the CLP files look just like HTML files. This is intentional.
A CLP file is just an html file that is processed by Webactions before being
sent to the browser. Two types of processing are done by Webactions. First all
symbolic page references are replaced by page references that work in the
current context. Symbolic page references are found in two places: `href="xxx"`
inside an `<a>` element and `action="xxx"` inside a `<form>` element. Second if
there are references in the CLP file to special CLP Lisp functions, then those
functions are called at the appropriate time when the page is being sent to the
browser.

In our example above each CLP page has one symbolic page reference. Suppose we
load Webactions and this project definition into Lisp and start the server on
port 8000. We go to a web browser and ask for `http://localhost:8000/`. What
happens is the url in the web browser changes to `http://localhost:8000/home`
and we see:

```html
<div style="margin-left: 80px;">

This is page one.
Go to
<span style="color: rgb(51, 102, 255); text-decoration: underline;">page
two.</span>

</div>
```

The reason the url changed to "/home" is that in the project definition we
specified "home" as the index page for this project. Thus accessing the project
without specifying a particular page resulted in the request being redirected to
the index page.

We click on the "page two" link and the browser now shows a url like:

```html
<div style="margin-left: 40px;">

<span style="font-weight: bold;"> </span>

http://localhost:8000/\~159c546f07540c5a9ee4155d\~/pagetwo.clp

</div>
```

and we see in the main frame of the web browser:

```html
<div style="margin-left: 80px;">

This is page two.
Go to
<span style="color: rgb(51, 102, 255); text-decoration: underline;">page
one</span>.

</div>
```

Why does the url look so strange?  The reason is that when the first page (the
page named "home") was processed by Webactions and sent back to the browser,
Webactions didn't know if the browser would accept a cookie. What Webactions
did was send a cookie back with the page and at the same time it put the session
identifier in the url for all symbolic page references. The number
159c546f07540c5a9ee4155d is the session id chosen by Webactions. When you
clicked on <span style="color: rgb(51, 102, 255); text-decoration:
underline;">page two</span> this caused the web browser to send a request for
that page to Webactions. That request either arrived at the web server with
cookie value or it did not. If the request came with the cookie value then
Webactions notes that for this session Webactions can depend on the cookie value
being sent with each request and thus Webactions will not put the session id in
any more urls. If the cookie value wasn't sent then Webactions notes that it
will have to alter urls for this session and Webactions no longer tries to send
cookies for this session.

If you run this simple example twice, once with your browser set to accept
cookies and once with it set to not accept cookies you can see how Webactions
adapts to your browser setting and still maintains session identity.

This simple two web page example doesn't do anything that couldn't have been
done with two static html pages. Even though this example doesn't make use of
it, Webactions is maintaining session information while the pages of this
example are accessed. We'll extend the example to make use of session tracking
by counting the number of times pages in this session were accessed.


## Adding a CLP function

We'll change the two CLP files to add a line showing the hit count:

`site/pageone.clp`:

```xml
<html>
<body>
This is page one.
<br>
Go to <a href="second">page two<a>
<br>
session hits: <sample_hitcount/>
</body>
</html>
```

`site/pagetwo.clp`:

```xml
<html>
<body>
This is page two.
<br>
Go to <a href="home">page one<a>
<br>
session hits: <sample_hitcount/>
</body>
</html>
```

and then we define a CLP function in this way and load it into the Lisp running
the web server:

```lisp
(def-clp-function sample_hitcount (req ent args body)
  (let ((session (websession-from-req req)))
    (net.html.generator:html
     (:princ
      (setf (websession-variable session "hitcount")
 (1+ (or (websession-variable session "hitcount") 0)))))))

```

Now we again go to a browser and view the pages. Now we see pages like this:

```html
<div style="margin-left: 80px;">

This is page one.
Go to
<span style="color: rgb(51, 102, 255); text-decoration: underline;">page
two.
</span>session hits: 9

</div>
```

What is happening now is that when Webactions returns pageone.clp it sends all
of the page up to but not including `<sample_hitcount/>` to the browser, then it
runs the `sample_hitcount` CLP function, and then it sends the contents of
`pageone.clp` after `<sample_hitcount/>` to the browser. The `sample_hitcount`
function retrieves the session object associated with this request (using
**`websession-from-req`**) and uses this session object to increment the session
variable named "hitcount". You're free to define as many session variables as
you wish. If you access a session variable that hasn't been set, the value
**`nil`** is returned. `sample_hitcount` increments the "hitcount" variable for
this session and then prints it to the html stream.

You may have noticed in the CLP files that we used an xhtml syntax when we wrote
html code to invoke `sample_hitcount`:

```html
<div style="margin-left: 40px;">

<sample_hitcount/>

</div>

We could have written the equivalent

<div style="margin-left: 40px;">

<sample_hitcount></sample_hitcount>

</div>
```

instead but the former is easier to type. While some html elements don't have a
body (e.g. `<img>`) *all* CLP elements we add to html have a body and the end of
the body *must* be denoted in one of the two above ways.

## Actions

The previous examples show the use of static CLP pages and the introduction of
dynamic content via CLP functions. There is one important part of Webactions
left to describe and that is Actions. When a web page is referenced
symbolically it can invoke a lisp function to perform some action. Usually that
action affects the data object behind the web site or the current session
object. After the action is performed an invisible redirect is done to another
page on the web site which is then handled either by another action or by
displaying a web page. Actions should never send anything to the web browser.
The sole function of an action is to affect the state of the Model behind the
web site for this particular session.

Our example for the use of actions is something found in most dynamic web sites
these days: the login page. In our sample web site we want to know the name of
the person visiting our site so we can personalize the page. The user enters
the web site at the main entry point and we check to see if he has logged on
yet. If so we go right to the home page of the site. If he hasn't then we ask
him to identify himself and once that's done we go to the home page of the site.

We begin with the project description and the two actions referenced:

```lisp
(webaction-project "simpleproject"
        :destination "site/"
        :index "home"
           :map
        '(("home"   action-check-login)
           ("login" "login.clp")
           ("gotlogin" action-got-login)
           ("realhome" "home.clp")))




(defun action-check-login (req ent)
  (let ((session (websession-from-req req)))
    (let ((user (websession-variable session "username")))
      (if* user
   then ; already logged in
         ; just go to the real home
          "realhome"
     else ; must login
        "login"))))

(defun action-got-login (req ent)
  (let ((username (request-query-value "username" req)))
    (if* (and username (> (length username) 0))
       then (setf (websession-variable (websession-from-req req) "user")
          username)
     "realhome"
       else "login")))
```


We have four symbolic pages named in this project. Two of them refer to CLP
files we'll show below. Two others refer to lisp functions that we call *action
functions*. Users coming to the site are redirected to the page with symbolic
name "home". That causes **`action-check-login`** to be called. The
**`action-check-login`** function checks to see if this session has a non-nil value
for the session variable "user". If so the user has already logged in. Action
functions never send anything back to the browser. They simply return a string
which is the symbolic name of the page in the project that should be processed
next. This action function returns either "realhome" or "login". Both of those
symbolic page names refer to CLP files in the project description.

`site/login.clp`:

```html
<html>
<body>
What is your name:
<form action="gotlogin">
<input type="text" name="username">
<input type="submit">
</form>
</body>
</html>
```

`site/home.clp`:

```html
<html>
<body>
Welcome to my page, <clp_value name="user" session/>.
</body>
</html>
```

The login.clp file puts up a form that asks the visitor to enter his name. When
the submit button is pressed control goes to symbolic page name "gotlogin". In
our project, symbolic page name "gotlogin" is handled by action function
**`action-got-login`** shown above. This action function reads the user name from
the form and if it's non-empty it stores the user name in the session variable
named "user". **`action-got-login`** returns either symbolic page name "realhome"
(if a valid user name was given) or "login" if a name wasn't given and if the
visitor must try again to identify himself. The symbolic page "realhome" is
connected to the file home.clp. This is a very simple home page that simply
welcomes the user by name. The `clp_value` function is part of the built-in
Webactions library. It retrieves and prints the value of a variable. Here the
variable name is "user" and the context is "session".

## Further Information

The [Allegro Webactions](webactions.md) document is a reference manual for CLP
pages and Webactions.

## Summary

Allegro WebActions is a dynamic framework for building a web application.
Webactions does the work necessary to track sessions whether or not the browser
accepts cookies and allows the visual part of the website to be designed by html
programmers using tools they are accustomed to using. The dynamic part of the
web site is clearly partitioned from the static part so that the programming
behind the web site will not interfere with the visual part. Allegro WebActions
makes it easier to structure and update any web application in a cleaner,
simpler way than with other current web application building tools.
