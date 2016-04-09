;;
;; chat.cl
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2013 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;

;; Description:
;;   aserve chat program

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-



(defpackage :user (:use :net.aserve :excl :common-lisp :net.uri
			:net.aserve.client
			:net.html.generator))
(in-package :user)

(net.aserve::check-smp-consistency)

(defmacro with-mp-locked-controller ((c) &body body)
  (net.aserve::smp-case
   ((t :macros) `(with-locked-object (,c :non-smp :without-scheduling)
		   ,@body))
   (nil `(si::without-scheduling ,c ,@body))))

(defvar *chat-home-package* :user) ; :user for now while debugging
(defvar *chat-home*)     ; home dir for chat info
(defvar *chat-home-pics*)     ; home dir for chat pics
(defvar *chat-picname* 0)
(defvar *default-count* 10)
(defvar *default-secs*  10)

; secret path to get back to admin control
(defvar *quick-return-path* "/xyzz") 

(defvar *idle-timeout* (* 30 60)) ; 30 minutes

(defvar *do-dnscheck* nil) ; translate ip to dns names

(defvar *chat-hook* nil) ; invoked when the chat page is accessed

(defvar *offer-transcript* nil) ; offer a chat transcript

(defparameter *initial-msg-size* 100)  ; size of initial message array
(defparameter *msg-increment* 200)  ; how much to grow array each time

;; parameters
;
; one set of of paraamter is the page style of the top frame
;   call (set-style xxx) where xxx is one of the *xxx-style* values
;  (set-style *normal-style*)
;  (set-style *white-style*)
;
; setting *background-image* to an image url will put that url on
; the background of the top window
; e.g.
;  (setq *background-image* "http://www.franz.com/~jkf/aserveback4.gif")
;  (setq *background-image* nil)
; set *recent-first* to true to make the newest messages show first
;
; set *show-style* to 1 for normal, 2 for tables
; (setq *show-style* 1)
; (setq *show-style* 2)
;



(defparameter *bottom-frames-bgcolor* "#dddddd") ; gray
(defparameter *bottom-frames-private* "#ff5555") ; for private messaging

(defparameter *private-font-color*  "#ff4444") ; red
(defparameter *public-font-color* "#ffcc66") ; gold

(defstruct color-style
  bgcolor
  font-color
  vlink-color
  link-color
  alink-color)

(defparameter *normal-style* 
    (make-color-style 
     :bgcolor 		"#000000" ; black
     :font-color        "#ffcc66" ; gold
     :vlink-color 	"#ffaaaa" ; red
     :link-color 	"#aaffaa" ; green
     :alink-color       "#aaaaff" ; blue
     ))

(defparameter *white-style* 
    (make-color-style 
     :bgcolor 		"#ffffff" ; white
     :font-color        "#000000" ; black
     :vlink-color 	"#ff0000" ; red
     :link-color 	"#0000ff" ; blue
     :alink-color       "#00aa00" ; green
     ))



(defvar *top-frame-bgcolor* )
(defvar *top-frame-font-color*)
(defvar *top-frame-vlink-color*)
(defvar *top-frame-link-color*)
(defvar *top-frame-alink-color*)

(defvar *background-image* nil)

(defvar *message-id-hook* nil) ; if true it can contribute to the messsage line

(defvar *max-active-time* #.(* 2 60)) ; after 2 minutes no longer active


(defvar *recent-first* t)   ; if true show most recent messages first

(defvar *show-style* 1)     ; 1 = tables, 2 = just entries

; true if we wish to restrict messaging at all based on logged in 
; and level
(defvar *restrict-messages* nil) 

; true if we show the machine name of chatters to everyone instead
; of just the owner
(defvar *show-machine-name-to-all* t)

; force it to be loaded
;(defparameter *ensure-ef* (find-external-format :utf-8))
  
;; sample building command to create a standalone chat serve
#|
(generate-application "allegrochat"
		      "allegrochat/"
		      '(:sock :process :seq2 :acldns 
			"aserve/aserve.fasl"
			"aserve/examples/chat.fasl"
			)
		      :restart-init-function
		      'start-chat-standalone
		      :include-compiler nil
		      :read-init-files nil
		      :include-debugger t
		      :ignore-command-line-arguments t)

|#
			  


;
; query attribute usage:
;  u = controller ustring
;  c = chat ustring
;  s = secret key (specific to the object being accessed)
;  x = user uid
;  pp = uid of person sending message to, * means all
;  purl = picture url
;  z = lurk
;  y = delete message
;  b = upgrade user


(defclass master-chat-controller (#+smp mp:lockable-object)
  ((controllers :initform nil
		; list of chat-controller instances
		:initarg :controllers
		:accessor controllers)
   (ustrings :initform nil 
	     :initarg :ustrings
	     :accessor ustrings)
   (master-lock :initform (mp:make-process-lock :name "chat master")
		;; used when doing serious altering to chat info
		:reader master-lock)
   (secret-key :initarg :secret-key
	       ;; to unlock the setup-chat
	       :initform (make-unique-string)
	       :reader secret-key)
   (users :initform nil
	  :initarg :users
	  ;; list of user objects
	  :accessor users)
   ))


(defvar *master-controller* nil) ; the master-controller instance



(defclass chat-controller (#+smp mp:lockable-object)
  ;; describes a whole set of chats
  
  ((chats :initform nil
	  ; list of chat instances
	  :initarg :chats
	  :accessor chats)
   (owner-name :initarg :owner-name
	       :reader owner-name)
   (controller-name :initarg :controller-name
		    :reader controller-name)
   (ustring :initarg :ustring :accessor ustring) ; un
   (ustrings :initform nil
	     ;; ustrings of all the chats
	     :initarg :ustrings
	     :accessor ustrings)
   (secret-key :initarg :secret-key
	       ;; knowing this key gives you access to 
	       ;; changing the parameters of the chat
	       :accessor secret-key)
   (controller-uri :initarg :controller-uri
		   ;; uri to reach this controller page
		   :accessor controller-uri)
   (controller-query-string :initarg :controller-query-string
			    ; u=xxxxx&s=xxxxx  specify this controller and
			    ;	the secret key for this controller
			    :reader controller-query-string)
   ))
   
   

(defclass chat ()
  ((name :initarg :name
	 :reader chat-name
	 )
   
   (state :initform :open
	  ; :open or :closed
	  :initarg :state
	  :accessor chat-state)
   
   (ustring :initarg :ustring
	    :accessor ustring)

   (filename :initarg :filename
	     ;; name of file holding chat info.
	     ;; should be just a name, no directory stuff, so
	     ;; it can be relative to the chat home
	     :accessor chat-filename)
   
   (secret-key :initarg :secret-key
	       ;; to do admin things to this chat
	       :initform (make-unique-string)
	       :reader secret-key)
   
   (chat-query-string :initarg :chat-query-string
		      ;; u=xxxx&c=yyyyyy  indentifies chat
		      :reader chat-query-string)
   (chat-owner-query-string :initarg :chat-owner-query-string
		      ;; u=xxxx&c=yyyyyy&s=xxxx  indentifies chat
			    :reader chat-owner-query-string)
   
   
   (messages :initform (make-array *initial-msg-size*)
	     :accessor chat-messages)
   (message-next :initform 0
		 ;; index in the messages array of place
		 ;; to store next message
		 :accessor chat-message-next)
   (message-number :initform 0
		   :initarg :message-number
		   ;; message number of the next message
		   :accessor chat-message-number)
   (message-archive :initform 0
		    :initarg :message-archive
		     ;; 1+ last message number archived so far
		    :accessor chat-message-archive)
   ; used by experimental code to delete private messages
   ; message number to scan next
   (message-prvcheck :initform 0
		     :accessor chat-message-prvcheck)
   
   ; list of deleted message numbers since the last archive
   (messages-deleted :initform nil
		     :initarg :messages-deleted
		     :accessor chat-messages-deleted)
   
   
   (message-lock :initform (mp:make-process-lock :name "chat-message")
		 ; grab this before changing the above
		 :accessor chat-message-lock)
   

   ;; list of people monitoring this chat
   (viewers :initform (make-viewers)
	    :accessor chat-viewers)
   
   ; used as index value in the redirect struct
   (redirect-counter :initform 0
		     :initarg :redirect-counter
		     :accessor redirect-counter)
   
   ; list of redirect structures
   (redirects :initform nil
	      :initarg :redirects
	      :accessor chat-redirects)
   ))

(defstruct user 
  handle	; official handle of the user
  password	; password string
  ustring	; unique string of this user
  pstring	; unique string, this one denotes user as a send target
  cookie	; cookie stored under achat name
  level         ; nil - novice, 1 - higher privs
  (time	0)	;  time of last user activity
  to-users      ; string holding comma sep list of users to send to (nil=all)
  )


(defstruct viewers 
  (lock (mp:make-process-lock :name "viewers-lock"))
  list	; list of viewent
  )

(defstruct viewent
  time	; time of last read, nil to make this one unused
  user	; if user access, then user object
  ipaddr ; if random access then ipaddr
  hostname ; string if we've figured it out
  )



(defstruct message
  number  ; is unique for each chat
  ipaddr  ; integer ip address of the source
  dns     ; dns name corresponding to the ip address
  handle  ; from handle (for unlogged in user)
  real    ; true if this is a real handle of a logged in user
  to	  ; if non nil then a list of handles who are the target of this message
          ; if nil then this goes to no-one
          ; if t then this goes to everyone
  time    ; string - message time in a pretty format
  (ut 0)  ; universal time of message
  body)



(defstruct redirect
  index		; unique name for each redirect
  
  ;; structure describing the redirection of a group of ip addresses
  ipaddr	; bits not under the mask are ignored
  maskbits	; bits from 0 to 32
  mask		; the actual mask
  
  to		; where to send the redirect
  before	; true if we check before seeing if they are logged in
  info		; string describing this redirect
  (use	0)	; use count 
  active	; true if active
  )

  
  
;; roles
; master-controller - can create controllers.  has a secret key (s)
; controller - can create chats, each has a public key (u) and
;	       a private key (s).  
; chat - is a collection of posted messages.  has a public key (c)
;	 and a controller public key (u) and a secret key (s)
;	 Most access the chat using u and c.  If you also know s then
;	 you have owner priviledges to the chat
;


;; pages
;
; url		set		what
;
; setup-chat	-		if no chat setup yet, bring up first page
;				with link to first controller page page
; setup-chat	s		s has master control key, bring up page of
;				existing chat controllers and form for
;				craeting new one.  This is the master controller
;				page.
; new-controller s,name,controllername
;				posted from setup-chat
;				s is the master controller secret key
;				name and controllername are used to build
;				new controller object.
; controller	u,s		u has controller public string, s has
;				controller private string, show chats by
;				this controller and offer to create a new one
; create-chat	u,s,name,filename   create a new chat under the controller
;				denoted by u.  s has the controller private
;				string (secret key)
; chat		u,c,[s]		build frameset for the given chat.
;				s is the chat secret key [if given.]
;  chattop      u,c,[s],count,secs,y,z,b  display count message and refresh in secs
;  chaviewers   u,c,[s]         list who is watching the chat
;  chatenter    u,c,[s],pp,purl		box into which to enter a message
;  chatcontrol  u,c,[s]		specify message count and refresh seconds
; chatlogin	u,c,x,[s]	login to a existing user or create a new user
;
; chatloginnew  u,c,[s],handle,password,password2
;				define new user
;
; chatlogincurrent u,c,[s],handle,password
;				login as an existing user
;
; chatmaster	u,c,s		control elements of the chat.
;
;


; top level published urls




; functions
(defun start-chat (&key port home restart (listeners 10))
  ;; start the chat system going
  (declare (special socket::*dns-configured*))
  
  ;(unpublish :all t) ; useful during debugging, remove afterwards
  
  (if* (not (stringp home))
     then (error "must specify :home value as a string naming a directory (no trailing slash)"))
  
  (setq *chat-home* home)
  
  (ignore-errors
   (excl::mkdir (setq *chat-home-pics*
		  (concatenate 'string *chat-home* "/pics")) #o755))
  
  (setq *chat-picname*
    (logand #xffffff (* 8 (get-universal-time))))
  
  (publish-directory :prefix "/chatpics"
		     :destination *chat-home-pics*
		     )
  
  (setq *master-controller* nil)
  
  (if* (not restart)
     then (load-existing-chat *chat-home*)
	  (let (did-fixup)
	    ;; temp to add cookies to old chat
	    (dolist (user (users *master-controller*))
	      (if* (null (user-cookie user))
		 then (setf (user-cookie user) (make-unique-string))
		      (setq did-fixup t)))
	    (if* did-fixup then (dump-existing-chat *chat-home*))))
  
  (if* *master-controller*
     then ; we have an existing chat setup
	  (publish-chat-links)
	  (start-chat-archiver *master-controller*)
	  )
  
  (publish :path "/setup-chat" :function 'setup-chat 
	   ; :content-type "text/html; charset=utf-8"
	   :content-type "text/html"
	   )

  ; setup for reverse dns lookups.  don't do reverse lookups if we
  ; have to use the C library
  #+(version>= 6 0)
  (if* (and (boundp 'socket::*dns-configured*)
	    socket::*dns-configured*)
     thenret
     else (socket:configure-dns :auto t)
	  (setq *do-dnscheck* socket::*dns-configured*
		socket::*dns-mode* :acldns))
  
  
  (if* port then (net.aserve:start :port port :listeners listeners
				   ; :external-format (crlf-base-ef :utf-8)
				   )
	  )
  )


(defun start-chat-standalone ()
  ;; useful function for starting chat standalone where the 
  ;; port and home arguments are required
  
  (if* (not (eql 5 (length (sys:command-line-arguments))))
     then (format t "use: ~s  port  home~%" (sys:command-line-argument 0))
          (exit 1))

  (let ((port (read-from-string (nth 3 (sys:command-line-arguments))))
        (home (nth 4 (sys:command-line-arguments))))
    (start-chat :port port :home home)
    (loop (sleep 9999999))))

(defun shutdown-chat ()
  ;; stop the chat
  (net.aserve:shutdown)
  (setq *master-controller* nil)
  (sleep 10)
  (exit 0 :quiet t))

  
(defun publish-chat-links ()

  ; debugging only.  builds link to the master controller page 
  (publish :path *quick-return-path* :function 'quick-return-master
	   ; :content-type "text/html; charset=utf-8"
	   :content-type "text/html"
	   )
  
  
  ; post'ed from form in setup-chat
  (publish :path "/new-controller" :function 'new-controller
	   ;:content-type "text/html; charset=utf-8"	   
	   :content-type "text/html"
	   )

  (publish :path "/controller" :function 'existing-controller
	   ;:content-type "text/html; charset=utf-8"
	   :content-type "text/html"
	   )

  ; get'ed from the controller page when user asks to create a chat
  (publish :path "/create-chat" :function 'create-chat
	   ;:content-type "text/html; charset=utf-8"
	   :content-type "text/html"
	   )


  (publish :path "/chat"  :function 'chat
	   ;:content-type "text/html; charset=utf-8"
	   :content-type "text/html"
	   )
  (publish :path "/chattop" :function 'chattop
	   ;:content-type "text/html; charset=utf-8"	   
	   :content-type "text/html"
	   )

  (publish :path "/chatenter" :function 'chatenter
	   ;:content-type "text/html; charset=utf-8"	   
	   :content-type "text/html"
	   )
  
  (publish :path "/chatenter-pic" :function 'chatenter-pic
	   ;:content-type "text/html; charset=utf-8"	   
	   :content-type "text/html"
	   )

  (publish :path "/chatcontrol" :function 'chatcontrol
	   ;:content-type "text/html; charset=utf-8"	   
	   :content-type "text/html"
	   )
  
  (publish :path "/chatlogin" :function 'chatlogin
	   ;:content-type "text/html; charset=utf-8"	   
	   :content-type "text/html"
	   )
  
  (publish :path "/chatloginnew" :function 'chatloginnew
	   ;:content-type "text/html; charset=utf-8"	   
	   :content-type "text/html"
	   )
  
  (publish :path "/chatlogincurrent" 
	   :function 'chat-login-current
	   ;:content-type "text/html; charset=utf-8"	   
	   :content-type "text/html"
	   )
  
  (publish :path "/chatviewers" :function 'chatviewers
	   ;:content-type "text/html; charset=utf-8"
	   :content-type "text/html"
	   )
  
  (publish :path "/chatmaster" :function 'chatmaster
	   ;:content-type "text/html; charset=utf-8"
	   :content-type "text/html"
	   )
  
  (publish :path "/chattranscript" :function 'chattranscript
	   ;:content-type "text/html; charset=utf-8"
	   :content-type "text/html"
	   )
  )


(defun load-existing-chat (home)
  ;; read in and build the chat information
  (declare (special user::value1))
  
  (let ((master-file (concatenate 'string home "/cmaster.cl")))
    (if* (probe-file master-file)
       then (with-standard-io-syntax
	      (load master-file)
	      (if* (boundp 'user::value1)
		 then (setq *master-controller* user::value1)
		      ; ensure users have cookies
		      )
	      
	      ;; now read in chat data
	      (dolist (controller (controllers *master-controller*))
		(dolist (chat (chats controller))
		  (and (archive-filename chat)
		       (probe-file (archive-filename chat))
		       (let (did-delete)
			 (with-open-file (p (archive-filename chat)
					  :direction :input
					  :external-format :octets)
			   (do ((message (read p nil :eof) (read p nil :eof)))
			       ((eq message :eof)
				; use everything is archived we've read
				(setf (chat-message-archive chat) 
				  (chat-message-number chat))
				; remove those put back on redundantly 
				; by delete-chat-message
				(setf (chat-messages-deleted chat) nil))
			     (if* message
				then (if* (and (consp message)
					       (eq :delete (car message)))
					then (mapcar #'(lambda (num)
							 (delete-chat-message
							  chat num t nil))
						     (cdr message))
					     (setq did-delete t)
					else (add-chat-message chat message)))))
		    
			 (if* did-delete
			    then ; write out archive again this time
				 ; without the deleted messages
				 (format t "Rewriting ~s~%" (archive-filename chat))
				 (let ((messages (chat-messages chat)))
				   (with-open-file (p (archive-filename chat)
						    :direction :output
						    :if-exists :supersede
						    ;:external-format :utf-8
						    )
				     (dotimes (i (chat-message-next chat))
				       (let ((message (svref messages i)))
					 (if* (message-to message)
					    then (pprint (svref messages i) p)))))))))))))))
    
(defun dump-existing-chat (home)
  (mp:with-process-lock ((master-lock *master-controller*))
    (labels ((dump-master-chat-controller (masterc)
	       `(make-instance 'master-chat-controller
		  :ustrings ',(ustrings masterc)
		  :secret-key ',(secret-key masterc)
		  :controllers
		  (list ,@(mapcar #'dump-chat-controller 
				  (controllers masterc)))
		  :users ',(users masterc)
		  ))
	     
	     (dump-chat-controller (controller)
	       `(make-instance 'chat-controller
		  :chats
		  (list ,@(mapcar #'dump-chat (chats controller)))
		  :owner-name ',(owner-name controller)
		  :controller-name ',(controller-name controller)
		  :ustring ',(ustring controller)
		  :ustrings ',(ustrings controller)
		  :secret-key ',(secret-key controller)
		  :controller-uri ',(controller-uri controller)
		  :controller-query-string
		  ',(controller-query-string controller)))
	   
	     (dump-chat (chat)
	       `(make-instance 'chat
		  :name ',(chat-name chat)
		  :state ',(chat-state chat)
		  :ustring ',(ustring chat)
		  :filename ',(chat-filename chat)
		  :secret-key ',(secret-key chat)
		  :chat-query-string ',(chat-query-string chat)
		  :chat-owner-query-string ',(chat-owner-query-string chat)
		  :redirect-counter ',(redirect-counter chat)
		  :redirects ',(chat-redirects chat)
		  ))
	     
	     )
	     
				 
      (let ((new-master-file (concatenate 'string home "/ncmaster.cl"))
	    (master-file (concatenate 'string home "/cmaster.cl"))
	    (value))

	(setq value
	  `(setq user::value1
	     ,(dump-master-chat-controller *master-controller*)))
			
	(with-open-file (p new-master-file 
			 :direction :output
			 :if-exists :supersede
			 ;:external-format :utf-8
			 )
	  (with-standard-io-syntax 
	    (let ((*package* (find-package *chat-home-package*)))
	      (format p ";;Automatically generated, do not edit~%")
	      (print `(in-package ,*chat-home-package*) p)
	      (pprint value p)
	      (terpri p))))
    
	; success, so make it the official one
	(ignore-errors (delete-file master-file))
	
	#-(version>= 6 2 :pre-beta 11)
	(rename-file new-master-file master-file)
	
	#+(version>= 6 2 :pre-beta 11)
        (rename-file-raw new-master-file master-file)
	))))


    
    
      
      
      
  

(defun quick-return-master (req ent)
  ;; quick hack to get us to the master controller while debugging
  (if* (null *master-controller*)
     then (ancient-link-error req ent)
     else (with-http-response (req ent)
	    (with-http-body (req ent)
	      (html 
	       (:html
		(:body "The master controllers is "
		       ((:a href 
			    (format nil "setup-chat?s=~a"
				    (secret-key *master-controller*)))
			"here"))))))))
			   
(defun illegal-access (req ent)
  (with-http-response (req ent)
	    (with-http-body (req ent)
	      (html
	       (:html 
		(:head (:title "illegal access"))
		(:body "You are attempting to gain illegal access to this "
		       "chat control.  Stop doing this."))))))

(defun setup-chat (req ent)
  ;; this is the first function called to start a whole chat
  ;; system going (building a master controller) and is also
  ;; the function used by the master controller to specify new
  ;; controllers.
  (if* (null *master-controller*)
     then (setq *master-controller* (make-instance 'master-chat-controller))
	  (dump-existing-chat *chat-home*)
	  (do-first-setup-page req ent)
	  (start-chat-archiver *master-controller*)
   elseif (not (equal (secret-key *master-controller*) 
		      (request-query-value "s" req)))
     then (illegal-access req ent)
	    
     else (with-http-response (req ent)
	    (with-http-body (req ent)
	      (html (:head (:title "Chat Setup"))
		    (:body (:h1 "Chat Setup")
			   
			   (if* (controllers *master-controller*)
			      then (html (:h2 "Existing Chat Controllers")
					 (display-chat-controllers 
					  (controllers *master-controller*))))
			   
			   (:h2 "New Chat Controller")
			   " This page is used to create a chat controller which "
			   "then can be use to create chats."
			   " Just fill out the form below and click on submit "
			   " and you'll be taken to a new controller page. "
			   ((:form :action "new-controller"
				   :method "POST")
			    ((:input :type "hidden"
				     :name "s" 
				     :value (secret-key *master-controller*)))
			    ((:input :type "text"
				     :name "name"
				     :size 30
				     :maxlength 30))
			    "Your Name"
			    :br
		    
			    ((:input :type "text"
				     :name "controllername"
				     :size 30
				     :maxlength 30))
			    "Name for this collection of chats"
			    :br
		   
		    
			    ((:input :type "submit")))))))))


(defun display-chat-controllers (controllers)
  ;; display a table of chat controllers
  (html 
   ((:table :border "1" :cellspacing 1 :cellpadding 3)
    ((:tr :bgcolor "#9999ff")
     (:th "Owner Name")
     (:th "Collection Name")
     (:th "Link"))
    (dolist (controller controllers)
      (html (:tr (:td (:princ-safe (owner-name controller)))
		 (:td (:princ-safe (controller-name controller)))
		 (:td ((:a :href (format nil "controller?~a"
					 (controller-query-string 
					  controller)))
		       "Go To Page"))))))))
		      
(defun do-first-setup-page (req ent)
  ;; called when setup-chat is done for the first time 
  ;; gives the special url that can be used by the chat superadmin
  ;; to give chat controllers to others
  
  (publish-chat-links)
  
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html
       (:html
	(:head (:title "First Setup"))
	(:body (:h1 "First Setup")
	       "This is the first access to this chat setup and you "
	       "are now the chat super-adminstrator."
	       " This "
	       ((:a href 
		    (format nil "setup-chat?s=~a"
			    (secret-key *master-controller*)))
		"link")
	       " will take you to a page where you can create chat"
	       "controller who then can create chats"
	       " Once you follow the link to the page be sure to bookmark "
	       " the page since this will be the only way to "
	       " exert your superadminstrator powers.")))))
  
  )
	       



(defun new-controller (req ent)
  
  (if* (or (not (eq (request-method req) :post))
	   (not (equal (secret-key *master-controller*) 
		       (request-query-value "s" req))))
     then ; someone's playing around
	  (return-from new-controller
	    (ancient-link-error req ent)))
	    
  (with-http-response (req ent)
    (let ((query (request-query req)))
      (let ((controller 
	     (new-chat-controller
	      :owner-name (cdr (assoc "name" query :test #'equalp))
	      :controller-name (cdr (assoc "controllername" query 
					   :test #'equalp))
	      :secret-key (make-unique-string))))
	(mp:with-process-lock ((master-lock *master-controller*))
	  (push controller (controllers *master-controller*)))

	(dump-existing-chat *chat-home*)
	(with-http-body (req ent)
	  (html 
	   (:html
	    (:head (:title "Created New Controller"))
	    (:body
	     "A new controller page has been created, go to "
	     ((:a :href (format nil "controller?~a"
				(controller-query-string 
				 controller)))
	      "here")
	     " to see the page"))))))))

(defun existing-controller (req ent)
  ;; when an owner visits his control page
  (let ((controller (controller-from-req req)))
    (if* (or (null controller)
	     (not (equal (secret-key controller)
			 (cdr (assoc "s" (request-query req) 
				     :test #'equalp)))))
       then (ancient-link-error req ent)
       else (with-http-response (req ent)
	      (with-http-body (req ent)
		(display-controller-page controller))))))
  


(defun display-controller-page (controller)
  ;; display the html for the controller page
  (html 
   (:html 
    (:head (:title "Controller for " 
		   (:princ-safe (controller-name controller))))
    (:body 
     (:h1 "Controller for " 
	  (:princ-safe (controller-name controller)))
     (:h2 "Owner is " (:princ-safe
		       (owner-name controller)))
     (if* (null (chats controller))
	then (html (:h2 "There are no chats defined yet"))
	else (display-chat-list (chats controller) t))
		       
     ((:form :action 
	     (concatenate 'string
	       "create-chat?"
	       (controller-query-string controller))
			       
	     :method "POST")
      :hr
      (:h2 "Create New Chat")
      ((:input :type "text"
	       :name "name"
	       :size 30)
       "  Enter Name for Chat")
      :br
      ((:input :type "text"
	       :name "filename"
	       :value (format nil "chat-~a.txt" (make-unique-string))
	       :size 30))
       "  File where messages are stored"
      :br
      ((:input :type "submit"
	       :value "Create Chat")))))))
			       
		       
	   


(defun display-chat-list (chats owner-p)
  ;; display the characteristics of the chats in a table
  (html ((:table :border "1" :cellspacing 1 :cellpadding 3)
	 ((:tr :bgcolor "#9999ff")
	  (:th "Chat name")
	  (:th "State")
	  (:th "Link")
	  (if* owner-p
	     then (html (:th "Owner Link")))
	  )
	 (dolist (chat chats)
	   (html (:tr
		  (:td (:princ-safe (chat-name chat)))
		  (:td (:princ-safe (chat-state chat)))
		  (:td
		   ((:a :href (concatenate 'string
				"chat?"
				(chat-query-string chat)))
		    "Go to Chat"))
		  (if* owner-p
		     then (html (:td
				 ((:a :href (concatenate 'string
					      "chat?"
					      (chat-owner-query-string chat)))
				  "Go to Chat as owner"))))))))))
    
(defun new-chat-controller (&key owner-name controller-name secret-key)
  ;; create a new chat controller object
  (let (ustring)
    
    ; create a unique string to indentify this controller
    (loop
      (setq ustring (make-unique-string))
      (with-mp-locked-controller (*master-controller*)
	(if* (not (member ustring 
			  (ustrings *master-controller*)
			  :test #'equal))
	   then (push ustring (ustrings *master-controller*))
		(return))))
    
    (let ((controller (make-instance 'chat-controller
			:owner-name owner-name
			:controller-name controller-name
			:secret-key secret-key
			:ustring ustring
			:controller-uri (compute-controller-uri ustring)
			:controller-query-string
			(format nil "u=~a&s=~a" 
				ustring
				secret-key))))
      controller)))

      
      
(defun compute-controller-uri (ustring)
  (format nil "controller?u=~a" ustring))


(defun make-unique-string ()
  ;; make a unique string that's not one of the other strings
  ;; want it to around five characters long
  
  (let ((time (get-universal-time)))
    ; randomize things
    (dotimes (i (logand time #xf)) (random 10))
    (dotimes (i (logand time #x1f)) (random 10))
    (setq time (logxor time (random 4342211881376)))
    (setq time (logxor time (random
			     (load-time-value
			      (get-universal-time)))))
    ; make sure it's at least 8 digits base 26
    (if* (< time #.(expt 26 8))
       then (incf time #.(expt 26 8)))
    ;
    (string-downcase (format nil "~26r" time))))



(defun create-chat (req ent)
  ;; create a new chat for the given controller
  (let ((controller (controller-from-req req)))
    (if* (or (null controller)
	     (not (equal (secret-key controller)
			 (request-query-value "s" req))))
       then (ancient-link-error req ent)
       else (let (ustring)
	      (loop
		(setq ustring (make-unique-string))
		(with-mp-locked-controller (controller)
		  (if* (not (member ustring (ustrings controller) 
				    :test #'equal))
		     then (push ustring (ustrings controller))
			  (return))))
      
	      (let ((chat (make-new-chat controller
					 :name (request-query-value "name" req)
					 :filename 
					 (request-query-value "filename" req)
					 :ustring ustring)))
		(with-mp-locked-controller (controller)
		  (push chat (chats controller)))
		(dump-existing-chat *chat-home*)
		(with-http-response (req ent)
		  (with-http-body (req ent)
		    (display-controller-page controller))))))))

(defun ancient-link-error (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html "This link is ancient and won't work any more"))))


(defun controller-from-req (req)
  ;; locate controller named by request
  (let ((ustring (request-query-value "u" req)))
    (if* ustring
       then (dolist (controller (controllers *master-controller*))
	      (if* (equal ustring (ustring controller))
		 then (return controller))))))

(defun chat-from-req (req)
  ;; find the chat object given the req
  (let ((controller (controller-from-req req)))
    (if* controller
       then (let ((chat-ustring (cdr (assoc "c" (request-query req)
					    :test #'equalp))))
	      (if* chat-ustring
		 then (dolist (chat (chats controller))
			(if* (equal chat-ustring (ustring chat))
			   then (return chat))))))))

(defun user-from-req (req)
  ;; find the user object from this request
  (let ((val (request-query-value "x" req)))
    (if* val
       then (let ((user (user-from-ustring val)))
	      (if* (and user (equal (user-cookie user) 
				    (get-chat-cookie req)))
		 then user)))))




(defun user-from-ustring (ustring)
  ;; find user object based on unique string
  (find ustring (users *master-controller*)
	:key #'user-ustring :test #'equal))

(defun user-from-pstring (ustring)
  ;; find user object based on unique string
  (find ustring (users *master-controller*)
	:key #'user-pstring :test #'equal))

(defun users-from-ustring (ustring)
  ;; ustring may be a comma separated value
  (let (res)
    (dolist (usr (net.aserve::split-on-character ustring #\,))
      (let ((u (user-from-ustring usr)))
	(if* u then (pushnew u res :test #'eq))))
    (nreverse res)))


(defun users-from-pstring (ustring)
  ;; ustring may be a comma separated value
  (let (res)
    (dolist (usr (net.aserve::split-on-character ustring #\,))
      (let ((u (user-from-pstring usr)))
	(if* u then (pushnew u res :test #'eq))))
    (nreverse res)))

(defun user-from-handle (handle)
  ;; locate the user object given the handle
  (find handle (users *master-controller*)
	:key #'user-handle :test #'equal))

(defun make-new-chat (controller &key name filename ustring)
  ;; make a new chat object
  (let ((secret-key (make-unique-string)))
    (make-instance 'chat 
      :name name
      :ustring ustring
      :filename filename
      :secret-key secret-key
      :chat-query-string (format nil "u=~a&c=~a"
				 (ustring controller)
				 ustring)
      :chat-owner-query-string 
      (format nil "u=~a&c=~a&s=~a"
	      (ustring controller)
	      ustring
	      secret-key)
      :secret-key secret-key)))




(defun get-chat-cookie (req)
  (cdr (assoc "aschat" (get-cookie-values req) :test #'equal)))

(defun set-chat-cookie (req cookie)
  (set-cookie-header req :name "aschat"
		     :value cookie
		     :expires :never))

		     
; chat frames:
;
;  chattop 
;  chatviewers chatenter chatcontrol

(defun chat (req ent)
  ;; generate the chat frames
  (format t "start chat~%") (force-output)
  (let ((chat (chat-from-req req))
	(user (user-from-req req))
	(qstring))

    (if* user then (setf (user-time user) (get-universal-time)))
    
    ; do redirect check
    (if* (null user)
       then ; do not logged in check
	    (if* (redir-check req ent chat t)
	       then (return-from chat)))

    ; now the logged in or not logged in check
    (if* (redir-check req ent chat nil)
       then (return-from chat))
    
    
    
    
	    
    
    (if* *chat-hook*
       then (if* (funcall *chat-hook* req ent)
	       then (return-from chat)))
    
    
    (if* (null chat)
       then (ancient-link-error req ent)
       else (setq qstring 
	      (add-lurk
	       req
	       (add-secret req
			   (add-user req (chat-query-string chat)))))
	    
	    (with-http-response  (req ent)
	      (with-http-body (req ent)
		(html 
		 (:html
		  (:head (:title "chat - "
				 (:princ-safe (chat-name chat)))
			 )
		  
		  ((:frameset :rows "*,160")
		   ((:frame :src 
			    (format nil "chattop?~a&count=~d&secs=~d&hitbut=did"
				    qstring
				    *default-count*
				    *default-secs*)
			    :name "chattop")
		    ((:frameset :cols 
				(if* user 
				   then "15%,*,20%"
				   else "*,20%"))
		     (if* user 
			then (html ((:frame :src
					    (concatenate 'string
					      "chatviewers?"
					      qstring)))))
		     ((:frame :src
			      (concatenate 'string
				"chatenter?"
				qstring)
			      :name "chatenter"))
		     ((:frame :src
			      (concatenate 'string
				"chatcontrol?"
				qstring))))
		    (:noframes
		     "This chat program requires a browser that supports frames"
		     ))))))))))




	      
	
    
(defun add-user (req current-string)
  ;; if a user has been specified in the chat
  ;; the add it's x string to the current string
  (let ((val (request-query-value "x" req)))
    (if* val
       then (format nil "~a&x=~a" current-string val)
       else current-string)))

(defun add-secret (req current-string)
  ;; if a secret string has been defined then add it onto the 
  ;; current string
  (let ((val (request-query-value "s" req)))
    (if* val
       then (format nil "~a&s=~a" current-string val)
       else current-string)))

(defun add-reverse (req current-string)
  ;; if a reverse value has been defined then add it onto the 
  ;; current string
  (let ((val (request-query-value "rv" req)))
    (if* val
       then (format nil "~a&rv=~a" current-string val)
       else current-string)))

(defun add-lurk (req current-string)
  ;; if a lurk has been defined then add it onto the 
  ;; current string
  (let ((val (request-query-value "z" req)))
    (if* val
       then (format nil "~a&z=~a" current-string val)
       else current-string)))

(defun chattop (req ent)
  ;; put out the top part of the chat
  (let* ((chat (chat-from-req req))
	 (user (user-from-req req))
	 (is-owner
	  (equal (and chat (secret-key chat)) 
		 (request-query-value "s" req)))
	 (qstring))
    
    (if* (null chat)

       then (return-from chattop (ancient-link-error req ent)))

    ; do redirect check
    (if* (null user)
       then ; do not logged in check
	    (if* (redir-check req ent chat t)
	       then (return-from chattop)))

    ; now the logged in or not logged in check
    (if* (redir-check req ent chat nil)
       then (return-from chattop))


    (let ((delete (request-query-value "y" req)))
      (if* delete
	 then (delete-chat-message chat 
				   (compute-integer-value delete)
				   is-owner
				   (and user
					(user-handle user))
				   )))
    
    (let ((upgrade (request-query-value "b" req)))
      (if* upgrade
	 then (let ((user (user-from-ustring upgrade)))
		(if* user 
		   then (setf (user-level user) 1)
			(dump-existing-chat *chat-home*)))))
    
    (let* ((count (or (compute-integer-value
		       (request-query-value "count" req))
		      10))
	   (secs  (or (compute-integer-value
		       (request-query-value "secs" req))
		      0)))
      
      (if* (not (equal "385" (request-query-value "z" req)))
	 then (track-viewer chat user req))

      (if* user
	 then 
	      (if* (zerop (user-time user))
		 then (setf (user-time user) (get-universal-time)))
      
      
	      (if* (equal (request-query-value "hitbut" req) "did")
		 then ; user hit button in the chatcontrol frame
		      (setf (user-time user) (get-universal-time))
		 else ; test to see if time has expired
		      (if* (> (- (get-universal-time) (user-time user))
			      *idle-timeout*)
			 then (do-idle-timedout req ent
						(format nil
							"chat?~a"
							(add-lurk
							 req
							 (add-secret
							  req
							  (add-user
							   req
							   (chat-query-string chat))))))
			      (return-from chattop))))
      
      (with-http-response (req ent :timeout 500)
	(setq qstring 
	  (format nil "~a&count=~d&secs=~d"
		  (add-lurk
		   req
		   (add-reverse 
		    req
		    (add-secret 
		     req
		     (add-user 
		      req 
		      (chat-query-string chat)))))
		  count 
		  secs))
	(with-http-body (req ent)
	  (html 
	   (:html
	    (:head
	     (:title "chat frame")
	     (if* (and secs (> secs 0))
		then ; setup for auto refresh
		     (html ((:meta :http-equiv "Refresh"
				   :content 
				   (format nil "~d;url=chattop?~a"
					   secs
					   qstring)))))
	      
	     ((:body :if* *background-image*
		     :background *background-image*
		     :if* (not *background-image*)
		     :bgcolor *top-frame-bgcolor*
		     :text *top-frame-font-color*
		     :link *top-frame-link-color*
		     :vlink *top-frame-vlink-color*
		     :alink *top-frame-alink-color*
		     )
	      (if* (or (null secs) (zerop secs))
		 then ; indicate frozen
		      (html (:center (:b ((:font :color "green")
					  "--*-- Frozen --*--")))
			    :br))
		     
	      (show-chat-info chat count 
			      (not (equal "1" (request-query-value
					       "rv"
					       req)))
			      (if* user then (user-handle user))
			      (if* is-owner then qstring)
			      (format nil "~a&count=~d&secs=~d"
				      (add-lurk
				       req
				       (add-reverse 
					req
					(add-user 
					 req 
					 (chat-query-string chat))))
				      count 
				      secs)))))))))))

		     
(defun chatenter (req ent)
  ;;
  ;; this is the window where you enter the post and your handle.
  ;;
  (let* ((chat (chat-from-req req))
	 (user (user-from-req req))
	 (pp (or (request-query-value "pp" req) "*")) ; who to send to
	 (ppp (request-query-value "ppp" req)) ; add a user to the dest
	 (purl (request-query-value "purl" req))
	 (kind :multiline)
	 (to-users (users-from-pstring pp))
	 (qstring))
    (if* (null chat)
       then (return-from chatenter 
	      (ancient-link-error req ent)))
    
    (let* ((body (request-query-value "body" req))
	   (handle (request-query-value  "handle" req)))
	   
      (setq qstring 
	(add-secret req
		    (add-user req
			      (chat-query-string chat))))
      

      (if* user
	 then (setf (user-time user) (get-universal-time))
	      
	      (if* ppp
		 then ; add this user
		      
		      (setq pp (setf (user-to-users user)
				 (concatenate 'string
				   (or (user-to-users user) "")
				   ","
				   ppp)))
		      (setq to-users (users-from-pstring pp))
	       elseif (equal pp "*")
		 then (setf (user-to-users user) nil)
		 else (setf (user-to-users user) pp)))

      ; do redirect check
      (if* (null user)
	 then ; do not logged in check
	      (if* (redir-check req ent chat t)
		 then (return-from chatenter)))

      ; now the logged in or not logged in check
      (if* (redir-check req ent chat nil)
	 then (return-from chatenter))

	      
      (if* (and body (not (equal "" body)))
	 then ; user added content to the chat
	      (add-chat-data chat req handle body user to-users purl nil))
      
      (with-http-response (req ent)
	(with-http-body (req ent)
	  (html
	   (:html
	    (:head
	     :newline
	     "<script>
<!--
function sf(){document.f.body.focus();}
// -->
</script>
"
	     :newline
	     )
	    ((:body :onload "sf()"
		    :bgcolor 
		    (if* to-users 
		       then *bottom-frames-private*
		       else *bottom-frames-bgcolor*))
	     ((:form :action (concatenate 'string
			       "chatenter?"
			       qstring)
		     :method "POST"
		     :name "f"
		     )
	      (:center
	       (if* (eq kind :multiline)
		  then (html
			(:table
			 (:tr
			  (:td
			   (:center
			    ((:input :name "send"
				     :value "Send"
				     :type "submit"))
			    " "
			    (if* user
			       then (html 
				     (if* to-users
					then (html 
					      "Private msg from: ")
					else (html "From: "))
				     (:b 
				      (:princ-safe
				       (user-handle user)))
				     " to "
				     (:b
				      (if* to-users
					 then (dolist (to-user to-users)
						(html
						 (:princ-safe
						  (user-handle
						   to-user))
						 " "
						 ))
					 else (html "all"))))
				    
			       else (html
				     "Your Name" 
				     ((:input :name "handle"
					      :type "text"
					      :tabindex 3
					      :size 20
					      :value (if* handle then handle else "")))))
			    " -- " 
			    ((:a :href (format nil "chatlogin?~a" qstring)
				 :target "_top")
			     "Login")
			    " -- &nbsp;&nbsp;&nbsp;"
			    
			    ((:input :name "send"
				     :tabindex 2
				     :value "Send"
				     :type "submit"))
			    (if* user
			       then (html " "
					  ((:a :href (format nil "chatenter-pic?~a&pp=~a" 
							     qstring pp))
					   "upload picture")))
			    )))
			 (:tr
			  (:td 
			   ((:textarea :name "body"
				       :tabindex 1
				       :cols 50
				       :rows 5))
			   ((:input :type "hidden"
				    :name "pp"
				    :value pp))))
			 (:tr
			  (:td
			   (:center
			    ((:input :type "text"
				     :size 40
				     :maxlength 100
				     :value (or purl "")
				     :name "purl"))
			    " Picture Url")))))
		  else ; single line
		       (html 
			(:table
			 (:tr
			  ((:td :colspan 1)
			   (:center
			    "Your Name" 
			    ((:input :name "handle"
				     :type "text"
				     :size 20
				     :value (if* handle then handle else "")))
			    ((:input :name "send"
				     :value "Post Message"
				     :type "submit")))))
			 (:tr 
			  (:td
			   ((:input :type "text"
				    :name "body"
				    :size 60
				    :maxsize 10000)))))))))
	      
	     ))))))))

(defun chatenter-pic (req ent)
  ;;
  ;; this is the window where you enter the post and your handle.
  ;; this version is for when you post a picture
  ;
  (let* ((chat (chat-from-req req))
	 (user (user-from-req req))
	 (pp (or (request-query-value "pp" req) "*")) ; who to send to
	 (ppp (request-query-value "ppp" req)) ; add a user to the dest
	 (to-users (users-from-pstring pp))
	 (qstring))
    (if* (or (null chat) (null user))
       then (return-from chatenter-pic
	      (ancient-link-error req ent)))
    
    (if* (eq (request-method req) :post)
       then (process-incoming-file chat req user to-users)
	    (setf (request-method req) :get)
	    (return-from chatenter-pic (chatenter req ent)))
    
    (let* ()		    
	   
      (setq qstring 
	(add-secret req
		    (add-user req
			      (chat-query-string chat))))
      

      ;; user must be true
      (setf (user-time user) (get-universal-time))
	      
      (if* ppp
	 then ; add this user
		      
	      (setq pp (setf (user-to-users user)
			 (concatenate 'string
			   (or (user-to-users user) "")
			   ","
			   ppp)))
	      (setq to-users (users-from-pstring pp))
       elseif (equal pp "*")
	 then (setf (user-to-users user) nil)
	 else (setf (user-to-users user) pp))


      ; now the logged in or not logged in check
      (if* (redir-check req ent chat nil)
	 then (return-from chatenter-pic))

	      
      
      (with-http-response (req ent)
	(with-http-body (req ent)
	  (html
	   (:html
	    ((:body :bgcolor 
		    (if* to-users 
		       then *bottom-frames-private*
		       else *bottom-frames-bgcolor*))
	     ((:form :action (concatenate 'string
			       "chatenter-pic?"
			       (format nil "~a&pp=~a" qstring pp))
		     :method "POST"
		     :enctype "multipart/form-data"
		     )
	      (:center
	       
	       (html
		(:table
		 (:tr
		  (:td
		   (:center
		    ((:input :name "send"
			     :value "Send"
			     :type "submit"))
		    " "
		    
		    (html 
		     (if* to-users
			then (html 
			      "Private msg from: ")
			else (html "From: "))
		     (:b 
		      (:princ-safe
		       (user-handle user)))
		     " to "
		     (:b
		      (if* to-users
			 then (dolist (to-user to-users)
				(html
				 (:princ-safe
				  (user-handle
				   to-user))
				 " "
				 ))
			 else (html "all"))))
		    " -- " 
		    ((:a :href (format nil "chatlogin?~a" qstring)
			 :target "_top")
		     "Login")
		    " -- &nbsp;&nbsp;&nbsp;"
			    
		    ((:input :name "send"
			     :tabindex 2
			     :value "Send"
			     :type "submit")))))
		 (:tr
		  (:td
		   "The picture file to upload (click Browse):" :br
		   ((:input :type "file"
			    :name "thefile"
			    :size 40
			    :value "*.jpg")))
		  (:tr
		   (:td 
		    "Add commments about your picture" :br
		    ((:textarea :name "comments"
				:tabindex 1
				:cols 50
				:rows 3)))))))))))))))))



(defparameter *pic-counter* 0)

(defun process-incoming-file (chat req user to-users)
  (let ((comment "") type upload-pic)
    (loop
      (multiple-value-bind (kind name filename content-type)
	  (parse-multipart-header
	   (get-multipart-header req))
	(case kind
	  (:eof (return))
	  (:data ; must be contents
	   (if* (equal name "comments")
		   then (setq comment (get-all-multipart-data req))))
	  (:file 
	   (let ((contents (get-all-multipart-data req :type :binary
						   :limit 2000000)))
	     ; see if it ends in .jpg or .gif
	     (if* (member content-type '("image/jpeg" 
					 "image/pjpeg"
					 "image/jpg")
			  :test #'equal)
		then (setq type "jpg")
	      elseif (equal content-type "image/gif")
		then (setq type "gif")
		else (format t "uploaded type of ~s is ~s~%" 
			     filename content-type))
	     (if* type
		then (let ((filename (concatenate 'string
				       (format nil "~x" (incf *chat-picname* 23))
				       "."
				       type)))
		       (with-open-file (p (concatenate 'string
					    *chat-home-pics*
					    "/"
					    filename)
					:direction :output
					:if-exists :supersede)
			 (write-sequence contents p))
		       (setq upload-pic
			 `(:span :br ((:img :src ,(format nil "/chatpics/~a" filename))) :br))))))
	  (t (get-all-multipart-data req :limit 1000)))))
  
    (if* (or (and  comment (> (length comment) 0))
	     upload-pic)
       then (add-chat-data chat req nil comment user to-users nil
			   upload-pic))))
    
    
  
  
		   
		     
#+ignore	
(defun process-incoming-file (chat req user to-users)
  ;; read the multipart file, publish it
  ;; create the message referencing it, and then add that to the chat.
  (let (file content-type comment upload-pic)
    (loop (let ((h (get-multipart-header req)))
	    (if* (null h) then (return))
	    (pprint h)(force-output)
	    (let ((name (cdr
			 (assoc "name" 
				(cddr (assoc :param
					     (cdr (assoc :content-disposition h :test #'eq))
					     :test #'eq))
				:test #'equal))))
	      (if* (equal name "thefile")
		 then ; the file we're uploading
		      (setq content-type (cadr (assoc :content-type h :test #'eq)))
		      (setq file (read-multipart-guts req))
		      
	       elseif (equal name "comments")
		 then ; read the comments
		      (setq comment (octets-to-string (read-multipart-guts req)))
		 else (read-multipart-guts req)))))
    
    ;; now we may have a picture
    (if* (and file content-type)
       then ; we have guts
	    (let ((picname (format nil "/chatpix/~d~d" 
				   (get-universal-time) (incf *pic-counter*))))
	      (publish-multi :path picname
			     :content-type content-type
			     :items (list (list :binary file)))
	      
	      (setq upload-pic
		`(:span :br ((:img :src ,picname)) :br))
	      
	      (setq comment (or comment ""))))
    
    (if* (and comment (> (length comment) 0))
       then (add-chat-data chat req nil comment user to-users nil
			   upload-pic))
    
	      
    ))
			
		      
			
(defun read-multipart-guts (req)			
  (let ((buffer (make-array 40000 :element-type '(unsigned-byte 8)))
	(buffs)
	(total-size 0))
    (loop (let ((count (get-multipart-sequence req buffer)))
	    (if* count
	       then (incf total-size count)
		    (push (subseq buffer 0 count) buffs)
	       else (return))))
			
    (setq buffer (make-array total-size :element-type '(unsigned-byte 8)))
    (let ((count  0))
      (dolist (buf (nreverse buffs))
	(replace buffer buf :start1 count)
	(incf count (length buf))))
    buffer))
			  
		      
	      
	      
      
      
    
  
			      
(defun do-idle-timedout (req ent goback)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html (:head (:title "timed out"))
	    (:body "due to inactivity you have been timed out"
		   :br
		   (if* goback
		      then (html "To return to the chat click "
				 ((:a :href goback
				      :target "_top")
				  "here"))))))))


(defun chatcontrol (req ent)
  ; control the updating
  (let ((chat (chat-from-req req))
	(qstring))
    
    (if* (null chat)
       then (return-from chatcontrol (ancient-link-error req ent)))
    
    (let* ((count (or (request-query-value "count" req) *default-count*))
	   (secs  (or (request-query-value "secs" req) *default-secs*)))
      
      (setq qstring 
	(add-lurk
	 req
	 (add-secret req
		     (add-user req (chat-query-string chat)))))
      (with-http-response (req ent)
	(with-http-body (req ent)
	  (html
	   (:html
	    ((:body :bgcolor *bottom-frames-bgcolor*)
	     ((:form :action
		     (concatenate 'string
		       "chattop?"
		       qstring
		       )
		     :target "chattop"
		     :method "POST")
	      ((:input :type "text"
		       :name "secs"
		       :size 3
		       :value secs)
	       "Seconds")
	      :br
	      ((:input :type "text"
		       :name "count"
		       :size 4 
		       :value count))
	      "messages"
	      :br
	      ((:input :type "checkbox"
		       :name "rv"
		       :value "1"))
	      " Reversed"
	      :br

	      ; use to distinguish a call to chattop from
	      ; a user button click from a refresh one
	      
	      ((:input :type "hidden"
		       :name "hitbut"
		       :value "did"))
	      
	      ((:input :type "submit"
		       :name "submit"
		       :value "Update Messages"))
	      
	      
	      ; optional chat transcript link
	      (if* *offer-transcript*
		 then (html
		       :br :hr
		       ((:a :href (format nil "chattranscript?~a" qstring)
			    :target "_blank")
			"View transcript.")))
	      )))))))))
		     

(defun compute-integer-value (string)
  ;; compute the string to a number
  ;; if there's any junk return nil if we haven't seen good stuff yet
  (and (stringp string)
       (let ((ans 0))
	 (do ((i 0 (1+ i))
	      (end (length string))
	      (seen-digit)
	      )
	     ((>= i end)
	      (if* seen-digit 
		 then ans
		 else nil))
	   (let ((digit (- (char-code (schar string i)) #.(char-code #\0))))
	     (if* (<= 0 digit 9)
		then (setq ans (+ (* ans 10) digit))
		     (setq seen-digit t)
		else (if* seen-digit
			then (return ans)
			else (return nil))))))))

  
    
(defun add-chat-data (chat req handle body user to-users purl upload-pic)
  ;; chat is chat object
  ;; req is http request object
  ;; handle is handle typed by user (only matters  if user not logged in)
  ;; body is the string that's the posting
  ;; user is the user object if user is logged in
  ;; to-user is nil or the string naming the private message receipient
  ;; purl is picture url value
  (multiple-value-bind (prefix link) 
      (if* (and (stringp purl) (not (equal "" purl)))
	 then (scan-for-http purl))
    (declare (ignore prefix))

    
    
    (if* (stringp to-users) 
       then ; just one user, turn it into a list
	    (setq to-users (list to-users)))
    
    (if* link
       then (if* (and (consp link)
		      (consp (car link))
		      (eq :img (caar link)))
	       thenret  ; valid image url
	       else (setq link nil)))
    
    (if* (null link)
       then (setq link upload-pic))
    
    (let* ((cvted-body (html-chk-string-to-lhtml body))
	   (ipaddr (socket:remote-host
		    (request-socket req)))
	   (dns (or #+ignore (socket:ipaddr-to-hostname ipaddr)
		    (socket:ipaddr-to-dotted ipaddr)))
	   (ut (get-universal-time))
	 
	   (message 
	    (make-message
	     :number (chat-message-number chat)
	     :ipaddr ipaddr
	     :dns dns
	     :handle (if* user then (user-handle user) else handle)
	     :to (if* to-users
		    then (mapcar #'user-handle to-users)
		    else t)
	     :real (if* user then t else nil)
	     :time (let ((time (compute-chat-date ut)))
		     (if* *message-id-hook*
			then (funcall *message-id-hook* time)
			else time))
	     :ut   ut
	     :body (if* link
		      then (cons link cvted-body)
		      else cvted-body))))
				     
      (mp:with-process-lock ((chat-message-lock chat))
	(add-chat-message chat message)))))

(defun compute-chat-date (ut)
  ; return string to use as time for this message
  ; quick hack - hardwire in pdt
  (multiple-value-bind (sec min hour day month)
      (decode-universal-time ut)
    (format nil "~d:~2,'0d:~2,'0d Pacific Time, ~a ~d" hour min sec
	    (month-name month) day
	    )))

(defun month-name (month)
  (svref '#("" "Jan" "Feb" "Mar" "Apr" "May" "June" "July"
	    "Aug" "Sep" "Oct" "Nov" "Dec")
	 month))

(defun add-chat-message (chat message)
  ;; add the message to the messages of the chat.
  ;; assume that we've got the lock to do this.
  (let ((messages (chat-messages chat))
	(message-next (chat-message-next chat)))
	    
    (if* (>= message-next (length messages))
       then ; must grow messages
	    (let ((nmessages (make-array (+ (length messages) 
					    *msg-increment*))))
	      ;; copy only non-deleted messages
	      (let ((to 0))
		(dotimes (i (length messages))
		  (let ((message (svref messages i)))
		    (if* (message-to message)
		       then (setf (svref nmessages to) message)
			    (incf to))))
		(setq message-next to)
		(setf (chat-messages chat) nmessages)
		(setq messages nmessages))))
    (setf (svref messages message-next)  message)
    (setf (chat-message-next chat) (1+ message-next))
    (setf (chat-message-number chat) 
      (1+ (message-number message)))))
  


(defun delete-chat-message (chat messagenum is-owner handle)
  ;; remove the  message numbered messagenumy setting the to field to nil
  (mp:with-process-lock ((chat-message-lock chat))
    (let ((message (find-chat-message chat messagenum)))
      (if* (and message
		(or is-owner ; owner can remove all
		    (and handle
			 (equal handle (message-handle message)))))
	 then (setf (message-to message) nil)
	      (push messagenum (chat-messages-deleted chat))))))

(defun delete-chat-message-by-message (chat message)
  ;; remove the given message by setting the to field to nil
  (mp:with-process-lock ((chat-message-lock chat))
    (if* message
       then (setf (message-to message) nil)
	    (push (message-number message)
		  (chat-messages-deleted chat)))))

(defun find-chat-message (chat number)
  ;; find the message with the given number
  (let* ((messages (chat-messages chat))
	 (len (and messages (chat-message-next chat)))
	 (bottom 0)
	 (top (and len (1- len)))
	 )
    (if* messages
       then ; find first message
	    ; do binary search
	    #+ignore (format t "Want message ~s~%" number)
	    (loop
	      (if* (> bottom top)
		 then (return nil) ; no message found
		 else (let ((try (truncate (+ top bottom) 2)))
			 #+ignore (format t "try ~d (~d -> ~d)~%"
				try bottom top)
			(let ((message (svref messages try)))
			  (if* message
			     then #+ignore (format t "try msg num is ~s~%" 
					  (message-number message))
				  (if* (eql (message-number message) number)
				     then  #+ignore (format t "**found~%")
					  (return message)
				   elseif (< (message-number message)
					     number)
				     then ; in top quadrant
					  (setq bottom 
					    (max (1+ bottom) try))
				     else (setq top 
					    (min (1- top) try)))
			     else (warn "Null chat message at ~d"
					try)
				  (return nil)))))))))
		      

(defun show-message-p (message handle)
  ;; return true if this message should be shown to someone with
  ;; the handle 'handle' 
  ;;
  ;; handle is non-nil iff this person is logged in.
  ;;
  ;; message-to is nil if this is a deleted message in which case
  ;; no one should see it.
  ;;
  (or 
   ; show everyone
   (eq t (message-to message)) 
   
   ; message specifically to handle
   (and handle (member handle (message-to message) :test #'equal))
   
   ; message from 'handle' and to at least one person
   (and (equal (message-handle message) handle)
	(message-to message))))


(defun find-nth-message (messages start handle count)
  ;; count down from start to find the index of the counth
  ;; message visible to handle.  return that index
  
  (assert (> count 0))
  
  (loop
    (if* (<= start 0) then (return 0))
    (let ((message (svref messages start)))
      (if* (show-message-p message handle)
	 then (if* (<= (decf count) 0) then (return start)))
      (decf start))))


(defun compute-chat-statistics (chat)
  ;; compute information about this chat
  (mp::with-process-lock ((chat-message-lock chat))
    (let ((messages (chat-messages chat))
	  (message-next (chat-message-next chat)))
      (let ((total-messages 0)
	    (private-messages 0))
	(dotimes (i message-next)
	  (let ((message (svref messages i)))
	    (if* message
	       then (if* (message-to message)
		       then (incf total-messages)
			    (if* (not (eq t (message-to message)))
			       then (incf private-messages))))))
      
	(values total-messages private-messages)))))



(defun set-saved-chat-messages (chat count)	
  ;; set to save approx 'count' messages
  (mp::with-process-lock ((chat-message-lock chat))
    (let ((messages (chat-messages chat))
	  (message-next (chat-message-next chat)))
      ; count backwards until we've passed 'count' messages
      (do ((i (1- message-next) (1- i)))
	  ((< i 0)
	   ; no messages to remove
	   nil)
	
	(let ((message (svref messages i)))
	  (if* message
	     then (if* (<= count 0)
		     then ; remove all messages at this point
			  (delete-chat-message-by-message  chat message)
		     else (if* (message-to message)
			     then (decf count)))))))))
	
		  

  
		  
		    
      
  
  
  
(defun show-chat-info (chat count recent-first handle ownerp qstring)
  ;; show the messages for all and those private ones for handle
  ;; handle is only non-nil if this is a legit logged in handle
  (let ((message-next (chat-message-next chat))
	(messages (chat-messages chat))
	(first-message)
	(last-message)
	(nth-message)
	(message-increment)
	)
    
    ;; if the person is not logged in then minimize the count 
    (if* *restrict-messages*
       then (if* (null handle) 
	       then (setq count (min 5 count))
	       else (let ((user (user-from-handle handle)))
		      (if* (and user (null (user-level user)))
			 then (setq count (min 10 count))))))
    
    
    (if* (zerop message-next)
       then (html (:b "There are no messages in this chat"))
     elseif (<= count 0)
       thenret ; nothing to show
       else ; starting at the end find the counth message
	    (setq nth-message
	      (find-nth-message messages (1- message-next) handle count))
	    
	    (if* recent-first
	       then (setq first-message (1- message-next)
			  last-message nth-message
			  message-increment -1)
	       else (setq last-message (1- message-next)
			  first-message nth-message
			  message-increment 1))

	    (if* recent-first
	       then ; tag most recent message
		    (html ((:div :id "recent"))))
	    
	    (do ((i first-message (+ i message-increment)))
		(nil)
	    
	      (let ((message (svref messages i)))
		(if* (null message)
		   then (warn "null message at index ~d" i)
		 elseif (if* (or (eq t (message-to message))
				 (member handle (message-to message)
					 :test #'equal))
			   then ;; to everyone or us
				nil	 ; don't skip
			 elseif (and (equal (message-handle message)
					    handle)
				     (message-to message))
			   then ;; from us to someone, anyone
				nil ; don't skip
			   else t ; skip
				)
		   thenret ; skip this message
		 elseif (eq *show-style* 1)
		   then
			(html :newline 
			      ((:font :color 
				      (if* (consp (message-to message))
					 then *private-font-color*
					 else *public-font-color*))
			       
			       (:b (:i (:princ-safe (message-handle message))))
			       (if* (not (message-real message))
				  then (html " (unverified)"))
			       ((:font :size 1)
				" -- ("
				(:princ (message-time message))
				(if* (consp (message-to message))
				   then (html " to: "
					      (:princ-safe (message-to message))))
				")")
			      
			       " <!-- "
			       (:princ (message-number message)) 
			       " "
			       (:princ (message-dns message))
			       " --> "
			       (if* (or ownerp
					(and (message-real message)
					     (equal (message-handle message)
						    handle)))
				  then (html
					((:a :href 
					     (format nil "chattop?y=~a&~a"
						     (message-number message)
						     (or ownerp qstring)))
					 "Delete")))
			       
			       (if* ownerp
				  then
				       (let ((user (and (message-real message)
							(user-from-handle
							 (message-handle message)))))
					 (if* (and user (null (user-level user)))
					    then ; can upgrade if desired
						 (html "  "
						       ((:a :href
							    (format nil 
								    "chattop?b=~a&~a"
								    (user-ustring
								     user)
								    ownerp))
							" Upgrade ")))))
			       :newline
			       :br
			       (html-print-list (message-body message)
						*html-stream*)
			       :br)
			      :newline)
		   else 
			(html
			 :newline
			 ((:table :border 1 :width "100%" :frame "box")
			  (:tr
			   ((:td :width "10%")
			    (:b (:i (:princ-safe (message-handle message))))
			    :br
			    ((:font :size 1) (:princ (message-time message)))
			    " <!-- "
			    (:princ (message-number message)) 
			    " "
			    (:princ (message-dns message))
			    " --> "
			    )
			   (:td
			    (html-print-list (message-body message)
					     *html-stream*)))))))
		 
	      (if* (eql i last-message) then (return)))
	    
	    (if* (not recent-first)
	       then ; tag most recent message
		    (html ((:div :id "recent")))))
    
    (if* (null handle)
       then (html :br 
		  ((:table :border 1)
		   (:tr
		    (:td
		     (if* *restrict-messages*
			then (html
			     
			      "In order to have access to the other facilities of this chat, "
			      "such as private messaging and viewing the history of messages "
			      "you must log in, by clicking on the Login link below.")
			else (html
			     
			      "In order to have access to the other facilities of this chat, "
			      "such as private messaging "
			      "you must log in, by clicking on the Login link below.")
			     ))))))
	     
    ))




(defun chatlogin (req ent)
  ;; response function for /chatlogin?ucstring"
  (let ((chat (chat-from-req req)))
    (if* chat
       then (do-chat-login req ent 
			   (add-secret req
				       (add-user req
						 (chat-query-string chat)))
			   nil)
       else (ancient-link-error req ent))))


(defun do-chat-login (req ent qstring failure)
  ;; put up a login screen for this chat
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html
       (:html
	(:head (:title "Login to Chat"))
	(:body
	 (if* failure
	    then (html (:blink 
			(:b "Error: " (:princ-safe failure) :br :br))))
	 
	 (:h1 "Login as an existing user")
	 ((:form :action (format nil "chatlogincurrent?~a" qstring)
		 :target "_top"
		 :method "POST")
	  ((:input :type "text" :size "15" :name "handle")) "Your Handle" :br
	  ((:input :type "password" :size "15" :name "password")) "Your password" :br
	  ((:input :type "submit" :name "submit" :value "login")))
	 :hr
	 (:h1 "Create a new account and login")
	 ((:form :action (format nil "chatloginnew?~a" qstring)
		 :method "POST")
	  ((:input :type "text" :size "15" :name "handle")) "Your Handle" :br
	  ((:input :type "password" :size "15" :name "password")) "Your password" :br
	  ((:input :type "password" :size "15" :name "password2")) "Type your password again" :br
	  ((:input :type "submit" :name "submit" :value "New Account")))))))))


(defun chat-login-current (req ent)
  ;; handle a post to  chatlogincurrent 
  
  ; guard aginst
  (if* (not (eq :post (request-method req)))
     then (return-from chat-login-current (ancient-link-error req ent)))
  
  (let ((chat (chat-from-req req))
	(handle (request-query-value "handle" req))
	(password (request-query-value "password" req)))
    ; locate the handle
    (let ((user (find handle (users *master-controller*)
		      :key #'user-handle :test #'equalp)))
      (if* (null user)
	 then (return-from chat-login-current
		(do-chat-login req ent 
			       (add-secret req
					   (add-user req
						     (chat-query-string chat)))
			       "That user name is unknown")))
      (if* (not (equal password (user-password user)))
	 then (return-from chat-login-current
		(do-chat-login req ent 
			       (add-secret req
					   (add-user req
						     (chat-query-string chat)))
			       "That password is incorrect")))
      
      ; worked, do a redirect
      (with-http-response (req ent :response *response-moved-permanently*)
	(setf (reply-header-slot-value req :location)
	  (format  nil "chat?~a&x=~a"
		   (add-secret req 
			       (chat-query-string chat))
		   (user-ustring user)))
	(set-chat-cookie req (user-cookie user))
	(with-http-body (req ent)
	  (html "redirect"))))))

      
      
(defun chatloginnew (req ent)
  ;; response function when a new user is being defined
  
  
  (if* (not (eq :post (request-method req)))
     then (return-from chatloginnew (ancient-link-error req ent)))
  
  (let* ((handle (request-query-value "handle" req))
	 (password (request-query-value "password" req))
	 (password2 (request-query-value "password2" req))
	 (chat (chat-from-req req))
	 (qstring (and chat (chat-query-string chat))))
    
    (if* (null chat)
       then (return-from chatloginnew (ancient-link-error req ent)))
    
    
    (if* (equal "" password)
       then (return-from chatloginnew
	      (do-chat-login req ent qstring "No password given")))
    
    (if* (not (equal password password2))
       then (return-from chatloginnew
	      (do-chat-login req ent qstring "Passwords don't match")))
    
    (dolist (user (users *master-controller*))
      (if* (equalp (user-handle user) handle)
	 then (return-from chatloginnew
		(do-chat-login req ent qstring "That user name exists"))))
    
    ; add new user
    (let (new-ustring new-pstring new-cookie)
      (mp:with-process-lock ((master-lock *master-controller*))
	(loop 
	  (setq new-ustring (make-unique-string))
	  (setq new-pstring (make-unique-string))
	  (if* (dolist (user (users *master-controller*) t)
		 (if* (or  (equal new-ustring (user-ustring user))
			   (equal new-ustring (user-pstring user)))
		    then ; already in use
			 (return nil)))
	     then (return)))
	; leave the loop with new-ustring being unique among users
	(push (make-user :handle handle
			 :password password
			 :ustring new-ustring
			 :pstring new-pstring
			 :cookie (setq new-cookie (make-unique-string)))
	      (users *master-controller*))
	(dump-existing-chat *chat-home*))
      
      ; go to the chat as the user
      (with-http-response (req ent :response
			       *response-moved-permanently*)
	(setf (reply-header-slot-value req :location)
	  (format nil "chat?~a&x=~a" 
		  (add-secret req qstring) new-ustring))
	(set-chat-cookie req new-cookie)
	(with-http-body (req ent) 
	  "move to the chat")))))
							   
    
    
      
    
    
    

    
    
    
  
      
      
    
(defun html-chk-string-to-lhtml (form)
  ;; look for {< to start html and >} to end it.
  ;;
  (multiple-value-bind (match full first quoted last)
      (match-regexp "\\(.*\\){<\\(.*\\)>}\\(.*\\)" form :newlines-special nil)
    (declare (ignore full))
    (if* match
       then ; contains embedded html
	    (append (string-to-lhtml first)
		    (list quoted)
		    (string-to-lhtml last))
       else (string-to-lhtml form))))      
	
	
	 
       
(defun string-to-lhtml (form)
  ;; convert the string to a list of lhtml forms
  ;;
  ;; break the text into lines separated by :br's.
  ;; look for http://'s in the lines and replace them with
  ;; links or inline images
  
  (let (res (i 0) (start 0) (max (length form)))
    (loop
      ; we go around one last time when (eq i max) in which
      ; case we pretent there's a linefeed at the end
      (let ((ch 
	     (if* (>= i max)
		then #\linefeed
		else (schar form i))))
	
	(if* (or (eq ch #\return) (eq ch #\linefeed))
	   then ; end of line
		(if* (not (eq start i))
		   then (let ((line (subseq form start i)))
			  (loop
			    (if* (or (null line) 
				     (equal line ""))
			       then (return))
			    (multiple-value-bind (pref link rest)
				(scan-for-http line)
				(if* link
				   then (push (de-angle pref) res)
					(push link res)
					(setq line rest)
				   else (push (de-angle pref) res)
					(setq line nil))))))
		(push :br res)
		
		(incf i)
		(if* (and (eq ch #\return)
			  (< i max)
			  (eq (schar form i) #\linefeed))
		   then (incf i) ; past following linefeed
			)
		
		(setq start i)
	   else (incf i))
	    
	(if* (> i max) then (return))))
    (nreverse res)))


(defun de-angle (str)
  ;; replace < and > in strings by their entity tags
  (if* (find #\< str)
     then (setq str (replace-regexp str "<" "&lt;")))
  (if* (find #\> str)
     then (setq str (replace-regexp str ">" "&gt;")))
  str)


(defun scan-for-http (line)
  ;; look for http:// in the line and if found return it as
  ;; a link or image lhtml
  ;;
  
  (multiple-value-bind (ok whole)
      (match-regexp "http://[^ 	>]+" line :return :index)
    (if* ok
       then ; found one
	    (let (http)
	      (setq http (subseq line (car whole) (cdr whole)))
	    
	      (values
	       ; value 1 -- everything before the http
	       (subseq line 0 (car whole)) 
	       
	       ; value 2 - the link 
	     
	       (do ((i (1- (length http)) (1- i)))
		   ((< i 0)
		    ; didn't find a . .. set to a link
		    `((:a :href ,http :target "_blank") (:princ-safe ,http)))
		 
		 (if* (eq (schar http i) #\.)
		    then ; found a period
			 (let ((type (subseq http (1+ i))))
			   (if* (member type '("gif" "jpg" "png")
					:test #'equalp)
			      then ; an image link
				   (return 
				     `((:img :src ,http)))
			      else (setq i 0) ; stop search
				   ))))
	       
	       ; value 3 - the rest of the line
	       (subseq line (cdr whole))))
       else line)))


;; chatmaster page

(defun chatmaster (req ent)
  ;; commands
  ;;  
  (let* ((chat (chat-from-req req))
	 (is-owner
	  (equal (and chat (secret-key chat)) 
		 (request-query-value "s" req)))
	 (act (request-query-value "act" req)))
    (if* (not is-owner)
       then (illegal-access req ent)
	    (return-from chatmaster nil))
    
    (if* (equal act "set-msg-count")
       then ; set the message count to the given value
	    (let ((val (compute-integer-value
			(request-query-value "val" req))))
	      (if* (>= val 0)
		 then (format t " set msg count to ~d~%" val)
		      (set-saved-chat-messages chat val)))
     elseif (equal act "set-idle")
       then (let ((val (compute-integer-value
			(request-query-value "val" req))))
	      (if* (> val 0)
		 then (format t " set idle timeout ~d mins~%" val)
		      (setq *idle-timeout* (* 60 val))))
     elseif (equal act "set-redirects")
       then (set-redirects req chat))
    
    (if* (equal "yes" (request-query-value "shut" req))
       then ; shutting down the chat
	    (with-http-response (req ent)
	      (with-http-body (req ent)
		(html (:body (:h1 "Shutdown")))))
	    (mp:process-run-function "killer" #'shutdown-chat)
	    (sleep 10)
	    (exit 0)
	    (return-from chatmaster nil))
	    
    
    (multiple-value-bind (total-messages private-messages)
	(compute-chat-statistics chat)
    
      (with-http-response  (req ent)
	(with-http-body (req ent)
	  (html (:html
		 (:head (:title "Chat Master"))
		 (:body
		  (:h2 "Statistics")
		  "There are " (:princ total-messages) 
		  " messages in the chat and " 
		  (:princ private-messages)
		  " of those are private"
		  :br
		  
		  ((:form :method "POST")
		   "Reduce the number of stored messages to "
		   ((:input :type "text" :name "val" :value total-messages
			    :size 6))
		   ((:input :type "hidden" :name "act" :value "set-msg-count"))
		   ((:input :type "submit" :value "do it")))
		  :br
		  
		  
		  
		  (:h2 "Control")
		  ((:form :method "POST")
		   "Idle timeout in minutes: "
		   ((:input :type "text"
			    :name "val"
			    :value (truncate *idle-timeout* 60)
			    :size 4))
		   ((:input :type "hidden"
			    :name "act"
			    :value "set-idle"))
		   ((:input :type "submit"
			    :value "Set It")))
		  :br

		  ((:form :method "POST")
		   ((:input :type "checkbox"
			    :name "shut"
			    :value "yes"))
		   "Shut down the chat "
		   ((:input :type "submit"
			    :value "really kill it")))
		  :br

		  (show-redirects chat)
		   
		  )))))
    
      )))


(defun show-redirects (chat)
  ;; display redirect dialog
  (html 
   (:h2 "Redirects")
   
   ((:form :method "POST")
    ((:input :type "hidden"
	     :name "act"
	     :value "set-redirects"))
    ((:table :border 1)
	
     ; show current ones
     (dolist (redir (chat-redirects chat))
       (html
	:newline
	(:tr
	 (:td
	  ((:input :type "text" :size 50 
		   :name (redir-info-name redir)
		   :value (redirect-info redir)))
	  :br
	  "ipaddr: " 
	  ((:input :type "text" :size 50
		   :name (redir-ipaddr-name redir)
		   :value (socket:ipaddr-to-dotted
			   (redirect-ipaddr redir))))
	  ", mask bits: " 
	  ((:input :type "text" :size 4
		   :name (redir-maskbits-name redir)
		   :value (redirect-maskbits redir)))
	  :br
	  "to: " 
	  ((:input :type "text" :size 50
		   :name (redir-to-name redir)
		   :value (redirect-to redir)))
	  
	  :br
	  ((:input :type "checkbox"
		   :if* (redirect-before redir)
		   :checked "checked"
		   :name (redir-before-name redir)
		   :value "xxxx"))
	  "applies only to people not logged on"
	   
	  
	  :br
	  ((:input :type "radio"
		   :name (redir-state-name redir)
		   :value "active"
		   :if* (redirect-active redir) :checked "checked"))
	  "On, "
	  ((:input :type "radio"
		   :name (redir-state-name redir)
		   :value "disabled"
		   :if* (not (redirect-active redir)) :checked "checked"))
	  "Disabled, "
	  ((:input :type "radio"
		   :name (redir-state-name redir)
		   :value "disrem"))
	     
	  "Disable then remove"
	  :br
	  "this rule used " (:princ-safe (redirect-use redir)) " time(s)"
	  :br
	  ((:input :type "checkbox"
		   :name (redir-change-name redir)
		   :value 0)) 
	  ((:font :color "red") "Make Changes")
	  ))))
	
     ; show new one
     (html
      :newline
      (:tr
       (:td
	"info: " ((:input :type "text" :size 50 :name "newinfo"))
	:br
	"ipaddr:" ((:input :type "text" :size 50 :name "newipaddr"))
	", mask bits" ((:input :type "text" :size 4 :name "newmask"))
	:br
	"redirect to: " ((:input :type "text" :size 50 :name "newto"))
	:br
	((:input :type "checkbox"
		 :name "newredirbefore"
		 :value 0))
	"applies only to people not logged on"
	:br
	((:input :type "checkbox" :name "newdo" :value "1")) 
	((:font :color "red") "Add this entry")))))
       
    ((:input :type "submit" :value "Change Redirects")))))


(defun set-redirects (req chat)
  ;; change the redirect information for this chat

  (let (changed)
    (dolist (redir (chat-redirects chat))
      (if* (request-query-value (redir-change-name redir) req)
	 then ; something changed in here
	      (set-redir-info chat 
			      redir
			      req
			      (redir-info-name redir)
			      (redir-ipaddr-name redir)
			      (redir-maskbits-name redir)
			      (redir-to-name redir)
			      (redir-before-name redir)
			      (redir-state-name redir))
	      (setq changed t)))
    (if* (request-query-value "newdo" req)
       then ; add a new entry
	    (let ((redir (make-redirect)))
	      (setf (redirect-index redir)
		(incf (redirect-counter chat)))
	      (set-redir-info chat 
			      redir
			      req
			      "newinfo"
			      "newipaddr"
			      "newmask"
			      "newto"
			      "newredirbefore"
			      "newxxxxxx")
	      (setf (redirect-active redir) t)
	    
	      (setf (chat-redirects chat)
		(append (chat-redirects chat) (list redir)))
	    
	      (setq changed t)
	    
	      ))
  
    (if* changed then (dump-existing-chat *chat-home*))))

	

(defun set-redir-info (chat redir req ninfo nipaddr nmask nto nbefore nstate)
  (setf (redirect-info redir) (request-query-value ninfo req))
  (let ((ipaddr (or 
		 (ignore-errors (socket:lookup-hostname
				 (request-query-value nipaddr req)))
		 0)))
    (setf (redirect-ipaddr redir) ipaddr))
  
  (let ((maskbits (or (compute-integer-value
		   (request-query-value nmask req))
		  32)))
    (setf (redirect-maskbits redir) maskbits)
    (setf (redirect-mask redir)
      (logand #xffffffff (ash -1 (- 32 maskbits))))
    )
  
  (setf (redirect-to redir) (request-query-value nto req))
  (setf (redirect-before redir) (request-query-value nbefore req))
  
  (let ((state (request-query-value nstate req)))
    (if* (equal state "active")
       then (setf (redirect-active redir) t)
     elseif (equal state "disabled")
       then (setf (redirect-active redir) nil)
     elseif (equal state "disrem")
       then ; eliminate
	    (setf (chat-redirects chat)
	      (delete redir (chat-redirects chat))))))
  
  
	
  
    
    
    
  
  

;; generate temp names for form objects

(defun redir-info-name (redir)
  (format nil "~a-info" (redirect-index redir)))

(defun redir-ipaddr-name (redir)
  (format nil "~a-ipaddr" (redirect-index redir)))

(defun redir-maskbits-name (redir)
  (format nil "~a-maskbits" (redirect-index redir)))

(defun redir-before-name (redir)
  (format nil "~a-before" (redirect-index redir)))

(defun redir-to-name (redir)
  (format nil "~a-to" (redirect-index redir)))
			   
(defun redir-change-name (redir)
  (format nil "~a-change" (redirect-index redir)))
	 
(defun redir-state-name (redir)
  (format nil "~a-state" (redirect-index redir)))  
  



	     
;; Chat archiver
;;
;; The chat archiver stores chat info to files

(let (last-master-controller)
(defun start-chat-archiver (master-controller)
  (and t (if* (not (eq master-controller last-master-controller))
     then ; we've already started the process
	  (setq last-master-controller master-controller)
	  (mp:process-run-function "chat archiver"
				   #'chat-archiver master-controller)))))

(defun chat-archiver (master-controller)
  (let ((sleep-time 30)
	(did-work))
    (loop
      (if* (not (eq *master-controller* master-controller))
	 then ; chat has been restarted, let this process die
	      (return))

      (format t "Chat archiver awoken~%")
      (setq did-work nil)
    
      ; write out the data
      (dolist (controller (controllers master-controller))
	(dolist (chat (chats controller))
	  (mp:with-process-lock ((chat-message-lock chat))
	    (format t " arch ~d   num ~d~%"
		    (chat-message-archive chat)
		    (chat-message-number  chat))
	    (if* (or (< (chat-message-archive chat)
			(chat-message-number  chat))
		     (chat-messages-deleted chat))
	       then ; must do work
		    (archive-chat chat)
		    (setq did-work t)))))

      ; adjust archive time so that we sleep longer when 
      ; the chat is inactive.
      (if* did-work 
	 then (setq sleep-time 30)
	 else (setq sleep-time (min (+ sleep-time 30) 
				    (* 30 60) ; 30 minutes
				    )))
      
      (format t "Chat archiver going to sleep~%")
      (sleep sleep-time))))



(defun find-index-of-message (chat number)
  ;; find index of message 'number' or the first one after that
  (let ((messages (chat-messages chat))
	(message-next (chat-message-next chat)))
    (do ((i (1- message-next) (1- i)))
	((< i 0) 0)
      (let* ((message (svref messages i))
	     (num (message-number message)))
	(if* (and num
		  (< num number))
	   then (return (1+ i))
	 elseif (eql num number)
	   then (return i))))))
    
(defun archive-chat (chat)
  ;; write out new messages for this chat
  ;; we are inside a process lock for this chat's message lock
  ;; so we can alter the fields at will
  (let ((messages (chat-messages chat))
	(message-next (chat-message-next chat))
	(message-archive (chat-message-archive chat)))
    
    ; we have to find the location of the 
    ; message-archive message
    (if* (> message-next 0)
       then ; it better be greater than 0 since to be zero
	    ; would be no messages stored
	    
	    ; locate the message numbered message-archive
	    (let ((start-to-save 
		   (find-index-of-message chat message-archive)))
	    
	      (with-open-file (archive-port (archive-filename chat)
			       :direction :output
			       :if-exists :append
			       :if-does-not-exist :create
			       ;:external-foramt :utf-8
			       )
		(do ((i start-to-save (1+ i)))
		    ((>= i message-next))
		  (if* (eq t (message-to (svref messages i)))
		     then ; a public message, archive it
			  (pprint (svref messages i) archive-port))
		  )
		(if* (chat-messages-deleted chat)
		   then (pprint `(:delete ,@(chat-messages-deleted chat))
				archive-port)
			(setf (chat-messages-deleted chat) nil)))
	      
	      (setf (chat-message-archive chat) 
		(1+ (message-number (svref messages (1- message-next)))))))))

(defun archive-filename (chat)
  (format nil "~a/~a" *chat-home* (chat-filename chat)))


	
(defmethod set-style ((style color-style))
  (setq *top-frame-bgcolor*     (color-style-bgcolor style)
	*top-frame-font-color*  (color-style-font-color style)
	*public-font-color*     (color-style-font-color style)
	*top-frame-vlink-color* (color-style-vlink-color style)
	*top-frame-link-color*  (color-style-link-color style)
	*top-frame-alink-color* (color-style-alink-color style)))

(if* (not (boundp '*top-frame-bgcolor*))
   then (set-style *normal-style*))

;; for franz chats uncomment this since some people like this style better
;(set-style *white-style*)
;(setq *quick-return-path* "/xyzzy")
;-------- 

(defun chat-transcript (uc-string filename)
  ;; generate a transcript of the chat with the given uc-string
  ;; to the given filename
  ;
  ; find chat
  (let* ((query-alist (form-urlencoded-to-query uc-string))
	 (u (cdr (assoc "u" query-alist :test #'equalp)))
	 (c (cdr (assoc "c" query-alist :test #'equalp))))
    
    (let ((chat 
	   (dolist (controller (controllers *master-controller*))
	     (if* (equal u (ustring controller))
		then (return
		       (dolist (chat (chats controller))
			 (if* (equal c (ustring chat))
			    then (return chat))))))))
      (if* (null chat)
	 then (error "can't find chat with uc-string ~s" uc-string))
      
      (with-open-file (*html-stream* filename :direction :output
		       :if-exists :supersede
		       ;:external-format :utf-8
		       )
	(html 
	 (:head
	  (:title "Transcript of "
		  (:princ-safe (chat-name chat))))
	 (:body
	  (:h1 "Transcript of "
	       (:princ-safe (chat-name chat)))
	  (show-chat-info chat (chat-message-next chat) nil nil nil nil)))))))
		     
		     
			 
;;  viewer tracking

(defun track-viewer (chat user req)
  ;; note that this user/req has read the postings for this chat
  (let* ((time (get-universal-time))
	 (viewers (chat-viewers chat))
	 (ipaddr (socket:remote-host (request-socket req)))
	 (empty-ent))

    
    (mp::with-process-lock ((viewers-lock viewers))
      
      ;; scan list of viewers.
      ;; set emptyent to the first viewent with a null time, thus meaning
      ;;  it's a free entry
      ;; if an entry already exists for this user or ipaddr use it
      (dolist (viewent (viewers-list viewers)
		; not there yet
		(if* empty-ent
		   then ; replace old one
			(setf (viewent-time empty-ent) time
			      (viewent-user empty-ent) user
			      (viewent-ipaddr empty-ent) ipaddr
			      (viewent-hostname empty-ent) nil)
		   else 
			(push (setq empty-ent 
				(make-viewent :time time
					      :user user
					      :ipaddr ipaddr))
			      (viewers-list viewers))
			))
	(if* user
	   then (if* (eq user (viewent-user viewent))
		   then ; update this one
			(setf (viewent-time viewent) time)
			(if* (not (eql ipaddr (viewent-ipaddr viewent)))
			   then ; hmm, changed ipaddr
				(setf (viewent-ipaddr viewent) ipaddr
				      (viewent-hostname viewent) nil))
			(return))
	   else ; ipaddr test
		(if* (and (null (viewent-user viewent))
			  (eql ipaddr (viewent-ipaddr viewent)))
		   then (setf (viewent-time viewent) time)
			(return)))
	(if* (null (viewent-time viewent))
	   then (if* (null empty-ent)
		   then (setf empty-ent viewent))
	 elseif (> (- time (viewent-time viewent)) *max-active-time*)
	   then ; this one is too old
		(setf (viewent-time viewent) nil)
		(if* (null empty-ent)
		   then (setq empty-ent viewent)))))))

(defun chatviewers (req ent)
  ;; display page of chat viewers (except us)
  (let* ((chat (chat-from-req req))
	 (user (user-from-req req))
	 (time (get-universal-time))
	 (is-owner
	  (equal (and chat (secret-key chat)) 
		 (request-query-value "s" req)))
	 (qstring)
	 (viewers)
	 (idletime)
	 )
    (if* (null chat)
       then (return-from chatviewers (ancient-link-error req ent)))
    
    (if* (and user (zerop (user-time user)))
       then (setf (user-time user) (get-universal-time)))
    
    (if* (> (setq idletime (- (get-universal-time) (user-time user)))
	    (+ 10 *idle-timeout*))
       then (do-idle-timedout req ent nil)
	    (return-from chatviewers))
    (setq qstring
      (add-secret req
		  (add-user req
			    (chat-query-string chat))))
    (setq viewers (chat-viewers chat))

    (setq idletime (truncate idletime 60)) ; cvt to minutes
    
    (with-http-response (req ent)
      (with-http-body (req ent)
	(html
	 (:html
	  ((:meta :http-equiv "Refresh"
		  :content
		  (format nil "30;url=chatviewers?~a" qstring)))
	  (:body
	   
	   ((:font :size 2)
	    ((:a :href (concatenate 'string
			 "chatenter?pp=*&" qstring)
		 :target "chatenter"
		 )
	     "Send to All")
	    :hr
	    :newline
	    (:pre
	     (mp::with-process-lock ((viewers-lock viewers))
	       (dolist (viewent (viewers-list viewers))
		 (let* ((vtime (viewent-time viewent))
			(vuser (viewent-user viewent))
			(alive-time (if* vtime then (- time vtime)))
			(idle-time (if* vuser
				      then (- time (or (user-time vuser) 0))
				      else 0)))
		   
		  
		 (if* (and alive-time
			   (> alive-time *max-active-time*))
		    then (setq vtime nil)
			 (setf (viewent-time viewent) nil))

		 ; cvt to minutes
		 (setq idle-time (min 120 (truncate idle-time 60)))
		   
		 (if* vtime
		    then ; fill in the hostname if it's not there yet
			 #+(version>= 6 0)
			 (if* (null (viewent-hostname viewent))
			    then (setf (viewent-hostname
					viewent)
				   (socket::dns-query 
				    (viewent-ipaddr viewent)
				    :type :ptr
				    :repeat 1
				    :timeout 0)))
					      
			 (if* (not (eq vuser user))
			    then ; list this one
				 (if* vuser
				    then 
					 (html
					  ; link to add a user
					  ((:a :href
					       (format nil
						       "chatenter?ppp=~a&~a"
						       (user-pstring vuser)
						       qstring)
					       :target "chatenter")
					   "(+)")
					  
					  " "
					       
					  
					 ; link to create a private message 
					  ((:a :href 
					       (format nil
						       "chatenter?pp=~a&~a"
						       (user-pstring vuser)
						       qstring)
					       :target "chatenter"
					       )
					   (:princ-safe
					    (user-handle vuser))))
					  
				    else ; ip address
						    
					 (html
					  (:princ
					   (or (viewent-hostname viewent)
					       (socket:ipaddr-to-dotted
						(viewent-ipaddr viewent))))))
				 (html 
				  " ("
				  (:princ (- time vtime))
				  "s)")
				   
				 (if* (> idle-time 2)
				    then (html
					  " [idle: "
					  (:princ idle-time)
					  "m] "))

				 (if* (or *show-machine-name-to-all* 
					  is-owner)
				    then ; name then ip address
					 (html 
					  " @" 
					  (:princ-safe 
					   (or (viewent-hostname viewent)
					       (socket:ipaddr-to-dotted
						(viewent-ipaddr viewent))))))
				 (html :newline)))))))))))))))
						
					  


(defun chattranscript (req ent)
  (let* ((chat (or (chat-from-req req)
		   (return-from chattranscript (ancient-link-error req ent))))
	 (title (format nil "full transcript of chat ~a as of ~a"
			(chat-name chat) (compute-chat-date
					  (get-universal-time)))))

    (with-http-response (req ent)
      (with-http-body (req ent)
	(html
	 (:html
	  (:title (:princ-safe title))
	  (:body
	   (:h1 (:princ-safe title))
	   (let ((*top-frame-bgcolor* "#xffffff") ; white
		 (*public-font-color* "#x000000") ; black
		 )
	     (show-chat-info chat (chat-message-next chat) nil 
			     "bogushandle" nil nil))
	   )))))))

(defun redir-check (req ent chat before)
  ;; check if this request should be redirected
  ;; before is true if we are in the before login state
  (let ((redirects (chat-redirects chat)))
    (if* redirects
       then (let ((ipaddr (socket:remote-host (request-socket req))))
	      (dolist (redir redirects)
		(if* (and (redirect-active redir)
			  (eq before (redirect-before redir))
			  (eql (logand (redirect-ipaddr redir)
				       (redirect-mask redir))
			       (logand ipaddr
				       (redirect-mask redir))))
		   then ; a match!
			(incf (redirect-use redir))
			(with-http-response (req ent
						 :response 
						 *response-moved-permanently*)
			  (setf (reply-header-slot-value req :location) 
			    (redirect-to redir))
			  (with-http-body (req ent)
			    (html "redirect")))
			(return t)))))))
			     
  
      
			
    

    
    
    
    
    
    
;;;;; chat test code
;;
;;

(defun block-test (testers &rest args)
  (dotimes (i testers)
    (let ((name (format nil "tester-~d" i))
	  (delay (max 1 (random 10))))
      
      (mp:process-run-function name
			       #'(lambda ()
				   (apply #'test-chat
					  :name name
					  :delay delay
					  args))))))
					  


				 
  
  


(defun test-chat (&key uc-string
		       (count 100) 
		       (reads 5) 
		       (delay 2)
		       (name "dummy1")
		       (machine "localhost")
		       (port  8000)
		       (protocol :http/1.1))
  (let ((reader-url
	 (format nil "http://~a:~d/chattop?~a&~a"
		 machine
		 port
		 uc-string
		 (query-to-form-urlencoded
		  `(("count" . 10)
		    ("secs" . 5)))))
	(post-url 
	 (format nil "http://~a:~d/chatenter?~a"
		 machine
		 port
		 uc-string)))
		  
  (dotimes (i count)
    ; post first
    (let ((message (format nil "message ~d from ~a~%" i name)))
      (do-http-request post-url
	:protocol protocol
	:method :post
	:query `(("secs" . 5) ; not used
		 ("handle" . ,name)
		 ("body" . ,message)))
      (sleep delay)
      (dotimes (i reads)
	; read it now
	(do-http-request reader-url
	  :method :get
	  :protocol protocol)
	(sleep delay))))))
      
	  
;;; fix up old chats

(defun fixupchat ()
  (setf (users *master-controller*) (nreverse (users *master-controller*)))
  (dolist (user (users *master-controller*))
    (setf (user-ustring user) (make-unique-string))
    (setf (user-pstring user) (make-unique-string)))
  (dump-existing-chat *chat-home*)
  )

  
    
