;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; publish.cl
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2016 Franz Inc, Oakland, CA - All rights reserved.
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

;; Description:
;;   publishing urls

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(in-package :net.aserve)


(defclass entity ()
  ;; an object to be published
  ;; host and port may be nil, meaning "don't care", or a list of
  ;; items or just an item
  ((host 
    :initarg :host
	 :initform nil
	 :reader host)
   (port :initarg :port
	 :initform nil
	 :reader port)
   (path :initarg :path
	 :reader path
	 :initform "--unspecified-path--"
	 )
   (location :initarg :location
	     :reader location)
   (prefix :initarg :prefix
	   :initform nil
	   :reader prefix)
   (last-modified :initarg :last-modified
		  :accessor last-modified
		  :initform nil ; means always considered new
		  )
   
   ; ut string format for last-modified cached here.
   (last-modified-string :initarg :last-modified-string
			 :accessor last-modified-string
			 :initform nil)
   
   (format :initarg :format  ;; :text or :binary
	   :initform :text
	   :reader  entity-format)
   
   (content-type :initarg :content-type
		 :reader content-type
		 :initform nil)

   ; can be a single object or a list of objects
   (authorizer  :initarg :authorizer  
		:accessor entity-authorizer
		:initform nil)
   
   ; if not nil then the timeout to be used in a with-http-response
   ; for this entity
   (timeout  :initarg :timeout
	     :initform nil
	     :accessor entity-timeout)
   
   ; property list for storing info on this entity
   (plist    :initarg :plist
	     :initform nil
	     :accessor entity-plist)
   
   ; function of 3 args (req ent extra) called between
   ; with-http-request and with-http-body for entity types
   ; where the user has no control (i.e. non function types)
   (hook     :initarg :hook
	     :initform nil
	     :accessor entity-hook)
   
   ; cons holding extra headers to send with this entity
   (headers  :initarg :headers
	     :initform nil
	     :accessor entity-headers)

   ; if true then aserve is permitted (but not required) to
   ; send this entity back compressed.  There is also a 
   ; global compress switch in the wserver object that must be true
   ; in order for compression to be done
   (compress :initarg :compress
	     :initform t
	     :accessor entity-compress)
   
   ; extra holds random info we need for a particular entity
   (extra    :initarg :extra  :reader entity-extra)
   
   ;; should aserve auto-send a 100 Continue response for
   ;; this entity if an 'Expect: 100-continue' header is
   ;; in the request.
   (will-handle-expect-continue
    :initarg :will-handle-expect-continue
    :initform nil
    :reader entity-will-handle-expect-continue)
   ))


(defclass file-entity (entity)
  ;; a file to be published
  (
   (file  :initarg :file :reader file)
   (contents :initarg :contents :accessor contents
	     :initform nil)
   (cache-p 
    ;; true if the contents should be cached when accessed
    :initarg :cache-p
    :initform nil
    :accessor cache-p)
     
   ))


(defclass computed-entity (entity)
  ;; entity computed each time it's called
  ((function :initarg :function :reader entity-function)))

(defvar *dummy-computed-entity* 
    ;; needed when intercepting and sending a computed entity in place
    ;; of the entity being published
    (make-instance 'computed-entity))


(defclass access-file-mixin ()
  ;; slots needed if you want to use access files during
  ;; the handling of this entity
  ; if non-nil the name of the file to look for in directories to
  ; personalize the creation of file entities
  ((access-file :initarg :access-file
		:initform nil
		:accessor directory-entity-access-file)
   
   ; internal slot used to cache the files we've read
   ; is a list of
   ; (whole-access-filename last-write-dat cached-value)
   ;
   (access-file-cache :initform nil
		      :accessor directory-entity-access-file-cache)
   ))
  


(defclass directory-entity (entity access-file-mixin)
  ;; entity that displays the contents of a directory
  ((directory :initarg :directory ; directory to display
	      :reader entity-directory)
   (prefix    :initarg :prefix   ; url prefix pointing to ths dir
	      :reader prefix
	      :initform "")
   (recurse   :initarg :recurse	   ; t to descend to sub directories
	      :initform nil
	      :reader recurse)
   
   (cache-p 
    ;; settting for file entities created:
    ;; true if the contents should be cached when accessed
    :initarg :cache-p
    :initform nil
    :accessor cache-p)   
   
   ; list of name of files that can be used to index this directory
   (indexes   :initarg :indexes
	      :initform nil
	      :accessor directory-entity-indexes)
   
   ; filter is nil or a function of   req ent filename info
   ; which can process the request or return nil
   (filter    :initarg :filter
	      :initform nil
	      :accessor directory-entity-filter)
    

   ;: fcn of  req ent realname
   ;  it should create and publish an entity and return it
   (publisher :initarg :publisher
	      :initform nil
	      :accessor directory-entity-publisher)
   
   
   (hidden-index-redirect
    :initform nil
    :initarg  :hidden-index-redirect
    :reader   directory-hidden-index-redirect)
   ))


(defclass special-entity (entity)
  ;; used to hold a certain body we want to always return
  ;; nil means we'll return no body
  ((content :initform nil
	    :initarg :content
	    :reader special-entity-content)))

(setq *not-modified-entity* (make-instance 'special-entity))



;; the multi-entity contains  list of items.  the items can be
;;
;;  atom - assumed to be a namestring or pathname that can be opened
;;  function - function to run to compute a result
;;             function takes req ent last-modified-time

(defclass multi-entity (entity)
  ;; handle multiple files and compute entities
  
  ((items 
    ;; list of multi-item structs
    :initarg :items
    :reader items)
   (content-length :initform 0
		   :accessor multi-entity-content-length))
  )



(defstruct multi-item
  kind	; :file, :function
  data  ; for :file, the filename  for :function the function
  cache ; nil or unsigned-byte 8 array 
  last-modified)



;;-------- locators - objects which find the entity to return

(defclass locator ()
  ((name :initform :unnamed
	 :initarg :name
	 :reader  locator-name)

   ; info is where the locator will likely store data related
   ; to mapping
   (info :initform nil
	 :initarg :info
	 :accessor locator-info)
   
   ; for random extra info
   (extra    :initarg :extra  :reader locator-extra)
   ))


(defclass locator-exact (locator)
  ;; used to map specific uri paths to entities
  ;; the table slot holds the hash table that's used
  ()
  (:default-initargs :info (make-hash-table :test #'equal)))


(defclass locator-prefix (locator)
  ;; use to map prefixes to entities
  ()
  )


;; the info slot of a locator-prefix class is a list of
;; prefix-handler objects, sorted by the length of the path
;; (from longest to smallest).
(defstruct (prefix-handler (:type list))
  path           ;; string which must be the prefix of the url part to match
  host-handlers  ;; list of host-handlers
  )

(defstruct (host-handler  (:type list))
  host	  ;; vhost object to match or  :wild meaning match anything
  entity  ;; entity object to handle this request
  )







  









; we can specify either an exact url or one that handles all
; urls with a common prefix.
;;
;; if the prefix is given as a list: e.g. ("ReadMe") then it says that
;; this mime type applie to file named ReadMe.  Note that file types
;; are checked first and if no match then a filename match is done.
;
(defparameter *file-type-to-mime-type*
    ;; this list constructed by generate-mime-table in parse.cl
    '(("application/EDI-Consent") ("application/EDI-X12")
      ("application/EDIFACT") ("application/activemessage")
      ("application/andrew-inset" "ez") ("application/applefile")
      ("application/atomicmail") ("application/batch-SMTP")
      ("application/beep+xml") ("application/cals-1840")
      ("application/commonground") ("application/cybercash")
      ("application/dca-rft") ("application/dec-dx") ("application/dvcs")
      ("application/eshop") ("application/http") ("application/hyperstudio")
      ("application/iges") ("application/index") ("application/index.cmd")
      ("application/index.obj") ("application/index.response")
      ("application/index.vnd") ("application/iotp") ("application/ipp")
      ("application/isup") ("application/font-tdpfr")
      ("application/mac-binhex40" "hqx")
      ("application/mac-compactpro" "cpt") ("application/macwriteii")
      ("application/marc") ("application/mathematica")
      ("application/mathematica-old") ("application/msword" "doc")
      ("application/news-message-id") ("application/news-transmission")
      ("application/ocsp-request") ("application/ocsp-response")
      ("application/octet-stream" "bin" "dms" "lha" "lzh" "exe" "class" "so"
       "dll" "img" "iso")
      ("application/ogg" "ogg") ("application/parityfec")
      ("application/pdf" "pdf") ("application/pgp-encrypted")
      ("application/pgp-keys") ("application/pgp-signature")
      ("application/pkcs10") ("application/pkcs7-mime")
      ("application/pkcs7-signature") ("application/pkix-cert")
      ("application/pkix-crl") ("application/pkixcmp")
      ("application/postscript" "ai" "eps" "ps")
      ("application/prs.alvestrand.titrax-sheet") ("application/prs.cww")
      ("application/prs.nprend") ("application/qsig")
      ("application/remote-printing") ("application/riscos")
      ("application/rtf" "rtf") ("application/sdp")
      ("application/set-payment") ("application/set-payment-initiation")
      ("application/set-registration")
      ("application/set-registration-initiation") ("application/sgml")
      ("application/sgml-open-catalog") ("application/sieve")
      ("application/slate") ("application/smil" "smi" "smil")
      ("application/timestamp-query") ("application/timestamp-reply")
      ("application/vemmi") ("application/vnd.3M.Post-it-Notes")
      ("application/vnd.FloGraphIt") ("application/vnd.accpac.simply.aso")
      ("application/vnd.accpac.simply.imp") ("application/vnd.acucobol")
      ("application/vnd.aether.imp")
      ("application/vnd.anser-web-certificate-issue-initiation")
      ("application/vnd.anser-web-funds-transfer-initiation")
      ("application/vnd.audiograph") ("application/vnd.businessobjects")
      ("application/vnd.bmi") ("application/vnd.canon-cpdl")
      ("application/vnd.canon-lips") ("application/vnd.claymore")
      ("application/vnd.commerce-battelle") ("application/vnd.commonspace")
      ("application/vnd.comsocaller") ("application/vnd.contact.cmsg")
      ("application/vnd.cosmocaller") ("application/vnd.cups-postscript")
      ("application/vnd.cups-raster") ("application/vnd.cups-raw")
      ("application/vnd.ctc-posml") ("application/vnd.cybank")
      ("application/vnd.dna") ("application/vnd.dpgraph")
      ("application/vnd.dxr") ("application/vnd.ecdis-update")
      ("application/vnd.ecowin.chart")
      ("application/vnd.ecowin.filerequest")
      ("application/vnd.ecowin.fileupdate")
      ("application/vnd.ecowin.series")
      ("application/vnd.ecowin.seriesrequest")
      ("application/vnd.ecowin.seriesupdate") ("application/vnd.enliven")
      ("application/vnd.epson.esf") ("application/vnd.epson.msf")
      ("application/vnd.epson.quickanime") ("application/vnd.epson.salt")
      ("application/vnd.epson.ssf") ("application/vnd.ericsson.quickcall")
      ("application/vnd.eudora.data") ("application/vnd.fdf")
      ("application/vnd.ffsns") ("application/vnd.framemaker")
      ("application/vnd.fsc.weblaunch") ("application/vnd.fujitsu.oasys")
      ("application/vnd.fujitsu.oasys2") ("application/vnd.fujitsu.oasys3")
      ("application/vnd.fujitsu.oasysgp")
      ("application/vnd.fujitsu.oasysprs") ("application/vnd.fujixerox.ddd")
      ("application/vnd.fujixerox.docuworks")
      ("application/vnd.fujixerox.docuworks.binder")
      ("application/vnd.fut-misnet") ("application/vnd.grafeq")
      ("application/vnd.groove-account")
      ("application/vnd.groove-identity-message")
      ("application/vnd.groove-injector")
      ("application/vnd.groove-tool-message")
      ("application/vnd.groove-tool-template")
      ("application/vnd.groove-vcard") ("application/vnd.hhe.lesson-player")
      ("application/vnd.hp-HPGL") ("application/vnd.hp-PCL")
      ("application/vnd.hp-PCLXL") ("application/vnd.hp-hpid")
      ("application/vnd.hp-hps") ("application/vnd.httphone")
      ("application/vnd.hzn-3d-crossword")
      ("application/vnd.ibm.afplinedata") ("application/vnd.ibm.MiniPay")
      ("application/vnd.ibm.modcap") ("application/vnd.informix-visionary")
      ("application/vnd.intercon.formnet")
      ("application/vnd.intertrust.digibox")
      ("application/vnd.intertrust.nncp") ("application/vnd.intu.qbo")
      ("application/vnd.intu.qfx")
      ("application/vnd.irepository.package+xml") ("application/vnd.is-xpr")
      ("application/vnd.japannet-directory-service")
      ("application/vnd.japannet-jpnstore-wakeup")
      ("application/vnd.japannet-payment-wakeup")
      ("application/vnd.japannet-registration")
      ("application/vnd.japannet-registration-wakeup")
      ("application/vnd.japannet-setstore-wakeup")
      ("application/vnd.japannet-verification")
      ("application/vnd.japannet-verification-wakeup")
      ("application/vnd.koan") ("application/vnd.lotus-1-2-3")
      ("application/vnd.lotus-approach") ("application/vnd.lotus-freelance")
      ("application/vnd.lotus-notes") ("application/vnd.lotus-organizer")
      ("application/vnd.lotus-screencam") ("application/vnd.lotus-wordpro")
      ("application/vnd.mcd") ("application/vnd.mediastation.cdkey")
      ("application/vnd.meridian-slingshot") ("application/vnd.mif" "mif")
      ("application/vnd.minisoft-hp3000-save")
      ("application/vnd.mitsubishi.misty-guard.trustweb")
      ("application/vnd.mobius.daf") ("application/vnd.mobius.dis")
      ("application/vnd.mobius.msl") ("application/vnd.mobius.plc")
      ("application/vnd.mobius.txf") ("application/vnd.motorola.flexsuite")
      ("application/vnd.motorola.flexsuite.adsi")
      ("application/vnd.motorola.flexsuite.fis")
      ("application/vnd.motorola.flexsuite.gotap")
      ("application/vnd.motorola.flexsuite.kmr")
      ("application/vnd.motorola.flexsuite.ttc")
      ("application/vnd.motorola.flexsuite.wem")
      ("application/vnd.mozilla.xul+xml") ("application/vnd.ms-artgalry")
      ("application/vnd.ms-asf") ("application/vnd.ms-excel" "xls")
      ("application/vnd.ms-lrm") ("application/vnd.ms-powerpoint" "ppt")
      ("application/vnd.ms-project") ("application/vnd.ms-tnef")
      ("application/vnd.ms-works") ("application/vnd.mseq")
      ("application/vnd.msign") ("application/vnd.music-niff")
      ("application/vnd.musician") ("application/vnd.netfpx")
      ("application/vnd.noblenet-directory")
      ("application/vnd.noblenet-sealer") ("application/vnd.noblenet-web")
      ("application/vnd.novadigm.EDM") ("application/vnd.novadigm.EDX")
      ("application/vnd.novadigm.EXT")
      ("application/vnd.oasis.opendocument.chart" "odc")
      ("application/vnd.oasis.opendocument.database" "odb")
      ("application/vnd.oasis.opendocument.formula" "odf")
      ("application/vnd.oasis.opendocument.graphics" "odg")
      ("application/vnd.oasis.opendocument.graphics-template" "otg")
      ("application/vnd.oasis.opendocument.image" "odi")
      ("application/vnd.oasis.opendocument.presentation" "odp")
      ("application/vnd.oasis.opendocument.presentation-template" "otp")
      ("application/vnd.oasis.opendocument.spreadsheet" "ods")
      ("application/vnd.oasis.opendocument.spreadsheet-template" "ots")
      ("application/vnd.oasis.opendocument.text" "odt")
      ("application/vnd.oasis.opendocument.text-master" "odm")
      ("application/vnd.oasis.opendocument.text-template" "ott")
      ("application/vnd.oasis.opendocument.text-web" "oth")
      ("application/vnd.osa.netdeploy") ("application/vnd.palm")
      ("application/vnd.pg.format") ("application/vnd.pg.osasli")
      ("application/vnd.powerbuilder6") ("application/vnd.powerbuilder6-s")
      ("application/vnd.powerbuilder7") ("application/vnd.powerbuilder7-s")
      ("application/vnd.powerbuilder75")
      ("application/vnd.powerbuilder75-s")
      ("application/vnd.previewsystems.box")
      ("application/vnd.publishare-delta-tree")
      ("application/vnd.pvi.ptid1") ("application/vnd.pwg-xhtml-print+xml")
      ("application/vnd.rapid") ("application/vnd.s3sms")
      ("application/vnd.seemail")
      ("application/vnd.shana.informed.formdata")
      ("application/vnd.shana.informed.formtemplate")
      ("application/vnd.shana.informed.interchange")
      ("application/vnd.shana.informed.package") ("application/vnd.sss-cod")
      ("application/vnd.sss-dtf") ("application/vnd.sss-ntf")
      ("application/vnd.street-stream")
      ("application/vnd.sun.xml.writer" "sxw")
      ("application/vnd.sun.xml.writer.template" "stw")
      ("application/vnd.sun.xml.calc" "sxc")
      ("application/vnd.sun.xml.calc.template" "stc")
      ("application/vnd.sun.xml.draw" "sxd")
      ("application/vnd.sun.xml.draw.template" "std")
      ("application/vnd.sun.xml.impress" "sxi")
      ("application/vnd.sun.xml.impress.template" "sti")
      ("application/vnd.sun.xml.writer.global" "sxg")
      ("application/vnd.sun.xml.math" "sxm") ("application/vnd.svd")
      ("application/vnd.swiftview-ics")
      ("application/vnd.symbian.install" "sis")
      ("application/vnd.triscape.mxs") ("application/vnd.trueapp")
      ("application/vnd.truedoc") ("application/vnd.tve-trigger")
      ("application/vnd.ufdl") ("application/vnd.uplanet.alert")
      ("application/vnd.uplanet.alert-wbxml")
      ("application/vnd.uplanet.bearer-choice-wbxml")
      ("application/vnd.uplanet.bearer-choice")
      ("application/vnd.uplanet.cacheop")
      ("application/vnd.uplanet.cacheop-wbxml")
      ("application/vnd.uplanet.channel")
      ("application/vnd.uplanet.channel-wbxml")
      ("application/vnd.uplanet.list")
      ("application/vnd.uplanet.list-wbxml")
      ("application/vnd.uplanet.listcmd")
      ("application/vnd.uplanet.listcmd-wbxml")
      ("application/vnd.uplanet.signal") ("application/vnd.vcx")
      ("application/vnd.vectorworks")
      ("application/vnd.vidsoft.vidconference") ("application/vnd.visio")
      ("application/vnd.vividence.scriptfile") ("application/vnd.wap.sic")
      ("application/vnd.wap.slc") ("application/vnd.wap.wbxml" "wbxml")
      ("application/vnd.wap.wmlc" "wmlc")
      ("application/vnd.wap.wmlscriptc" "wmlsc")
      ("application/vnd.webturbo") ("application/vnd.wrq-hp3000-labelled")
      ("application/vnd.wt.stf") ("application/vnd.xara")
      ("application/vnd.xfdl") ("application/vnd.yellowriver-custom-menu")
      ("application/whoispp-query") ("application/whoispp-response")
      ("application/wita") ("application/wordperfect5.1")
      ("application/x-apple-diskimage" "dmg")
      ("application/x-bcpio" "bcpio") ("application/x-bittorrent" "torrent")
      ("application/x-bzip2" "bz2") ("application/x-cdlink" "vcd")
      ("application/x-chess-pgn" "pgn") ("application/x-compress")
      ("application/x-cpio" "cpio") ("application/x-csh" "csh")
      ("application/x-director" "dcr" "dir" "dxr")
      ("application/x-dvi" "dvi") ("application/x-futuresplash" "spl")
      ("application/x-gtar" "gtar") ("application/x-gzip" "gz" "tgz")
      ("application/x-hdf" "hdf") ("application/x-java-archive" "jar")
      ("application/x-javascript" "js") ("application/x-kword" "kwd" "kwt")
      ("application/x-kspread" "ksp")
      ("application/x-kpresenter" "kpr" "kpt")
      ("application/x-kchart" "chrt") ("application/x-killustrator" "kil")
      ("application/x-koan" "skp" "skd" "skt" "skm")
      ("application/x-latex" "latex") ("application/x-netcdf" "nc" "cdf")
      ("application/x-rpm" "rpm") ("application/x-sh" "sh")
      ("application/x-shar" "shar") ("application/x-shockwave-flash" "swf")
      ("application/x-stuffit" "sit") ("application/x-sv4cpio" "sv4cpio")
      ("application/x-sv4crc" "sv4crc") ("application/x-tar" "tar")
      ("application/x-tcl" "tcl") ("application/x-tex" "tex")
      ("application/x-texinfo" "texinfo" "texi")
      ("application/x-troff" "t" "tr" "roff")
      ("application/x-troff-man" "man") ("application/x-troff-me" "me")
      ("application/x-troff-ms" "ms") ("application/x-ustar" "ustar")
      ("application/x-wais-source" "src") ("application/x400-bp")
      ("application/xhtml+xml" "xhtml" "xht") ("application/xml")
      ("application/xml-dtd") ("application/xml-external-parsed-entity")
      ("application/zip" "zip") ("audio/32kadpcm")
      ("audio/basic" "au" "snd") ("audio/g.722.1") ("audio/l16")
      ("audio/midi" "mid" "midi" "kar") ("audio/mp4a-latm")
      ("audio/mpa-robust") ("audio/mpeg" "mpga" "mp2" "mp3")
      ("audio/parityfec") ("audio/prs.sid") ("audio/telephone-event")
      ("audio/tone") ("audio/vnd.cisco.nse") ("audio/vnd.cns.anp1")
      ("audio/vnd.cns.inf1") ("audio/vnd.digital-winds")
      ("audio/vnd.everad.plj") ("audio/vnd.lucent.voice")
      ("audio/vnd.nortel.vbk") ("audio/vnd.nuera.ecelp4800")
      ("audio/vnd.nuera.ecelp7470") ("audio/vnd.nuera.ecelp9600")
      ("audio/vnd.octel.sbc") ("audio/vnd.qcelp")
      ("audio/vnd.rhetorex.32kadpcm") ("audio/vnd.vmx.cvsd")
      ("audio/x-aiff" "aif" "aiff" "aifc") ("audio/x-mpegurl" "m3u")
      ("audio/x-pn-realaudio" "ram" "rm") ("audio/x-realaudio" "ra")
      ("audio/x-wav" "wav") ("audio/x-ms-wma" "wma")
      ("audio/x-ms-wax" "wax") ("chemical/x-pdb" "pdb")
      ("chemical/x-xyz" "xyz") ("image/bmp" "bmp") ("image/cgm")
      ("image/g3fax") ("image/gif" "gif") ("image/ief" "ief")
      ("image/jpeg" "jpeg" "jpg" "jpe") ("image/naplps") ("image/png" "png")
      ("image/prs.btif") ("image/prs.pti") ("image/tiff" "tiff" "tif")
      ("image/vnd.cns.inf2") ("image/vnd.djvu" "djvu" "djv")
      ("image/vnd.dwg") ("image/vnd.dxf") ("image/vnd.fastbidsheet")
      ("image/vnd.fpx") ("image/vnd.fst") ("image/vnd.fujixerox.edmics-mmr")
      ("image/vnd.fujixerox.edmics-rlc") ("image/vnd.mix")
      ("image/vnd.net-fpx") ("image/vnd.svf") ("image/vnd.wap.wbmp" "wbmp")
      ("image/vnd.xiff") ("image/x-cmu-raster" "ras")
      ("image/x-portable-anymap" "pnm") ("image/x-portable-bitmap" "pbm")
      ("image/x-portable-graymap" "pgm") ("image/x-portable-pixmap" "ppm")
      ("image/x-rgb" "rgb") ("image/x-xbitmap" "xbm")
      ("image/x-xpixmap" "xpm") ("image/x-xwindowdump" "xwd")
      ("message/delivery-status") ("message/disposition-notification")
      ("message/external-body") ("message/http") ("message/news")
      ("message/partial") ("message/rfc822") ("message/s-http")
      ("model/iges" "igs" "iges") ("model/mesh" "msh" "mesh" "silo")
      ("model/vnd.dwf") ("model/vnd.flatland.3dml") ("model/vnd.gdl")
      ("model/vnd.gs-gdl") ("model/vnd.gtw") ("model/vnd.mts")
      ("model/vnd.vtu") ("model/vrml" "wrl" "vrml")
      ("multipart/alternative") ("multipart/appledouble")
      ("multipart/byteranges") ("multipart/digest") ("multipart/encrypted")
      ("multipart/form-data") ("multipart/header-set") ("multipart/mixed")
      ("multipart/parallel") ("multipart/related") ("multipart/report")
      ("multipart/signed") ("multipart/voice-message") ("text/calendar")
      ("text/css" "css") ("text/directory") ("text/enriched")
      ("text/html" "html" "htm") ("text/parityfec")
      ("text/plain" "asc" "txt") ("text/prs.lines.tag")
      ("text/rfc822-headers") ("text/richtext" "rtx") ("text/rtf" "rtf")
      ("text/sgml" "sgml" "sgm") ("text/tab-separated-values" "tsv")
      ("text/t140") ("text/uri-list") ("text/vnd.DMClientScript")
      ("text/vnd.IPTC.NITF") ("text/vnd.IPTC.NewsML") ("text/vnd.abc")
      ("text/vnd.curl") ("text/vnd.flatland.3dml") ("text/vnd.fly")
      ("text/vnd.fmi.flexstor") ("text/vnd.in3d.3dml")
      ("text/vnd.in3d.spot") ("text/vnd.latex-z")
      ("text/vnd.motorola.reflex") ("text/vnd.ms-mediapackage")
      ("text/vnd.sun.j2me.app-descriptor" "jad") ("text/vnd.wap.si")
      ("text/vnd.wap.sl") ("text/vnd.wap.wml" "wml")
      ("text/vnd.wap.wmlscript" "wmls") ("text/x-setext" "etx")
      ("text/xml" "xml" "xsl") ("text/xml-external-parsed-entity")
      ("video/mp4v-es") ("video/mpeg" "mpeg" "mpg" "mpe")
      ("video/parityfec") ("video/pointer") ("video/quicktime" "qt" "mov")
      ("video/vnd.fvt") ("video/vnd.motorola.video")
      ("video/vnd.motorola.videop") ("video/vnd.mpegurl" "mxu")
      ("video/vnd.mts") ("video/vnd.nokia.interleaved-multimedia")
      ("video/vnd.vivo") ("video/x-flv" "flv")
      ("video/x-ms-asf" "asf" "asx") ("video/x-ms-wm" "wm")
      ("video/x-ms-wmv" "wmv") ("video/x-ms-wmx" "wmx")
      ("video/x-ms-wvx" "wvx") ("video/x-msvideo" "avi")
      ("video/x-sgi-movie" "movie") ("x-conference/x-cooltalk" "ice")
      ;; hand-added by cox 27-jul-2015.  video/mp4 entry is in ubuntu 15.04's
      ;; /etc/mime.types
      ("video/mp4" "mp4")
      ))

(defvar *mime-types* nil)

(defun build-mime-types-table ()
  (if* (null *mime-types*)
     then (setf *mime-types* (make-hash-table :test #'equalp))
	  (dolist (ent *file-type-to-mime-type*)
	    (dolist (type (cdr ent))
	      (setf (gethash type *mime-types*) (car ent))))))
  

(build-mime-types-table)  ;; build the table now

(defmethod lookup-mime-type (filename)
  ;; return mime type if known
  (if* (pathnamep filename)
     then (setq filename (namestring filename)))
  (multiple-value-bind (root tail name type)
      (split-namestring filename)
    (declare (ignore root name))
    (if* (and type (gethash type *mime-types*))
       thenret
     elseif (gethash (list tail) *mime-types*) 
       thenret)))

		     

(defun unpublish (&key all (server *wserver*))
  (if* all
     then (dolist (locator (wserver-locators server))
	    (unpublish-locator locator))
     else (error "not done yet")))
  
;; methods on entity objects

;-- content-length -- how long is the body of the response, if we know

(defmethod content-length ((ent entity))
  ;; by default we don't know, and that's what nil means
  nil)

(defmethod content-length ((ent file-entity))
  (let ((contents (contents ent)))
    (if* contents
       then (length contents)
       else ; may be a file on the disk, we could
	    ; compute it.. this is
	    ;** to be done
	    nil)))


(defmethod content-length ((ent special-entity))
  (let ((body (special-entity-content ent)))
    (if* body 
       then (length body) 
       else 0)))

(defmethod content-length ((ent multi-entity))
  (multi-entity-content-length ent))

;- transfer-mode - will the body be sent in :text or :binary mode.
;  use :binary if you're not sure

(defmethod transfer-mode ((ent entity))
  (or (entity-format ent) :binary)
  )








  


;; url exporting








(defun publish (&key (host nil host-p) port path function class format
		     content-type
		     (server *wserver*)
		     locator
		     remove
		     authorizer
		     timeout
		     plist
		     hook
		     headers
		     (compress t)
		     will-handle-expect-continue
		     )
  ;; publish the given url
  ;; if file is given then it specifies a file to return
  ;; 
  (let (hval)
    (if* (null locator) 
       then (setq locator (find-locator :exact server)))

    (setq hval (convert-to-vhosts (if* (and host (atom host))
				     then (list host)
				     else host)
				  server))
    
    (if* remove
       then ; eliminate the entity if it exists
	    (unpublish-entity locator path hval host-p)
       else
	     
	    (let ((ent (make-instance (or class 'computed-entity)
			 :host hval 
			 :port port
			 :path path
			 :function function
			 :format format
			 :content-type content-type
			 :authorizer authorizer
			 :plist plist
			 :timeout timeout
			 :hook hook
			 :headers headers
			 :compress compress
			 :will-handle-expect-continue will-handle-expect-continue
			 )))
	      (publish-entity ent locator path hval)))))

(defun publish-prefix (&key (host nil host-p) port prefix
			    function class format
			    content-type
			    (server *wserver*)
			    locator
			    remove
			    authorizer
			    timeout
			    plist
			    headers
			    (compress t)
			    will-handle-expect-continue
			    )
  ;; publish a handler for all urls with a certain prefix
  ;; 
  (let (hval)
    (if* (null locator) 
       then (setq locator (find-locator :prefix server)))

    (setq hval (convert-to-vhosts (if* (and host (atom host))
				     then (list host)
				     else host)
				  server))
    
    (if* remove
       then ; eliminate the entity if it exists
	    (publish-prefix-entity nil prefix locator hval host-p t)
	    nil
       else
	     
	    (let ((ent (make-instance (or class 'computed-entity)
			 :host hval 
			 :port port
			 :prefix prefix
			 :function function
			 :format format
			 :content-type content-type
			 :authorizer authorizer
			 :plist plist
			 :timeout timeout
			 :headers headers
			 :compress compress
			 :will-handle-expect-continue will-handle-expect-continue
			 )))
	      (publish-prefix-entity ent prefix locator  hval
				     host-p nil)
	      ent))))

	     

(defun publish-file (&key (server *wserver*)
			  locator
			  (host nil host-p) 
			  port path
			  file content-type class preload
			  cache-p
			  remove
			  authorizer
			  plist
			  (timeout #+io-timeout #.(* 100 24 60 60)
				   #-io-timeout nil)
			  hook
			  headers
			  (compress t)
			  will-handle-expect-continue
			  )
			  
  ;; return the given file as the value of the url
  ;; for the given host.
  ;; If host is nil then return for any host
  (let (ent got c-type hval)
    (if* (null locator) 
       then (setq locator (find-locator :exact server)))

    (setq hval (convert-to-vhosts (if* (and host (atom host))
				     then (list host)
				     else host)
				  server))
    (if* remove
       then (unpublish-entity locator path
			      hval
			      host-p)
	    (return-from publish-file nil))
  
  
    (setq c-type (or content-type
		     (lookup-mime-type file)
		     "application/octet-stream"))
    
    (if* preload
       then ; keep the content in core for fast display
	    (with-open-file (p file
			     #-(and allegro (version>= 6))
			     :element-type
			     #-(and allegro (version>= 6))
			     '(unsigned-byte 8))
	      (let ((size (excl::filesys-size (stream-input-fn p)))
		    (lastmod (excl::filesys-write-date (stream-input-fn p)))
		    (guts))
		(setq guts (make-array size :element-type '(unsigned-byte 8)))
	      
		(if* (not (eql size (setq got (read-sequence guts p))))
		   then (error "~s should have been ~d bytes but was ~d"
			       file
			       size
			       got))
		(setq ent (make-instance (or class 'file-entity)
			    :host hval 
			    :port port
			    :path path
			    :file file
			    :content-type c-type
			    
			    :contents  guts
			    :last-modified lastmod
			    :last-modified-string (universal-time-to-date lastmod)
			    
			    :cache-p cache-p
			    :authorizer authorizer
			    :timeout  timeout
			    :plist plist
			    :hook hook
			    :headers headers
			    :compress compress
			    :will-handle-expect-continue will-handle-expect-continue
			    ))))
       else (setq ent (make-instance (or class 'file-entity)
			:host hval 
			:port port
			:path path
			:file file
			:content-type c-type
			:cache-p cache-p
			:authorizer authorizer
			:timeout timeout
			:plist plist
			:hook hook
			:headers headers
			:compress compress
			:will-handle-expect-continue will-handle-expect-continue
			)))

    (publish-entity ent locator path hval)))







(defun publish-directory (&key prefix 
			       (host nil host-p)
			       port
			       destination
			       (server *wserver*)
			       locator
			       remove
			       authorizer
			       (indexes '("index.html" "index.htm"))
			       filter
			       (timeout #+io-timeout #.(* 100 24 60 60)
					#-io-timeout nil)
			       publisher
			       access-file
			       plist
			       hook
			       headers
			       (compress t)
			       hidden-index-redirect
			       will-handle-expect-continue
			       )
  
  ;; make a whole directory available
  
  (if* (null locator) 
     then (setq locator (find-locator :prefix server)))

  (if* (and host (atom host))
     then (setq host (list host)))
  
  (setq host (convert-to-vhosts host server))  ; now a list of vhosts

  (if* remove
     then (publish-prefix-entity nil prefix locator
				 host host-p t)
	  (return-from publish-directory nil))
  
  (let ((ent (make-instance 'directory-entity 
	       :directory (if* (atom destination)
			     then (list destination)
			     else destination)
	       :prefix prefix
	       :host host
	       :port port
	       :authorizer authorizer
	       :indexes indexes
	       :filter filter
	       :timeout timeout
	       :publisher publisher
	       :access-file access-file
	       :plist plist
	       :hook hook
	       :headers headers
	       :compress compress
	       :hidden-index-redirect hidden-index-redirect
	       :will-handle-expect-continue will-handle-expect-continue
	       )))
    
    (publish-prefix-entity ent prefix locator host host-p nil)
    
    ent
    ))



(defun publish-prefix-entity (ent prefix locator host host-p remove)
  ;; add or remove an entity ent from the locator
  ;;
  (dolist (entpair (locator-info locator))
    (if* (equal (prefix-handler-path entpair) prefix)
       then ; match, prefix
	    (if* (and remove (not host-p))
	       then ; remove all entries for all hosts
		    (setf (locator-info locator)
		      (remove entpair (locator-info locator)))
		    (return-from publish-prefix-entity nil))
	    

	    (let ((handlers (prefix-handler-host-handlers entpair)))
	      (dolist (host host)
		(dolist (hostpair handlers
			  ; not found, add it if we're not removing
			  (if* (not remove)
			     then (push (make-host-handler :host host
							   :entity ent)
					handlers)))
		  (if* (eq host (host-handler-host hostpair))
		     then ; a match
			  (if* remove
			     then (setq handlers
				    (remove hostpair handlers :test #'eq))
			     else ; change
				  (setf (host-handler-entity hostpair) ent))
			  (return))))
	      (setf (prefix-handler-host-handlers entpair) handlers))
	    
	    ; has been processed, time to leave
	    (return-from publish-prefix-entity ent)))

  ; prefix not present, must add.
  ; keep prefixes in order, with max length first, so we match
  ; more specific before less specific
  
  (if* remove 
     then ; no work to do
	  (return-from publish-prefix-entity nil))
  
  (let ((len (length prefix))
	(list (locator-info locator))
	(new-ent (make-prefix-handler
		  :path prefix
		  :host-handlers (mapcar #'(lambda (host)
					     (make-host-handler 
					      :host host 
					      :entity ent))
					 host))))
    (if* (null list)
       then ; this is the first
	    (setf (locator-info locator) (list new-ent))
     elseif (>= len (length (caar list)))
       then ; this one should preceed all other ones
	    (setf (locator-info locator) (cons new-ent list))
       else ; must fit somewhere in the list
	    (do* ((back list (cdr back))
		  (cur  (cdr back) (cdr cur)))
		((null cur)
		 ; put at end
		 (setf (cdr back) (list new-ent)))
	      (if* (>= len (length (caar cur)))
		 then (setf (cdr back) `(,new-ent ,@cur))
		      (return))))))



(defun publish-multi (&key (server *wserver*)
			   locator
			   (host nil host-p)
			   port
			   path
			   items
			   class
			   content-type
			   remove
			   authorizer
			   timeout
			   plist
			   hook
			   headers
			   (compress t)
			   will-handle-expect-continue
			   )
  
  (if* (null locator)
     then (setq locator (find-locator :exact server)))
  
  (if* remove
     then (unpublish-entity locator path host host-p)
	  (return-from publish-multi nil))
  
  (let* ((hval)
	 (ent (make-instance (or class 'multi-entity)
		:host (setq hval 
			(convert-to-vhosts
			 (if* host 
			    then (if* (and host (atom host))
				    then (list host)
				    else host))
			 server))
		:port port
		:path path
		:plist plist
		:format :binary ; we send out octets
		:items (mapcar #'(lambda (it)
				   (if* (or (symbolp it)
					    (functionp it))
				      then (make-multi-item
					    :kind :function
					    :data  it)
				    elseif (and (consp it)
						(eq :string (car it))
						(stringp (cadr it)))
				      then (make-multi-item
					    :kind :string
					    :data (cadr it)
					    :cache (string-to-octets
						    (cadr it)
						    :null-terminate nil))
				    elseif (and (consp it)
						(eq :binary (car it))
						(typep (cadr it) 
						       '(simple-array (unsigned-byte 8) (*))))
				      then (make-multi-item
					    :kind :binary
					    :data (cadr it)
					    :cache (cadr it))
				    elseif (or (stringp it) (pathnamep it))
				      then (make-multi-item
					    :kind :file
					    :data  it)
				      else (error "Illegal item for publish-multi: ~s" it)
					   ))
			       items)
		:content-type (or content-type "application/octet-stream")
		:authorizer authorizer
		:timeout timeout
		:hook hook
		:headers headers
		:compress compress
		:will-handle-expect-continue will-handle-expect-continue
		)))
    (publish-entity ent locator path hval)))








(defmethod publish-entity ((ent entity) 
			   (locator locator-exact)
			   path
			   hosts)
  ;; handle  putting an entity in hash
  ;; table of a locator-exact.
  ;;
  ;; assert: hosts is a non-null list of vhosts
  ;;
  (let ((ents (gethash path (locator-info locator))))
    ;; must replace entry with matching host parameter
    (dolist (host hosts)
      (let ((xent (assoc host ents :test #'eq)))
	(if* (null xent)
	   then ; add new one
		(push (cons host ent) ents)
	   else ; replace
		(setf (cdr xent) ent))))
    (setf (gethash path (locator-info locator)) ents)
  
    ent))



  

(defmethod unpublish-entity ((locator locator-exact)
			     path
			     hosts
			     host-p)
  ;; remove any entities matching the host and path.
  ;; if host-p is nil then remove all entities, don't match the host
  (let ((ents (gethash path (locator-info locator))))
    (if* ents
       then (if* host-p
	       then ; must patch the hosts
		    (dolist (host hosts)
		      (let ((xent (assoc host ents :test #'eq)))
			(if* xent
			   then (setq ents
				  (delete xent ents :test #'eq)))))
		    (if* (null ents)
		       then (remhash path (locator-info locator))
		       else (setf (gethash path (locator-info locator)) ents))
	       else ; throw away everything
		    (remhash path (locator-info locator))))))


(defun convert-to-vhosts (hosts server)
  ;; host is a list or nil
  ;; if an element is a string lookup the virtual host
  ;; and create one of none is specified
  (if* (null hosts)
     then ; specify the wild card host
	  (list :wild)
     else ; convert strings to vhosts
	  (let (res)
	    (dolist (host hosts)
	      (let (vhost)
		(if* (stringp host)
		   then 
			(if* (null 
			      (setq vhost (gethash host 
						   (wserver-vhosts server))))
			   then ; not defined yet, must define
				(setq vhost
				  (setf (gethash host
						 (wserver-vhosts server))
				    (make-instance 'vhost
				      :log-stream
				      (wserver-log-stream server)
				      :error-stream
				      (wserver-log-stream server)
				      :names 
				      (list host)))))
		   else (setq vhost host))
		(pushnew vhost res :test #'eq)))
	    res)))
				

(defmethod handle-request ((req http-request))

  
  ;; run all filters, starting with vhost filters
  ;  a return value of :done means don't
  ;  run any further filters
  (dolist (filter (vhost-filters (request-vhost req))
	    (dolist (filter (wserver-filters *wserver*))
	      (if* (eq :done (funcall filter req)) then (return))))    
    (if* (eq :done (funcall filter req)) then (return)))
  
    
  (dolist (locator (wserver-locators *wserver*))
    (let ((ent (standard-locator req locator)))
      (if* ent
	 then ; check if it is authorized
	      (if* (authorize-and-process req ent)
		 then (return-from handle-request)))))
  
  ; no handler
  (failed-request req)
		       
  )

(defun authorize-and-process (req ent)
  ;; check for authorization need and process or send back 
  ;; a message why it failed
  ;; if we actually http responded return  true, else return nil
  ;;
  ;; all authorizers must succeed for it to succeed
  
  (let ((authorizers (entity-authorizer ent)))
    
    (if* (and authorizers (atom authorizers))
       then (setq authorizers (list authorizers)))
    
    (dolist (authorizer authorizers)
      (let ((result (authorize authorizer req ent)))
	(if* (eq result t)
	   thenret ; ok so far, but keep checking
	 elseif (eq result :done)
	   then ; already responsed
		(return-from authorize-and-process t)
	 elseif (eq result :deny)
	   then ; indicate denied request
		(denied-request req)
		(return-from authorize-and-process t)
	   else ; failed to authorize
		(return-from authorize-and-process nil))))
    
    (when (member (request-method req) '(:put :post))
      (let ((cont (header-slot-value req :expect)))
	(setf (request-has-continue-expectation req)
	  (and cont (match-re "\\b100-continue\\b" cont :case-fold t :return nil))))
      (unless (entity-will-handle-expect-continue ent)
	(send-100-continue req)))

    ; all authorization ok. try to run it and return the 
    ; value representing its exit status
    (process-entity req ent)))
    
    
  
  
(defmethod failed-request ((req http-request))
  ;; generate a response to a request that we can't handle
  (let ((entity (wserver-invalid-request *wserver*)))
    (if* (null entity)
       then (setq entity 
	      (make-instance 'computed-entity
		:path "--- built-in 404 ---"
		:function #'(lambda (req ent)
			      (with-http-response 
				  (req ent
				       :response *response-not-found*)
				(with-http-body (req ent)
				  (html 
				   (:html
				    (:head (:title "404 - NotFound"))
				    (:body
				     (:h1 "Not Found")
				     "The request for "
				     (:b
				      (:princ-safe 
				       (render-uri 
					(request-uri req)
					nil
					)))
				     " was not found on this server."
				     :br
				     :br
				     :hr
				     (:i
				      "AllegroServe "
				      (:princ-safe *aserve-version-string*))
				     ))))))
		:content-type "text/html"))
	    (setf (wserver-invalid-request *wserver*) entity))
    (process-entity req entity)))

(defmethod denied-request ((req http-request))
  ;; generate a response to a request that we can't handle
  (let ((entity (wserver-denied-request *wserver*)))
    (if* (null entity)
       then (setq entity 
	      (make-instance 'computed-entity
		:path "--- built-in 404 ---"
		:function #'(lambda (req ent)
			      (with-http-response 
				  (req ent
				       :response *response-not-found*)
				(with-http-body (req ent)
				  (html 
				   (:html
				    (:head (:title "404 - NotFound"))
				    (:body
				     (:h1 "Not Found")
				     "The request for "
				     (:princ-safe 
				      (render-uri 
				       (request-uri req)
				       nil
				       ))
				     " was denied."))))))
		:content-type "text/html"))
	    (setf (wserver-denied-request *wserver*) entity))
    (process-entity req entity)))


(defmethod standard-locator ((req http-request)
			     (locator locator-exact))
  ;; standard function for finding an entity in an exact locator
  ;; return the entity if one is found, else return nil
  
  (if* (uri-scheme (request-raw-uri req))
     then ; ignore proxy requests
	  (return-from standard-locator nil))
  
  (let ((ents (gethash (request-decoded-uri-path req)
		       (locator-info locator))))
    (cdr 
     (or (assoc (request-vhost req) ents :test #'eq)
	 (assoc :wild ents :test #'eq)))))

(defmethod standard-locator ((req http-request)
			     (locator locator-prefix))
  ;; standard function for finding an entity in an exact locator
  ;; return the entity if one is found, else return nil
  
  (if* (uri-scheme (request-raw-uri req))
     then ; ignore proxy requests
	  (return-from standard-locator nil))
  
  (let* ((url (request-decoded-uri-path req))
	 (len-url (length url))
	 (vhost (request-vhost req)))
	     
    (dolist (entpair (locator-info locator))
      (if* (and (>= len-url (length (prefix-handler-path entpair)))
		(buffer-match url 0 (prefix-handler-path entpair)))
	 then ; we may already be a wiener
	      (let ((hh (or (assoc vhost (prefix-handler-host-handlers
					   entpair)
				    :test #'eq)
			     (assoc :wild (prefix-handler-host-handlers
					   entpair)
				    :test #'eq))))
		(if* hh
		   then (return (host-handler-entity hh))))))))
    
					   
					  
  

(defun find-locator (name wserver)
  ;; give the locator with the given name
  (dolist (locator (wserver-locators wserver)
	    (error "no such locator as ~s" name))
    (if* (eq name (locator-name locator))
       then (return locator))))


(defmethod unpublish-locator ((locator locator-exact))
  (clrhash (locator-info locator)))

(defmethod unpublish-locator ((locator locator-prefix))
  (setf (locator-info locator) nil))


(defmethod map-entities (function (locator locator))
  ;; do nothing if no mapping function defined
  (declare (ignore function))
  nil)

(defmethod map-entities (function (locator locator-exact))
  ;; map the function over the entities in the locator
  (maphash #'(lambda (k v)
	       (let (remove)
		 (dolist (pair v)
		   (if* (eq :remove (funcall function (cdr pair)))
		      then (push pair remove)))
		 (if* remove
		    then (dolist (rem remove)
			   (setq v (remove rem v :test #'eq)))
			 (if* (null v)
			    then (remhash k (locator-info locator))
			    else (setf (gethash k (locator-info locator)) 
				   v)))))
	   (locator-info locator)))

(defmethod map-entities (function (locator locator-prefix))
  (let (outer-remove)
    (dolist (ph (locator-info locator))
      (let (remove)
	(dolist (hh (prefix-handler-host-handlers ph))
	  (let ((ent (host-handler-entity hh)))
	    (if* ent 
	       then (if* (eq :remove (funcall function ent))
		       then (push hh remove)))))
	(if* remove
	   then (let ((v (prefix-handler-host-handlers ph)))
		  (dolist (rem remove)
		    (setq v (remove rem v :test #'eq)))
		  (if* (null v)
		     then (push ph outer-remove) ; remove whole thing
		     else (setf (prefix-handler-host-handlers ph) v))))))
    
    (if* outer-remove
       then ; remove some whole prefixes
	    (let ((v (locator-info locator)))
	      (dolist (rem outer-remove)
		(setq v (remove rem v :test #'eq)))
	      (setf (locator-info locator) v)))
    ))
  
	      



  



	  
				



(defmethod process-entity ((req http-request) (entity computed-entity))
  ;; 
  (let ((fcn (entity-function entity)))
    (funcall fcn req entity)
    t	; processed
    ))




       
  
(defmethod process-entity ((req http-request) (ent file-entity))
    
  (tagbody 
   retry 
    (let ((contents (contents ent)))
      (if* contents
	 then ;(preloaded)
	      ; ensure that the cached file matches the 
	      ; actual file
	      (if* (not (eql (last-modified ent)
			     (file-write-date (file ent))))
		 then ; uncache it
		      (setf (contents ent) nil
			    (last-modified ent) nil)
		      (go retry))
	      
	      ; set the response code and 
	      ; and header fields then dump the value
	      
	      ; * should check for range here
	      ; for now we'll send it all
	      (with-http-response (req ent
				       :content-type (content-type ent)
				       :format :binary)
		(setf (request-reply-content-length req) (length contents))
		(setf (reply-header-slot-value req :last-modified)
		  (last-modified-string ent))

		(run-entity-hook req ent nil)
	      
		(with-http-body (req ent)
		  ;; at this point the header are out and we have a stream
		  ;; to write to 
		  (write-sequence contents (request-reply-stream req))
		  ))
       
	    
	    
	 else ; the non-preloaded case
	      (let (accepts compressed-file-comp)
		(if* (and (entity-compress ent) ; look for compression
			  (setq accepts (compute-accepted-encodings 
					 req :as-keywords t))
			  (setq compressed-file-comp
			    (find-compressed-version 
			     (excl::merge-to-physical (file ent)) 
			     accepts)))
		   then (send-file-back req 
					ent
					(car compressed-file-comp) ; name
					(cdr compressed-file-comp) ; enc
					nil	; no cache
					)
		   else (if* (eq :retry (send-file-back req 
							ent
							(file ent)
							nil
							t))
			   then (go retry)))))))
    
  t	; we've handled it
  )

(defun astring+ (&rest args)
  #+(version>= 8 2) (apply #'util.string:string+ args)
  ;; Need a substitute in 8.1. [bug23328] 
  #-(version>= 8 2) (format nil "~{~A~}" args)
  )

(defun find-compressed-version (filename accepted-encodings)
  ; see if there is a compressed version of this file on the 
  ; disk that's newer than the uncompressed one
  (let ((uncompressed-stat (ignore-errors (excl.osi:stat filename))))
    (dolist (file-type (wserver-compression-file-types *wserver*))
      (if* (member (cdr file-type) accepted-encodings :test #'eq)
	 then (let* ((compressed-name (astring+ 
				       filename "." (car file-type)))
		     (compressed-stat 
		      (ignore-errors (excl.osi:stat compressed-name))))
		(if* compressed-stat
		   then (if* uncompressed-stat
			   then ; both exist
				(if* (>= (excl.osi:stat-mtime compressed-stat)
					 (excl.osi:stat-mtime uncompressed-stat))
				   then ; compressed is newer, a win
					(return-from find-compressed-version
					  (cons compressed-name 
						(cdr file-type) ; encoding
						)))
			   else ; uncompressed doesn't exist.. return
				; compressed version
				(return-from find-compressed-version
				  (cons compressed-name 
					(cdr file-type) ; encoding
					)))))))))
		      
		  

; this is stack allocated so don't make it too big
(defparameter *send-buffer-lock* (mp:make-process-lock))
(defconstant  *send-buffer-size* #.(* 16 1024))

(defvar *send-buffers* nil)

(defun alloc-send-buffer ()
  (mp:with-process-lock (*send-buffer-lock*)
    (let ((buff (pop *send-buffers*)))
      (if* buff
	 thenret
	 else (make-array *send-buffer-size* :element-type '(unsigned-byte 8))))))

(defun free-send-buffer (buffer)
  (mp:with-process-lock (*send-buffer-lock*)
    (push buffer *send-buffers*)))


	      
  

(defun send-file-back (req ent filename encoding cache-ok)
  (let (p range)
		
    (setf (last-modified ent) nil) ; forget previous cached value
	      
    (if* (null (errorset 
		(setq p (open filename
			      :direction :input
			      #-(and allegro (version>= 6))
			      :element-type
			      #-(and allegro (version>= 6))
			      '(unsigned-byte 8)))))
       then ; file not readable
		      
	    (return-from send-file-back nil))
	      
    (unwind-protect 
	(progn
	  (let ((size (excl::filesys-size (stream-input-fn p)))
		(lastmod (excl::filesys-write-date 
			  (stream-input-fn p)))
		(buffer nil))
				
			
	    (setf (last-modified ent) lastmod
		  (last-modified-string ent)
		  (universal-time-to-date lastmod))
		      
	    (if* (and cache-ok (cache-p ent))
	       then ; we should read and cache the contents
		    ; and then do the cached thing
		    (let ((wholebuf 
			   (make-array
			    size
			    :element-type '(unsigned-byte 8))))
		      (read-sequence wholebuf p)
		      (setf (contents ent) wholebuf))
		    (return-from send-file-back :retry))
			      


	    (setq buffer (alloc-send-buffer))
	    
	    (if* (setq range (header-slot-value req :range))
	       then (setq range (parse-range-value range))
		    (if* (not (eql (length range) 1))
		       then ; ignore multiple ranges 
			    ; since we're not
			    ; prepared to send back a multipart
			    ; response yet.
			    (setq range nil)))
	    (if* range
	       then (return-from send-file-back
		      (prog1 (return-file-range-response
			      req ent range buffer p size)
			(free-send-buffer buffer))))

	    
	    
		      
	    (with-http-response (req ent :format :binary)

	      ;; control will not reach here if the request
	      ;; included an if-modified-since line and if
	      ;; the lastmod value we just calculated shows
	      ;; that the file hasn't changed since the browser
	      ;; last grabbed it.
                          
	      (setf (request-reply-content-length req) size)
	      (setf (reply-header-slot-value req :last-modified)
		(last-modified-string ent))
	      
	      (if* encoding
		 then (setf (reply-header-slot-value req :content-encoding)
			(string encoding)))
	       
			
	      (run-entity-hook req ent nil)

	      
	      
	      
	      (with-http-body (req ent)
		(loop
		  (if* (<= size 0) then (return))
		  (let ((got (read-sequence buffer 
					    p :end 
					    (min size (length buffer)))))
		    (if* (<= got 0) then (return))
		    (write-sequence buffer (request-reply-stream req)
				    :end got)
		    (decf size got))))
	      
	      
	      (free-send-buffer buffer)
	      
	      )))
		      
		      
		
      (close p))))
  


(defun run-entity-hook (req ent extra)
  ;; if there is a hook function, call it.
  (let ((hook (entity-hook ent)))
    (if* hook then (funcall hook req ent extra))))


(defun return-file-range-response (req ent range buffer p size)
  ;; read and return just the given range from the file.
  ;; assert: range has exactly one range
  
  (let ((start (caar range))
	(end   (cdar range)))
    (if* (null start)
       then ; suffix range
	    (setq start (max 0 (- size end)))
	    (setq end (1- size))
     elseif (null end)
	    ; extends beyond end
       then (setq end (1- size))
       else (setq end (min end (1- size))))
	    
    ; we allow end to be 1- start to mean 0 bytes to transfer
    (if* (> start (1- end))
       then ; bogus range
	    (with-http-response (req ent 
				     :response 
				     *response-requested-range-not-satisfiable*)
	      
	      (run-entity-hook req ent :illegal-range)
	      (with-http-body (req ent)
		(html "416 - Illegal Range Specified")))
       else ; valid range
	    (with-http-response (req ent
				     :response *response-partial-content*
				     :format :binary)
	      (setf (reply-header-slot-value req :content-range)
		(format nil "bytes ~d-~d/~d" start end size))
	      (setf (request-reply-content-length req) 
		(max 0 (1+ (- end start))))
	      
	      (run-entity-hook req ent :in-range)
	      (with-http-body (req ent)
		(file-position p start)
		(let ((left (max 0 (1+ (- end start)))))
		  (loop
		    (if* (<= left 0) then (return))
		    (let ((got (read-sequence buffer p :end
					      (min left (length buffer)))))
		      (if* (<= got 0) then (return))
		      (write-sequence buffer *html-stream*
				      :end got)
		      (decf left got)))))))
    
    t ; meaning we sent something
    ))
				    
		
	    
	    
	    
	    
	    
  


(defmethod process-entity ((req http-request) (ent directory-entity))
  ;; try each directory in turn for a file
  (dolist (directory (entity-directory ent) nil)
    (if* (process-entity-single-directory req ent directory)
       then (return t))))

  
(defun process-entity-single-directory (req ent directory)  
  ;; search for a file in the directory and then create a file
  ;; entity for it so we can track last modified.

  (let* ((postfix nil)
	 (path  (request-decoded-uri-path req))
	 (realname (concatenate 'string
		     directory
		     (setq postfix (subseq path (length (prefix ent))))))
	 (redir-to)
	 (info)
	 (forbidden)
	 (redirect-kind *response-temporary-redirect*)
	 )
    (debug-format :info "directory request for ~s" realname)
    
    ; we can't allow the brower to specify a url with 
    ; any ..'s in it as that would allow the browser to 
    ; search outside the tree that's been published
    (if* (or #+mswindows (position #\\ postfix) ; don't allow windows dir sep
	     (match-regexp "\\.\\.[\\/]" postfix))
       then ; contains ../ or ..\  
	    ; ok, it could be valid, like foo../, but that's unlikely
	    ; Also on Windows don't allow \ since that's a directory sep
	    ; and user should be using / in http paths for that.
	    (return-from process-entity-single-directory nil))
    
    (if* sys:*tilde-expand-namestrings*
       then (setq realname (excl::tilde-expand-unix-namestring realname)))
    
    (multiple-value-setq (info forbidden)
      (read-access-files ent realname postfix))
    
    (if* forbidden
       then ; give up right away.
	    (return-from process-entity-single-directory nil))
    
    (let ((type (excl::filesys-type realname)))
      
      ; file may not exist but compressed version might..
      ; if the compressed version exists then 
      ; publish the file in hopes that the request will
      ; ask for a compressed value
      (and (null type)
	   (entity-compress ent) ; compression allowed
	   (dolist (ctype (wserver-compression-file-types *wserver*))
	     (if* (probe-file (astring+ realname "." 
						   (car ctype)))
		then ; compressed version exists,
		     (setq type :file)
		     (return))))
       
      (if* (null type)
	 then ; not present
	      (return-from process-entity-single-directory nil)
       elseif (eq :directory type)
	 then ; Try the indexes (index.html, index.htm, or user-defined).
	      ; tack on a trailing slash if there isn't one already.
	      
	      (if* (not (eq #\/ (schar path (1- (length path)))))
		 then ; uri referred to a a directory but didn't end
		      ; in a slash, so add that slash
		      (setq redirect-kind 
			*response-moved-permanently*)
			
		      (setf redir-to 
			"" ; tack on a empty string
			)
		 else ; uri ends in /, realname is directory
		      ; ensure path we're building ends in / so
		      ; we can look for indexes
		      (if* (not (eq #\/ 
				    (schar realname (1- (length realname)))))
			 then (setq realname 
				(concatenate 'string realname "/")))
		      (dolist (index (directory-entity-indexes ent) 
				; no match to index file, give up
				(return-from process-entity-single-directory nil))
			(if* (eq :file (excl::filesys-type
					(concatenate 'string realname index)))
			   then ; we have an index
				(if* (directory-hidden-index-redirect ent)
				   then ; just send back the index file contents
					(setq realname
					  (concatenate 'string realname index))
				   else ; redirect to the index file
					(setq redir-to index))
				(return))))
	      
       elseif (not (eq :file type))
	 then  ; bizarre object
	      (return-from process-entity-single-directory nil)))
    
    (if* redir-to
       then ; redirect to an existing index file
	    (with-http-response (req ent
				     :response redirect-kind)
	      
		      
	      (let ((path (uri-path (request-uri req))))
		(setf (reply-header-slot-value req :location) 
		  (render-uri 
		   (copy-uri (request-uri req) 
			     :path 
			     (concatenate 
				 'string path
				 (if* (and path
					   (> (length path) 0)
					   (eq #\/ (aref path 
							 (1- (length path)))))
				    then ""
				    else "/")
				 redir-to))
		   nil))
			     
		(with-http-body (req ent))))
     elseif (and info (file-should-be-denied-p realname info))
       then ; we should ignore this file
	    (return-from process-entity-single-directory nil)
     elseif (and (directory-entity-filter ent)
		 (funcall (directory-entity-filter ent) req ent 
			  realname info))
       thenret ; processed by the filter
       else ;; ok realname is a file.
	    ;; create an entity object for it, publish it, and dispatch on it
	    (return-from process-entity-single-directory
	      (authorize-and-process 
	       req 
	       (funcall 
		(or (directory-entity-publisher ent)
		    #'standard-directory-entity-publisher)
				       
		req ent realname info))))
					   
    t))

    
(defun standard-directory-entity-publisher (req ent realname info)
  ;; the default publisher used when directory entity finds
  ;; a file it needs to publish

  (multiple-value-bind (content-type local-authorizer)
      (standard-access-file-reader realname info)

    ; now publish a file with all the knowledge
    (publish-file :path (request-decoded-uri-path req)
		  :host (host ent)
		  :file realname
		  :authorizer (or local-authorizer
				  (entity-authorizer ent))
		  :content-type content-type
		  :timeout (entity-timeout ent)
		  :plist (list :parent ent) ; who spawned us
		  :hook (entity-hook ent)
		  :headers (entity-headers ent)
		  :compress (entity-compress ent)
		  )))
      

(defun standard-access-file-reader (realname info)
  ;; gather the relevant information from the access file
  ;; information 'info' and return two values
  ;;  content-type  - if specific content type was specified
  ;;  authorizers - list of authorization objects
  ;;
  (let (content-type
	local-authorizer
	pswd-authorizer
	ip-authorizer
	)
    
    ; look for local mime info that would set the content-type
    ; of this file
    (block out
      (multiple-value-bind (root tail name type)
	  (split-namestring realname)
	(declare (ignore root name))
	(dolist (inf info)
	  (if* (eq :mime (car inf))
	     then ; test this mime info
		  (dolist (pat (getf (cdr inf) :types))
		    (if* (or (and type (member type (cdr pat) :test #'equalp))
			     (and tail 
				  (member (list tail) (cdr pat) 
					  :test #'equalp)))
		       then (setq content-type (car pat))
			    (return-from out t)))))))
		
    
    ; look for authorizer
    (let ((ip (assoc :ip info :test #'eq)))
      (if* ip
	 then (setq ip-authorizer 
		(make-instance 'location-authorizer 
		  :patterns (getf (cdr ip) :patterns)))))
    
    ; only one of ip and pswd allowed
    (let ((pswd (assoc :password info :test #'eq)))
      (if* pswd
	 then (setq pswd-authorizer
		(make-instance 'password-authorizer
		  :realm (getf (cdr pswd) :realm)
		  :allowed (getf (cdr pswd) :allowed)))))

    ; check password second
    (if* pswd-authorizer
       then (setq local-authorizer (list pswd-authorizer)))
    
    (if* ip-authorizer
       then (push ip-authorizer local-authorizer))
    
    (values content-type local-authorizer)

    ))


(defun read-access-files (ent realname postfix)
  ;; read and cache all access files involved in this access
  ; realname is the whole name of the file. Postfix is the part
  ; added by the uri and thus represents the part of the uri we
  ; need to scan for access files
  
  (let ((access-file (directory-entity-access-file ent))
	info
	pos
	opos
	file-write-date
	root)
    
    (if* (null access-file) then (return-from read-access-files nil))
    
    ; simplify by making '/' the directory separator on windows too
    #+mswindows
    (if* (position #\\ realname)
       then (setq realname (substitute #\/ #\\ realname)))
  
    ; search for slash ending root dir
    (setq pos (position #\/ realname
			:from-end t
			:end (- (length realname) (length postfix))))
    (loop 
      (if* (null pos) 
	 then (setq root "./")
	      (setq pos 1)
	 else (setq root (subseq realname 0 (1+ pos))))
    
      (let ((aname (concatenate 'string root access-file)))
	(if* (setq file-write-date (excl::file-write-date aname))
	   then ; access file exists
		(let ((entry (assoc aname 
				    (directory-entity-access-file-cache ent)
				    :test #'equal)))
		  (if* (null entry)
		     then (setq entry (list aname
					    0
					    nil))
			  (push entry (directory-entity-access-file-cache ent)))
		  (if* (> file-write-date (cadr entry))
		     then ; need to refresh
			  (setf (caddr entry) (read-access-file-contents aname))
			  (setf (cadr entry) file-write-date))
		
		  ; put new info at the beginning of the info list
		  (setq info (append (caddr entry) info)))))
    
      ; see if we have to descend a directory level
      (setq opos pos
	    pos (position #\/ realname :start (1+ pos)))
    
      (if* pos
	 then ; we must go down a directory level
	      
	      ; see if we can go down into this subdir
	      (if* info
		 then (let ((subdirname (subseq realname (1+ opos)
						pos)))
			(if* (eq :deny 
				 (check-allow-deny-info subdirname
							:subdirectories
							info))
			   then ; give up right away
				(return-from read-access-files 
				  (values nil :forbidden)))))
			      
		      
	      ; we can descend.. remove properties that don't get
	      ; inherited
	      (let (remove)
		(dolist (inf info)
		  (if* (null (getf (cdr inf) :inherit))
		     then (push inf remove)))
		(if* remove
		   then (dolist (rem remove)
			  (setq info (remove rem info)))))
	 else ; no more dirs to check
	      (return-from read-access-files info)))))


(defun read-access-file-contents (filename)
  ;; read and return the contents of the access file.
  ;;
  (handler-case 
      (with-open-file (p filename)
	(with-standard-io-syntax
	  (let ((*read-eval* nil) ; disable #. and #,
		(eof (cons nil nil)))
	    (let (info)
	      (loop (let ((inf (read p nil eof)))
		      (if* (eq eof inf)
			 then (return))
		      (push inf info)))
	      info))))
    (error (c)
      (logmess (format nil
		       "reading access file ~s resulted in error ~a" 
		       filename c))
      nil)))
		
   
	    
(defun file-should-be-denied-p (filename info)
  ;; given access info check to see if the given filename
  ;; should be denied (not allowed to access)
  ;; return t to deny access
  ;;
  (let (tailfilename)
    
    (let ((pos (position #\/ filename :from-end t)))
      (if* (null pos)
	 then (setq tailfilename filename)
	 else (setq tailfilename (subseq filename (1+ pos)))))

    ; :deny only if there are access files present which indicate deny
    
    (eq :deny (check-allow-deny-info tailfilename :files info))
	    
    ))
    
(defun check-allow-deny (name allow deny)
  ;; check to see if the name matches the allow/deny list.
  ;; possible answers
  ;; :allow  - on the allow list and not the deny
  ;; :deny - on the deny list
  ;;  nil    - not mentioned on the allow or deny lists
  ;;
  ;; :allow of nil same as ".*" meaning allow all
  ;; :deny of nil matches nothing 
  ;;
  
  ; clean up common mistakes in access files
  (let (state)
    (if* (and allow (atom allow))
       then (setq allow (list allow))
     elseif  (and (consp allow)
		  (eq 'quote (car allow)))
       then (setq allow (cadr allow)))
  
    (if* (and deny (atom deny))
       then (setq deny (list deny))
     elseif  (and (consp deny)
		  (eq 'quote (car deny)))
       then (setq deny (cadr deny)))
  
    (if* allow
       then ; must check all allows
	    (dolist (all allow 
		      ; not explicitly allowed
		      (setq state nil))
	      (if* (match-regexp all name :return nil)
		 then (setq state :allow)
		      (return)))
       else ; no allow's given, same as giving ".*" so matches all
	    (setq state :allow))
  
    (if* deny
       then ; must check all denys
	    (dolist (ign deny)
	      (if* (match-regexp ign name :return nil)
		 then ; matches, not allowed
		      (return-from check-allow-deny :deny))))
    
    state))
	    
	  

(defun check-allow-deny-info (name key info)
  ;; search the info under the given key to see if name is allowed
  ;; or denyd.
  ;; return :allow or :deny if we found access info in the info
  ;; else return nil if we didn't find any applicable access info
  (do* ((inflist info (cdr inflist))
       (inf (car inflist) (car inflist))
       (seen-inf)
       (state nil))
      ((null inf)
       (if* seen-inf
	  then (if* (null state)
		  then :deny  ; not mentioned as allowed
		  else state)))
    (if* (and (consp inf) (eq key (car inf)))
       then (setq seen-inf t) ; actually processed some info
	    (let ((new-state (check-allow-deny name
						 (getf (cdr inf) :allow)
						 (getf (cdr inf) :deny))))
	      (case new-state
		(:allow (setq state :allow))
		(nil ; state unchanged
		 )
		(:deny (return-from check-allow-deny-info :deny)))))))


    
  
  
  
  
  
  
		      
	      
	      
(defmethod process-entity ((req http-request) (ent multi-entity))
  ;; send out the contents of the multi
  ;;
  
  ; compute the contents of each item
  (let ((fwd) (max-fwd 0) (total-size 0))
    ;; we track max file write date (max-fwd) unless we can't compute
    ;; it in which case max-fwd is nil.
    
    (if* (not (member (request-method req) '(:get :head)))
       then ; we don't want to specify a last modified time except for
	    ; these two methods
	    (setq max-fwd nil))
    
    (dolist (item (items ent))
      (ecase (multi-item-kind item)
	(:file 
	 (setq fwd (file-write-date (multi-item-data item)))
	 (if* (or (null (multi-item-last-modified item))
		  (null fwd)
		  (> fwd (multi-item-last-modified item)))
	    then ; need to read new contents
		 (if* (null (errorset
			     (with-open-file (p (multi-item-data item)
					      :direction :input)
			       (let* ((size (excl::filesys-size 
					     (stream-input-fn p)))
				      (contents
				       (make-array size 
						   :element-type 
						   '(unsigned-byte 8))))
				 (read-sequence contents p)
				 (incf total-size size)
				 (setf (multi-item-cache item) contents)
				 (setf (multi-item-last-modified item) fwd)
				 (if* max-fwd
				    then (setq max-fwd (max max-fwd fwd)))))
			     t)
			    )
		    then ; failed to read, give up
			 (return-from process-entity nil))
	    else ; don't need to read, but keep running total
		 (incf total-size (length
				   (or (multi-item-cache item) "")))
		 (if* max-fwd 
		    then (setq max-fwd
			   (max max-fwd
				(or (multi-item-last-modified item) 0))))))
	(:function
	 (multiple-value-bind (new-value new-modified)
	     (funcall (multi-item-data item)
		      req
		      ent
		      (multi-item-last-modified item)
		      (multi-item-cache item))
	   (if* (stringp new-value)
	      then  (setq new-value (string-to-octets new-value
						      :null-terminate nil)))
	   (setf (multi-item-cache item) new-value)
	   (setf (multi-item-last-modified item) new-modified)
	   (if* (null new-modified) then (setq max-fwd nil))
	   (if* (and max-fwd (multi-item-last-modified item))
	      then (setf max-fwd (max max-fwd (multi-item-last-modified item))))
	   (incf total-size (length (or new-value "")))
	   ))
	((:string :binary)
	 ; a constant thing
	 (incf total-size (length (multi-item-cache item))))
	))
    
    (if* (not (eql (last-modified ent) max-fwd))
       then ; last modified has changed
	    (setf (last-modified ent) max-fwd)
	    (if* max-fwd
	       then (setf (last-modified-string ent)
		      (universal-time-to-date max-fwd))))
    
    (setf (multi-entity-content-length ent) total-size)
    
    ; now we have all the data
    (with-http-response (req ent :format :binary)
      (setf (request-reply-content-length req) total-size)
      (if* max-fwd
	 then (setf (reply-header-slot-value req :last-modified)
		(last-modified-string ent)))
      
      (run-entity-hook req ent nil)
      (with-http-body (req ent)
	(dolist (item (items ent))
	  (let ((cache (multi-item-cache item)))
	    (write-all-vector cache *html-stream*
			      :end (length cache))))))
    
    t ; processed
    ))		    
  
  

		
(defun up-to-date-check (doit req ent)
  ;; if doit is true and the request req has an
  ;; if-modified-since or if-unmodified-since then
  ;; check if it applies and this resuits in a response
  ;; we can return right away then do it and 
  ;; throw to abort the rest of the body being run
  
  ; to be done
  
  (if* (not doit)
     then ; we dont' even care
	  (return-from up-to-date-check nil))
  
  (let ((if-modified-since (header-slot-value req :if-modified-since)))
    (if* if-modified-since
       then (setq if-modified-since
	      (date-to-universal-time if-modified-since)))
    
    (if* if-modified-since
       then ; valid date, do the check
	    (if* (and (last-modified ent) 
		      (<= (last-modified ent) if-modified-since))
	       then ; send back a message that it is already
		    ; up to date
		    (let ((nm-ent *not-modified-entity*))
		      (debug-format :info "entity is up to date")
		      ; recompute strategy based on simple 0 length
		      ; thing to return
		      (compute-strategy req nm-ent nil)
		      ; if we had a stream to reply, forget it
		      (setf (request-reply-stream req) nil)
		      
		      (setf (request-reply-code req) *response-not-modified*)
		      (run-entity-hook req ent :not-modified)
		      (with-http-body (req nm-ent)
			;; force out the header
			)
		      (throw 'with-http-response nil) ; and quick exit
		      )))))

    
(defun keep-alive-specified (req)
  (if* (eq (request-protocol req) :http/1.1)
     then (not (header-value-member
                "close" (header-slot-value req :connection)))
     else (header-value-member
           "keep-alive" (header-slot-value req :connection))))


(defun compute-accepted-encodings (req &key as-keywords)
  ;; return a list of accepted encodings as strings removing
  ;; encoding parameters (such as q).
  ;; If as-keywords is specified then return the answer
  ;; as a list of keywords from the set that we support.
  (let ((str (header-slot-value req :accept-encoding)))
    (if* (stringp str)
       then (let ((string-ans (mapcar #'(lambda (param)
					  (if* (and (consp param)
						    (eq :param (car param)))
					     then (cadr param)
					   elseif (stringp param)
					     then param))
				      (parse-header-value str))))
	      (if* as-keywords
		 then ; convert to supported keywords
		      (let (res)
			(if* (member "deflate" string-ans :test #'equalp)
			   then (push :deflate res))
			(if* (member "gzip" string-ans :test #'equalp)
			   then (push :gzip res))
			res)
		 else string-ans)))))


(defun check-trailers-ok (req trailers)
  ;; determine if trailers are allowed for this request
  ;; and if so if the trailers arguement is correctly formed
  ;;
  ;;
  (let ((str (header-slot-value req :te)))
    (if* (and str (search "trailers" str))
       then (let (res)
	      (if* (consp trailers)
		 then (dolist (trailer trailers)
			(if* (or (stringp trailer)
				 (symbolp trailer))
			   then ; put in kwd package
				(push (cons (header-kwdize trailer) nil) res)
			 elseif (and (consp trailer)
				     (or (stringp (car trailer))
					 (symbolp (car trailer)))
				     (or (null (cdr trailer))
					 (stringp (cdr trailer))))
			   then (push (cons (header-kwdize (car trailer)) (cdr trailer))
				      res)
			   else (error "bad trailer ~s in trailers list ~s"
				       trailer trailers)))
	       elseif (null trailers)
		 thenret
		 else (error "bad trailers argument ~s" trailers))
	      
	      ;; we may have some trailers
	      (setf (request-reply-trailers req) res)))))

(defun header-kwdize (item)
  ;; convert to keyword if string, allow any symbol through..
  ;; all else is an error
  (if* (stringp item)
     then (case *current-case-mode*
	    ((:case-sensitive-lower :case-insensitive-lower)
	     (setq item (string-downcase item)))
	    ((:case-insensitive-upper :case-sensitive-upper)
	     (setq item (string-upcase item))))
	  (intern item (symbol-package :foo))
   elseif (symbolp item)
     then item
     else (error "non string or symbol: ~s" item)))

	    
	    
				
;; strategies
;;   The server has to let the client know when it's sent the entire
;;   body.  There are three ways of doing this:
;;   1. send the content-length in the header.   Only possible if we
;;      are sending a file, for computed entities not known until the
;;      computation is over
;;   2. chunking.  Only possible for HTTP/1.1.
;;   3. close the socket after the last byte was sent - if the client
;;	requested keep-alive then this is legal but not friendly.
;; 
;;
;;
;; inputs
;;   head or other request  - 
;;   keep-alive-possible - requested by client and we have resources
;;   content length specified or not
;;   transfer mode :binary, or not
;;   compression possible or not
;;
;;
;; head
;;     ([:keep-alive] :use-socket-stream :omit-body)
;;
;; http/1.1
;;   no content length
;;   chunking allowed
;;      ([:keep-alive] :chunked :use-socket-stream)
;;    
;; otherwise:
;;   keep-alive
;;	binary
;;         content-length
;;		(:keep-alive :use-socket-stream)
;;         no content length   (so close after sending)
;;		(:use-socket-stream)
;;	text
;;	    (:string-output-stream :keep-alive :post-headers)
;;   no keep-alive
;;	    (:use-socket-stream)
;;
;; If compression possible and :use-socket-stream is in strategy
;; then prepend this:
;;    (:compress  (:gzip | :deflate) ,@strategy)
;;
;;	
	    
    
(defmethod compute-strategy ((req http-request) (ent entity) format)
  ;; determine how we'll respond to this request

  (let ((strategy nil)
	(keep-alive-possible
	 (and (wserver-enable-keep-alive *wserver*)
	      (>= (wserver-free-workers *wserver*) 2)
              (keep-alive-specified req)))
	(compression-possible
	 (and (wserver-enable-compression *wserver*)
	      (entity-compress ent)
	      (let ((encs (compute-accepted-encodings req)))
		(if* (member "gzip" encs :test #'equalp)
		   then :gzip
		 elseif (member "deflate" encs :test #'equalp)
		   then :deflate)))))

    (if* (eq (request-method req) :head)
       then ; head commands are particularly easy to reply to
	    (setq strategy '(:use-socket-stream
			     :omit-body))
	    
	    (if* keep-alive-possible
	       then (push :keep-alive strategy))
	    
     elseif (and  ;; assert: get command
	     (wserver-enable-chunking *wserver*)
	     (eq (request-protocol req) :http/1.1)
	     (null (content-length ent)))
       then ;; http/1.1 so we can chunk
	    (if* keep-alive-possible
	       then (setq strategy '(:keep-alive :chunked :use-socket-stream))
	       else (setq strategy '(:chunked :use-socket-stream)))
       else ; can't chunk, let's see if keep alive is requested
	    (if* keep-alive-possible
	       then ; a keep alive is requested..
		    ; we may want reject this if we are running
		    ; short of processes to handle requests.
		    ; For now we'll accept it if we can.
		    
		    (if* (eq (or format (transfer-mode ent)) :binary)
		       then ; can't create binary stream string
			    ; see if we know the content length ahead of time
			    (if* (content-length ent)
			       then (setq strategy
				      '(:keep-alive :use-socket-stream))
			       else ; must not keep alive
				    (setq strategy
				      '(:use-socket-stream
					; no keep alive
					)))
		       else ; can build string stream
			    (setq strategy
			      '(:string-output-stream
				:keep-alive
				:post-headers)))
	       else ; keep alive not requested
		    (setq strategy '(:use-socket-stream
				     ))))

    (if* (and compression-possible
	      (member :use-socket-stream strategy :test #'eq)
	      (not (member :omit-body strategy :test #'eq))
	      ; if the content length is given then that
	      ; is pre-compression so it will be wrong if
	      ; we send compressed data
	      (null (content-length ent)) 
	      )
       then (push compression-possible strategy) ; must follow :compress
	    (push :compress strategy)
	    )
    
    ;;  save it

    (debug-format :info "strategy is ~s" strategy)
    (setf (request-reply-strategy req) strategy)
    
    ))
			     
		    
(defmethod compute-strategy ((req http-request) (ent file-entity) format)
  ;; for files we can always use the socket stream and keep alive
  ;; since we konw the file length ahead of time
  (declare (ignore format))
  (let ((keep-alive (and (wserver-enable-keep-alive *wserver*)
			 (>= (wserver-free-workers *wserver*) 2)
			 (keep-alive-specified req)))
	(strategy))
    
    (if*  (eq (request-method req) :get)
       then (setq strategy (if* keep-alive
			      then '(:use-socket-stream :keep-alive)
			      else '(:use-socket-stream)))
       else (setq strategy (call-next-method)))
    
    (debug-format :info "file strategy is ~s" strategy)
    (setf (request-reply-strategy req) strategy)))

	    

(defmethod compute-strategy :around ((req http-request) (ent computed-entity) format)
  (declare (ignore format))
  
  (let ((strategy (call-next-method)))
    
    (if* (and (member :use-socket-stream strategy)
	      (not (member :post-headers strategy)))
       then (setf (request-reply-strategy req)
	      (cons :delay-headers strategy))
       else strategy)))

  
  
  
		    
		    
    
(defmethod send-response-headers ((req http-request) (ent entity) time)
  ;;
  ;; called twice (from with-http-body) in the generation of a response 
  ;; to an http request
  ;; 1. before the body forms are run.  in this case time eq :pre
  ;; 2. after the body forms are run.  in this case  time eq :post
  ;;
  ;; we send the headers out at the time appropriate to the 
  ;; strategy.  We also deal with a body written to a
  ;; string output stream
  ;;
    
  (with-timeout-local (60 (logmess "timeout during header send")
			  ;;(setf (request-reply-keep-alive req) nil)
			  (throw 'with-http-response nil))
    (maybe-accumulate-log 
     (:xmit-server-response-headers
      (lambda (string)
	(log1 :xmit-server-response-headers :info
	      (list time string))))
     (with-standard-io-syntax
       (let* ((sock (request-socket req))
	      (hsock) ; to send headers
	      (reply-stream (request-reply-stream req))
	      (strategy (request-reply-strategy req))
	      (extra-headers (request-reply-headers req))
	      (post-headers (member :post-headers strategy :test #'eq))
	      (content)
	      (*print-readably* nil) ; set by w-s-io-syntax and not desired
	      (chunked-p (member :chunked strategy :test #'eq))
	      (compress (let ((ent (member :compress strategy :test #'eq)))
			  (cadr ent) ; will be :gzip or :deflate or nil
			  ))
	      (code (request-reply-code req))
	      (send-headers
	       (if* post-headers
		  then (eq time :post)
		  else (eq time :pre))
	       )
	      (sos-ef) ; string output stream external format
	      ;(ssl-p (wserver-ssl *wserver*))
	      )
       

 		
	 (setq hsock sock)
	 (if* (and send-headers (member :delay-headers strategy :test #'eq))
	    then ; must capture the headers in a string output stream
		 (setq hsock (make-string-output-stream)))
 	
       
	 (if* send-headers
	    then (format-dif :xmit-server-response-headers hsock "~a ~d  ~a~a"
			     (request-reply-protocol-string req)
			     (response-number code)
			     (response-desc   code)
			     *crlf*))
       
	 (if* (and post-headers
		   (eq time :post)
		   (member :string-output-stream strategy :test #'eq)
		   )
	    then ; must get data to send from the string output stream
		 (setq content 
		   (if* (request-reply-stream req)
		      then (setq sos-ef (stream-external-format 
					 (request-reply-stream req)))
			   (get-output-stream-string 
			    (request-reply-stream req))
 			
		      else ; no stream created since no body given
			   ""))
 	      
		 (if* (and sos-ef (not (eq (stream-external-format sock) sos-ef)))
		    then ; must do ext format conversion now
			 ; so we can compute the length
			 (setq content
			   (string-to-octets content :external-format sos-ef
					     :null-terminate nil)))
 	      
		 (setf (request-reply-content-length req) (length content)))
       	
 	(if* (and send-headers
 		  (not (eq (request-protocol req) :http/0.9)))
 	   then ; can put out headers
 		(format-dif :xmit-server-response-headers hsock "Date: ~a~a" 
 			    (maybe-universal-time-to-date (request-reply-date req))
 			    *crlf*)

 		(if* (member :keep-alive strategy :test #'eq)
 		   then (format-dif :xmit-server-response-headers
 				    hsock "Connection: Keep-Alive~aKeep-Alive: timeout=~d~a"
 				    *crlf*
 				    (wserver-header-read-timeout *wserver*)
 				    *crlf*)
 		   else (format-dif :xmit-server-response-headers
                                    hsock "Connection: Close~a" *crlf*))

 		(if* (not (assoc :server extra-headers :test #'eq))
 		   then ; put out default server info
 			(format-dif :xmit-server-response-headers
                                    hsock *server-fmt* 
 				    *aserve-version-string*
 				    *crlf*))
       
		 (if* (request-reply-content-type req)
		    then (format-dif :xmit-server-response-headers
				     hsock "Content-Type: ~a~a" 
				     (request-reply-content-type req)
				     *crlf*))

		 (if* chunked-p
		    then (format-dif :xmit-server-response-headers
				     hsock "Transfer-Encoding: chunked~a"
				     *crlf*))

		 (if* compress
		    then (format-dif :xmit-server-response-headers
				     hsock "Content-Encoding: ~a~a"
				     compress
				     *crlf*))
 			
		 (if* (and (not chunked-p)
			   (request-reply-content-length req))
		    then (format-dif :xmit-server-response-headers
				     hsock "Content-Length: ~d~a"
				     (request-reply-content-length req)      
				     *crlf*)
			 (debug-format :info
				       "~d ~s - ~d bytes" 
				       (response-number code)
				       (response-desc   code)
				       (request-reply-content-length req))
		  elseif chunked-p
		    then (if* (request-reply-trailers req)
			    then (format-dif :xmit-server-response-headers
					     hsock "Trailer: ~{~a ~}~a" 
					     (mapcar #'car (request-reply-trailers req))
					     *crlf*))
				
			 (debug-format :info "~d ~s - chunked" 
				       (response-number code)
				       (response-desc   code)
				       )
		    else (debug-format :info
				       "~d ~s - unknown length" 
				       (response-number code)
				       (response-desc   code)
				       ))

		 (dolist (head (request-reply-headers req))
		   (format-dif :xmit-server-response-headers
			       hsock "~a: ~a~a"
			       (car head)
			       (cdr head)
			       *crlf*))
		 (format hsock "~a" *crlf*)
		 (force-output hsock)
		 ; clear bytes written count so we can count data bytes
		 ; transferred
		 #+(and allegro (version>= 6))
		 (excl::socket-bytes-written hsock 0) 
		 )

	 (if* (and send-headers (member :delay-headers strategy :test #'eq))
	    then ; headers are now in a string output stream
 		
 		
		 (let ((header-content
			(string-to-octets
			 (get-output-stream-string hsock)
			 :null-terminate nil
			 :external-format :octets
			 )))
 		  
		   (setq reply-stream
		     (setf (request-reply-stream req)
		       (make-instance 'prepend-stream
			 :content header-content
			 :output-handle reply-stream)))))
 		    
 		
 	
 	
	 (if* (and send-headers chunked-p (eq time :pre))
	    then (force-output hsock)
		 ; do chunking
		 (setq reply-stream
		   (make-instance 'chunking-stream 
		     :output-handle reply-stream))
		 (setf (request-reply-stream req) 
		   reply-stream)
		 (setf (chunking-stream-trailers reply-stream)
		   (request-reply-trailers req))
		 )
	
 			
 		
       

	 (if* (and send-headers compress (eq time :pre))
	    then (setq reply-stream
		   (setf (request-reply-stream req)
		     (make-instance 'deflate-stream
		       :target reply-stream
		       :compress compress
		       ))))
 		
 		
	 ; if we did post-headers then there's a string input
	 ; stream to dump out.
	 (if* content
	    then (write-sequence content sock))

	 (if* (and (eq time :post) (prepend-stream-p reply-stream))
	    then (force-output reply-stream)
		 (close reply-stream) ; close prepend stream only
		 (setq reply-stream
		   (setf (request-reply-stream req)
		     (prepend-stream-inner-stream reply-stream))))
 		
	 (if* (and  compress (eq time :post))
	    then (if* reply-stream
		    then (force-output reply-stream)
 		
			 (close reply-stream)  ; close deflate stream
 		
			 (setq reply-stream
			   (setf (request-reply-stream req) (excl::inner-stream reply-stream)))
			 (and reply-stream (force-output reply-stream))))
 				      
	 ;; if we're chunking then shut that off
	 (if* (and chunked-p (eq time :post))
	    then ; unwrap the chunked stream
 		
		 (if* reply-stream
		    then (force-output reply-stream)
			 (close reply-stream) ; send chunking eof
			 (setq reply-stream
			   (setf (request-reply-stream req) (excl::inner-stream reply-stream)))
			 (and reply-stream (force-output reply-stream)))))))))

(defmethod compute-response-stream ((req http-request) (ent file-entity))
  ;; send directly to the socket since we already know the length
  ;;
  (setf (request-reply-stream req) (request-socket req)))

(defmethod compute-response-stream ((req http-request) (ent entity))
  ;; may have to build a string-output-stream
  (if* (member :string-output-stream (request-reply-strategy req) :test #'eq)
     then (setf (request-reply-stream req) (make-string-output-stream))
     else (setf (request-reply-stream req) (request-socket req))))

(defmethod compute-response-stream ((req http-request) (ent multi-entity))
    ;; send directly to the socket since we already know the length
  ;;
  (setf (request-reply-stream req) (request-socket req)))

(defvar *far-in-the-future*
    (encode-universal-time 12 32 12 11 8 2020 0))

(defmethod set-cookie-header ((req http-request)
			      &key name value expires domain 
				   (path "/")
				   secure http-only
				   (external-format 
				    *default-aserve-external-format*)
				   (encode-value t)
				   )
  ;; put a set cookie header in the list of header to be sent as
  ;; a response to this request.
  ;; name and value are required, they should be strings
  ;; name and value will be urlencoded.
  ;; If expires is nil (the default) then this cookie will expire
  ;;	when the browser exits.
  ;; If expires is :never then we'll sent a date so far into the future
  ;;  that this software is irrelevant
  ;; domain and path if given should be strings.
  ;; domain must have at least two periods (i.e. us  ".franz.com" rather
  ;; than "franz.com".... as netscape why this is important
  ;; secure is either true or false
  ;; http-only is either true or false
  ;;
  (let (res)
    
    (setq res
      (concatenate 'string 
	(uriencode-string (string name) :external-format external-format)
	"="
	(if* encode-value
	   then (uriencode-string (string value)
				  :external-format external-format)
	   else ; use value unencoded
		(string value))))
    
    (if* expires
       then (if* (eq expires :never)
	       then (setq expires *far-in-the-future*))
	    (if* (integerp expires)
	       then (setq res (concatenate 'string
				res
				"; expires="
				(universal-time-to-date expires)))
	       else (error "bad expiration date: ~s" expires)))
    
    (if* domain
       then (setq res (concatenate 'string
			res
			"; domain="
			(string domain))))
    
    (if* path
       then (setq res (concatenate 'string
			res
			"; path="
			(string path))))
    
    (if* secure
       then (setq res (concatenate 'string
			res
			"; secure")))
    
    (if* http-only
       then (setq res (concatenate 'string
			res
			"; HttpOnly")))
    
    ; add to the end
    (setf (request-reply-headers req)
      (nconc  (request-reply-headers req) (list `(:set-cookie . ,res))))
    res))


(defun get-cookie-values (req &key (external-format 
				    *default-aserve-external-format*))
  ;; return the set of cookie name value pairs from the current
  ;; request as conses  (name . value) 
  ;;
  (let ((cookie-string (header-slot-value req :cookie)))
    (if* (and cookie-string (not (equal "" cookie-string)))
       then ; form is  cookie: name=val; name2=val2; name2=val3
	    ; which is not exactly the format we want to see it in
	    ; to parse it.  we want   
	    ;     cookie: foo; name=val; name=val
	    ; we we'll dummy up something that we want to see. 
	    ; maybe later we'll have a parser for this form too
	    ;
	    (let ((res (parse-header-value
			(concatenate 'string "foo; " cookie-string))))
	      ; res should be: ((:param "foo" ("baz" . "bof")))
	      (if* (and (consp res)
			(consp (car res))
			(eq :param (caar res)))
		 then ; the correct format, must decode pieces
		      (mapcar #'(lambda (ent)
				  ; sometimes a param isn't name2=val2;
				  ; but is simply name2;.  pretend it
				  ; was name2=;
				  (if* (atom ent)
				     then (setq ent (cons ent "")))
				  (cons 
				   (uridecode-string
				    (car ent) :external-format external-format)
				   (uridecode-string
				    (cdr ent)
				    :external-format external-format)))
			      (cddr (car res))))))))
			

;-----------

(defmethod timedout-response ((req http-request) (ent entity))
  ;; return a response to a web server indicating that it is taking
  ;; too long for us to respond
  ;;
  (setf (request-reply-code req) *response-internal-server-error*)
  (with-http-body (req ent)
    (html (:title "Internal Server Error")
	  (:body "500 - The server has taken too long to respond to the request"))))

  
  

;; delayed send stream
;; 
;; 

(def-stream-class prepend-stream (single-channel-simple-stream)
  ((content :initform nil
	    ;; what to send before the first write
	    :initarg :content
	    :accessor prepend-stream-content))
  
  )


(defmethod prepend-stream-p (stream)
  (declare (ignore stream))
  nil)

(defmethod prepend-stream-p ((stream prepend-stream))
  t)

(defmethod print-object ((p prepend-stream) s)
  (print-unreadable-object (p s :identity t :type t)
    (format s "~a content" (if* (prepend-stream-content p)
			      then "has"
			      else "no"))))


(defmethod device-open ((p prepend-stream) dummy options)
  (declare (ignore dummy))
  
  (let ((output-handle (getf options :output-handle)))
    
    (setf 
	(slot-value p 'excl::buffer) (make-array 4096 :element-type '(unsigned-byte 8))
	  
	(slot-value p 'excl::buffer-ptr) 0
	(slot-value p 'excl::output-handle) output-handle
	
	(slot-value p 'excl::control-out) excl::*std-control-out-table*
	
	)
    
    (setf (stream-external-format p)
      (stream-external-format p))
  
    (add-stream-instance-flags p :output :simple)
    
    t))


(defmethod device-write ((p prepend-stream) buffer start end blocking)
  (declare (ignore blocking))
   
  (let ((content (prepend-stream-content p))
	(output-handle (slot-value p 'excl::output-handle)))
    (if* content
       then (setf (prepend-stream-content p) nil)
	    (write-full-vector content output-handle))
    
    (if* (> end start) 
       then (write-full-vector (or buffer (slot-value p 'excl::buffer))
			       output-handle
			       :start start
			       :end end)
       else start)))
	    

(defmethod device-close ((p prepend-stream) abort)
  ;; close only this stream, not the inner stream
  (if* (not abort)
     then (force-output p)
	  ; just to ensure header is sent
	  (device-write p nil 0 0 nil))
  
  t)

(defmethod prepend-stream-inner-stream ((p prepend-stream))
  (slot-value p 'excl::output-handle))

(defun write-full-vector (vec stream &key (start 0) (end (length vec)))
  (loop
    (if* (>= start end)
       then (return start)
       else (setq start (write-vector vec stream 
				      :start start
				      :end end)))))


(defmethod excl::socket-bytes-written ((stream prepend-stream) &optional set)
  ;; pass it on to the inner stream
  (excl::socket-bytes-written (slot-value stream 'excl::output-handle)
			      set))




;;;;;;;;;;;;;;; setup things

(if* (not (boundp '*wserver*))
   then ; create initial wserver object
	(setq *wserver* (make-instance 'wserver)))




  

	  
      



		    
		     
		     
		    
		  
    
