# $Id: makefile,v 1.3.44.4 2001/09/20 15:29:29 layer Exp $
#
# On Windows, this makefile requires the use of GNU make from Redhat
# (http://sources.redhat.com/cygwin/).

SHELL = sh

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

ifndef mlisp
ifeq ($(on_windows),yes)
acldir = /cygdrive/c/Program Files/ACL61
mlisp = "$(acldir)/mlisp.exe" +B +cn
else
acldir = /usr/local/acl61
mlisp = $(acldir)/mlisp
endif
endif

build: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "load.cl")' >> build.tmp
# -batch must come before -L, since arguments are evaluated from left to right
	$(mlisp) -batch -L build.tmp -kill

srcdist: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "load.cl")' >> build.tmp
	echo '(make-src-distribution "aserve")' >> build.tmp
# -batch must come before -L, since arguments are evaluated from left to right
	$(mlisp) -batch -L build.tmp -kill

clean:	FORCE
	rm -fr *.fasl */*.fasl

cleanall: clean
	rm -fr aserve-src

FORCE:
