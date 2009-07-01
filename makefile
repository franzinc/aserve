# $Id: makefile,v 1.15 2008/02/17 06:16:54 layer Exp $
#
# On Windows, this makefile requires the use of GNU make from Redhat
# (http://sources.redhat.com/cygwin/).

SHELL = sh

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

use_dcl = $(shell if test -f ../dcl.dxl; then echo yes; else echo no; fi)

ifeq ($(use_dcl),yes)
mlisp = ../lisp -I dcl.dxl
endif

ifndef mlisp
ifeq ($(on_windows),yes)
mlisp = "/cygdrive/c/Program Files/acl80/mlisp.exe" +B +cn
else
mlisp = /fi/cl/8.1/bin/mlisp
endif
endif

build: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "load.cl")' >> build.tmp
	echo '(make-aserve.fasl)' >> build.tmp
# -batch must come before -L, since arguments are evaluated from left to right
	$(mlisp) -batch -L build.tmp -kill

test: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "load.cl")' >> build.tmp
	echo '(dribble "test.out")' >> build.tmp
	echo '(load "test/t-aserve.cl")' >> build.tmp
	echo '(exit util.test::*test-errors*)' >> build.tmp
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
	rm -f build.tmp
	find . -name '*.fasl' -print | xargs rm -f

cleanall distclean: clean
	rm -fr aserve-src

tags: FORCE
	rm -f TAGS
	find . -name '*.cl' -print | xargs etags -a

FORCE:
