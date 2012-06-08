# On Windows, this makefile requires the use of GNU make from Redhat
# (http://sources.redhat.com/cygwin/).

SHELL = sh

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

use_dcl = $(shell if test -f ../dcl.dxl; then echo yes; else echo no; fi)

ifeq ($(use_dcl),yes)
mlisp = ../lisp
image = dcl.dxl
endif

ifeq ($(on_windows),yes)
mlisp ?= "/cygdrive/c/acl82/mlisp.exe"
else
mlisp ?= /fi/cl/8.2/bin/mlisp
endif

image ?= mlisp.dxl

ifeq ($(on_windows),yes)
mlisp += +B +cn +P # +M
endif
mlisp += -I $(image)

# -batch must come before -L, since arguments are evaluated from left to right
mlisp += -batch

build: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "load.cl")' >> build.tmp
	echo '(make-aserve.fasl)' >> build.tmp
	$(mlisp) -L build.tmp -kill

# Can be used to change the number of parallel test runs:
#NSERVERS = :n 1

test.tmp: FORCE
	rm -f test.tmp
	echo '(dribble "test.out")' >> test.tmp
	echo '(setq excl::*break-on-warnings* t)' >> test.tmp
	echo '(setq util.test::*break-on-test-failures* t)' >> test.tmp
	echo '(load "load.cl")' >> test.tmp
	echo '(setq user::*do-aserve-test* nil)' >> test.tmp
	echo '(load "test/t-aserve.cl")' >> test.tmp

test: test.tmp
	echo '(time (test-aserve-n :n 1 :exit t))' >> test.tmp
	$(mlisp) -L test.tmp -kill

testsmp: test.tmp
	echo '(time (test-aserve-n $(NSERVERS) :exit t))' >> test.tmp
	$(mlisp) -L test.tmp -kill

stress: test.tmp
	echo '(time (test-aserve-n $(NSERVERS) :exit t))' >> test.tmp
	../bin/repeat.sh 10 $(mlisp) -L test.tmp -kill

test-from-asdf: FORCE
	rm -f build.tmp
	echo '(dribble "test.out")' >> build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(require :tester)' >> build.tmp
	echo '(setq util.test::*break-on-test-failures* t)' >> build.tmp
	echo '(require :asdf)' >> build.tmp
	echo "(asdf:operate 'asdf:load-op :aserve)" >> build.tmp
	echo '(time (load "test/t-aserve.cl"))' >> build.tmp
	echo '(exit util.test::*test-errors*)' >> build.tmp
	$(mlisp) -L build.tmp -kill

srcdist: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "load.cl")' >> build.tmp
	echo '(make-src-distribution "aserve")' >> build.tmp
	$(mlisp) -L build.tmp -kill

clean:	FORCE
	rm -f *.tmp
	find . -name '*.fasl' -print | xargs rm -f

cleanall distclean: clean
	rm -fr aserve-src

tags: FORCE
	rm -f TAGS
	find . -name '*.cl' -print | xargs etags -a

FORCE:
