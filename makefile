# On Windows, this makefile requires the use of GNU make from Redhat
# (http://sources.redhat.com/cygwin/).

# Export everything
export

SHELL = bash

## First, so it can set variables and even change the default rule
makefile_local = $(shell if test -f makefile.local;then echo makefile.local;fi)
ifneq ($(makefile_local),)
include $(makefile_local)
endif

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

use_dcl = $(shell if test -f ../dcl.dxl; then echo yes; else echo no; fi)

ifeq ($(use_dcl),yes)

on_macOS = $(shell if test `uname -s` = Darwin; then echo yes; fi)

ifeq ($(on_macOS),yes)
mlisp_env = MACHINE=x86 OS_NAME=darwin source ../scm-bin/aclbuildenv.sh &&
endif

mlisp = $(mlisp_env) ../lisp
image = dcl.dxl
endif

ifeq ($(on_windows),yes)
mlisp ?= "/cygdrive/c/acl100/mlisp.exe"
else
mlisp ?= /fi/cl/10.0/bin/mlisp
endif

image ?= mlisp.dxl

ifeq ($(on_windows),yes)
mlisp += +B +cn +P # +M
endif
mlisp += -I $(image)

# -batch must come before -L, since arguments are evaluated from left to right
mlisp += -batch -backtrace-on-error

build: FORCE
	rm -f build.tmp
	echo '(setq *record-source-file-info* t)' >> build.tmp
	echo '(setq *load-source-file-info* t)' >> build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "load.cl")' >> build.tmp
	echo '(make-aserve.fasl)' >> build.tmp
	$(mlisp) -L build.tmp -kill

# Can be used to change the number of parallel test runs:
#NSERVERS = :n 1

COMPILE_TESTS = yes

test.tmp: FORCE
	rm -f test.tmp
	echo '(dribble "test.out")' >> test.tmp
	echo '(setq excl::*break-on-warnings* t)' >> test.tmp
	echo '(require :tester)' >> test.tmp
	echo '(setq util.test::*break-on-test-failures* t)' >> test.tmp
	echo '(load "load.cl")' >> test.tmp
	echo '(setq user::*do-aserve-test* nil)' >> test.tmp
ifeq ($(COMPILE_TESTS),yes)
	echo '(load (compile-file "test/t-aserve.cl"))' >> test.tmp
	echo '(load (compile-file "webactions/test/t-webactions.cl"))' >> test.tmp
else
	echo '(load "test/t-aserve.cl")' >> test.tmp
	echo '(load "webactions/test/t-webactions.cl")' >> test.tmp
endif

# Run tests with default setting of *hiper-socket-is-stream-socket* switch.
test: test.tmp
	echo '(time (test-aserve-n :n 1 :exit nil))' >> test.tmp
	echo '(time (net.aserve.testwa::test-webactions))'  >> test.tmp
	$(mlisp) -L test.tmp -kill

test-do-hiper: test.tmp
	echo '(setq excl::*hiper-socket-is-stream-socket* nil)' >> test.tmp
	echo '(time (test-aserve-n :n 1 :exit nil))' >> test.tmp
	$(mlisp) -L test.tmp -kill

test-no-hiper: test.tmp
	echo '(setq excl::*hiper-socket-is-stream-socket* t)' >> test.tmp
	echo '(time (test-aserve-n :n 1 :exit nil))' >> test.tmp
	$(mlisp) -L test.tmp -kill

testsmp: test.tmp
	echo '(time (test-aserve-n $(NSERVERS) :exit nil))' >> test.tmp
	$(mlisp) -L test.tmp -kill

stress: test.tmp
	echo '(net.aserve::debug-on :notrap)' >> test.tmp
	echo '(time (test-aserve-n $(NSERVERS) :exit nil))' >> test.tmp
	../bin/repeat.sh 10 $(mlisp) -L test.tmp -kill

stresswp: test.tmp
	echo '(net.aserve::debug-on :notrap)' >> test.tmp
	echo '(setq excl::*break-on-warnings* :pause)' >> test.tmp
	echo '(time (test-aserve-n $(NSERVERS) :exit nil))' >> test.tmp
	../bin/repeat.sh 10 $(mlisp) -L test.tmp -kill

test-from-asdf: FORCE
	rm -f build.tmp
	echo '(dribble "test.out")' >> build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(require :tester)' >> build.tmp
	echo '(setq util.test::*break-on-test-failures* t)' >> build.tmp
	echo '(require :asdf)' >> build.tmp
	echo "(asdf:operate 'asdf:load-op :aserve)" >> build.tmp
ifeq ($(COMPILE_TESTS),yes)
	echo '(time (load (compile-file "test/t-aserve.cl")))' >> build.tmp
	echo '(time (load (compile-file "webactions/test/t-webactions.cl")))' >> build.tmp
else
	echo '(time (load "test/t-aserve.cl"))' >> build.tmp
	echo '(time (load "webactions/test/t-webactions.cl"))' >> build.tmp
endif
	echo '(exit util.test::*test-errors*)' >> build.tmp
	$(mlisp) -L build.tmp -kill

srcdist: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "load.cl")' >> build.tmp
	echo '(make-src-distribution "aserve")' >> build.tmp
	$(mlisp) -L build.tmp -kill

clean:	FORCE
	rm -f *.tmp *.gz
	find . -name '*.fasl' -print | xargs rm -f

cleanall distclean: clean
	rm -fr aserve-src

tags: FORCE
	rm -f TAGS
	find . -name '*.cl' -print | xargs etags -a

doclinks:
	cd doc && ./doclinks.py

FORCE:

## last, for including new rules which are not the default
makefile_last = $(shell if test -f makefile.last;then echo makefile.last;fi)
ifneq ($(makefile_last),)
include $(makefile_last)
endif
