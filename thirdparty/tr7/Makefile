# Makefile for TR7

# version
MAJOR = 2
MINOR = 0
REVISION = 15
VERSION = $(MAJOR).$(MINOR).$(REVISION)
MINVERSION = $(MAJOR).$(MINOR)

# targets
.PHONY: all test chkopt clean updtref libtr7.so

# tools
CC ?= cc
AR ?= ar
RM ?= rm
LN ?= ln
LD ?= ld

# compilation flags
# SFLAGS  source conformity flags
# GFLAGS  generation flags
# WFLAGS  warming flags
# OFLAGS  optimisation/debug flags
# DFLAGS  debugging flags
# FFLAGS  feature flags
# EFLAGS  extra flags
# MFLAGS  internal make flags
# LDFLAGS linker flags
SFLAGS = -std=c11 -D_POSIX_C_SOURCE=200809L -pedantic
GFLAGS = -fno-asynchronous-unwind-tables -fomit-frame-pointer
WFLAGS = -Wall -Wextra -Wno-unused-parameter -Wsign-conversion
OFLAGS = -O3
DFLAGS = -g
FFLAGS =
EFLAGS =
MFLAGS = '-DVERSION="${VERSION}"' '-DMINVERSION="${MINVERSION}"'
LDFLAGS = -Wl,--gc-sections,-z,relro,-z,now
# Detect darwin linker, which is not compatible with gnu linker
ifneq ($(findstring PROJECT,$(shell $(LD) -v 2>&1)),)
	LDFLAGS = -Wl,-dead_strip,-pie
endif
SYSLIBS += -lm

# name of binary targets
LIBSTA = libtr7.a
LIBSO = libtr7.so.$(MAJOR)
TR7I = tr7i

# installing
DESTDIR =
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib
INCDIR = $(PREFIX)/include
DATADIR = $(PREFIX)/share
TR7LIBPATH = $(PREFIX)/share/tr7/tr7libs:$(PWD)/tr7libs

# default targets
ALL = $(LIBSTA) $(TR7I) tags

# uncomment below to compile shared library
#ALL += $(LIBSO)

# uncomment below to add load of extensions
#MFLAGS += -DUSE_TR7_EXTENSION=1
#SYSLIBS += -ldl

# uncomment below to dump lambdas
#FFLAGS += -DDUMP_LAMBDAS=1

# set CFLAGS now
CFLAGS := $(CFLAGS) $(SFLAGS) $(GFLAGS) $(WFLAGS) $(OFLAGS) $(DFLAGS) $(FFLAGS) $(EFLAGS) $(MFLAGS)

# rules
all: $(ALL)

$(LIBSTA): tr7.sta.o
	$(AR) crs $(LIBSTA) tr7.sta.o

libtr7.so: $(LIBSO)
	$(LN) -sf $(LIBSO) libtr7.so

$(LIBSO): tr7.dyn.o
	$(CC) -shared -o $(LIBSO) tr7.dyn.o $(SYSLIBS) -Wl,-soname,$(LIBSO)

$(TR7I): examples/tr7i.c tr7.c tr7.h
	$(CC) $(CFLAGS) -I. -o $(TR7I) examples/tr7i.c tr7.c $(LDFLAGS) $(SYSLIBS) -DDEFLIBPATH='"$(TR7LIBPATH)"'

tr7.sta.o: tr7.c tr7.h
	$(CC) $(CFLAGS) -c -o tr7.sta.o  tr7.c -ffunction-sections

tr7.dyn.o: tr7.c tr7.h
	$(CC) $(CFLAGS) -c -o tr7.dyn.o tr7.c -fPIC

clean:
	$(RM) -f tr7i libtr7.* *.o */*.o tests/*.result tr7.html demo-c

tags: tr7.h tr7.c examples/tr7i.c
	ctags tr7.h tr7.c examples/tr7i.c || true
	etags tr7.h tr7.c examples/tr7i.c || true

#---------------------
tr7.html: $(TR7I) examples/genedoc.scm tr7.h README.md
	./$(TR7I) -1 examples/genedoc.scm README.md tr7.h | sed s/TR7VERSION/$(VERSION)/ > tr7.html

#---------------------
# testing

TSTEXCL = 

test: $(TR7I)
	TSTEXCL=$(TSTEXCL) tests/test-tr7.sh

# for updating reference of tests
updtref:
	tests/update-references.sh

#---------------------
# C demo examples

demo-c: examples/demo-c.c $(LIBSTA)
	$(CC) $(CFLAGS) -I. -o demo-c examples/demo-c.c $(LIBSTA) $(LDFLAGS) $(SYSLIBS)

#---------------------
# install

.PHONY: install install-bin install-dev install-doc install-dyn install-all

install: install-bin install-dev install-doc

install-bin: $(TR7I)
	install -D -t $(DESTDIR)$(BINDIR) $(TR7I)
	install -d $(DESTDIR)$(DATADIR)/tr7
	cp -r tr7libs $(DESTDIR)$(DATADIR)/tr7

install-dev: $(LIBSTA) tr7.h
	install -D -t $(DESTDIR)$(LIBDIR) $(LIBSTA)
	install -D -t $(DESTDIR)$(INCDIR) tr7.h

install-doc: tr7.html
	install -D -t $(DESTDIR)$(DATADIR)/tr7 tr7.html README.md

install-all: install install-dyn

install-dyn: libtr7.so $(LIBSO) tr7.h
	install -D -t $(DESTDIR)$(LIBDIR) libtr7.so $(LIBSO)
	install -D -t $(DESTDIR)$(INCDIR) tr7.h


#---------------------
# GNUMake compatible only
# for checking that all flags are compiling
allopts = \
	USE_SCHEME_CASE_LAMBDA     \
	USE_SCHEME_CHAR            \
	USE_SCHEME_COMPLEX         \
	USE_SCHEME_CXR             \
	USE_SCHEME_EVAL            \
	USE_SCHEME_FILE            \
	USE_SCHEME_INEXACT         \
	USE_SCHEME_LAZY            \
	USE_SCHEME_LOAD            \
	USE_SCHEME_PROCESS_CONTEXT \
	USE_SCHEME_READ            \
	USE_SCHEME_REPL            \
	USE_SCHEME_TIME            \
	USE_SCHEME_WRITE           \
	USE_SCHEME_R5RS            \
	USE_SRFI_136               \
	USE_TR7_ENVIRONMENT        \
	USE_TR7_EXTENSION          \
	USE_TR7_GC                 \
	USE_TR7_DEBUG              \
	USE_TR7_TRACE              \
	DUMP_LAMBDAS               \
	SHOW_OPCODES               \
	USE_ASCII_NAMES            \
	USE_MATH                   \
	WITH_SHARP_HOOK            \
	AUTO_SHARP_TO_SYMBOL       \
	STACKED_GC

chkallopts = $(addsuffix .ck, $(allopts))

chkopt: $(chkallopts)

chkoptflags = -O0
#chkoptflags = -O2 -Wunused-function


%.ck:
	@echo ---------------------------------------------
	@echo ++ COMPILING FOR $(subst .ck,,$@)=0
	$(CC) -c -o tr7.ck.o tr7.c -D$(subst .ck,,$@)=0 $(chkoptflags)
	@echo ++ COMPILING FOR $(subst .ck,,$@)=1
	$(CC) -c -o tr7.ck.o tr7.c -D$(subst .ck,,$@)=1 $(chkoptflags)

