TR7: tiny R7RS-small scheme interpreter
=======================================

> "La perfection est atteinte, non pas lorsqu'il n'y a plus rien à ajouter,
>  mais lorsqu'il n'y a plus rien à retirer."
>
>   -- Antoine de Saint-Exupéry --

> "Perfection is achieved, not when there is nothing left to add,
>  but when there is nothing left to take away."

*************************************************************************

What is TR7?
------------

TR7 is a lightweight Scheme interpreter that implements the revision
R7RS small of scheme programming language.

It is meant to be used as an embedded scripting interpreter for
other programs.  A lot of functionality in TR7 is included conditionally,
to allow developers freedom in balancing features and footprint.

This version, vTR7VERSION, is complete and efficient.
Though released with a ***WORK IN PROGRESS*** documentation.

Effort is now put on documentation, so the next version,
will have a good documentation. Stay tuned.

For the known issues, see https://gitlab.com/jobol/tr7/-/issues.

Links
-----

For TR7:

- **Home and documentation** at https://jobol.gitlab.io/tr7/
- **Download TR7** at https://gitlab.com/jobol/tr7
- **Report of issue** at https://gitlab.com/jobol/tr7
- **Mailing list**: https://listes.lautre.net/cgi-bin/mailman/listinfo/tr7

Scheme:

- **R7RS-small**:      https://small.r7rs.org/
- **SRFI**:            https://small.r7rs.org/
- **R7RS benchmark**:  https://ecraven.github.io/r7rs-benchmarks/

Alternatives:

- **GNU guile**:       https://www.gnu.org/software/guile/
- **S7**:              https://ccrma.stanford.edu/software/snd/snd/s7.html
- **Implementations**: http://community.schemewiki.org/?scheme-faq-standards#implementations
- **Comparison**:      https://irvise.xyz/Blog/scheme-implementation-comparison.html

Very quick start
----------------

### get the sources

To get the source code either use `git` or
download the latest version.

Using `git`:
~~~~~~~~~~~~~~~~~~~~ sh
git clone https://gitlab.com/jobol/tr7.git
cd tr7
~~~~~~~~~~~~~~~~~~~~

Using `curl` and `tar` for latest commits
~~~~~~~~~~~~~~~~~~~~ sh
VER=vTR7VERSION
URL=https://gitlab.com/jobol/tr7/-/archive/$VER/tr7-$VER.tar.gz
curl $URL | tar zx
cd tr7-$VER
~~~~~~~~~~~~~~~~~~~~

### compile tr7i

To compile TR7 you need:

- a C compiler
- optionally, `make` or `bmake`

Compile using `make`
~~~~~~~~~~~~~~~~~~~~ sh
make
~~~~~~~~~~~~~~~~~~~~

Compile directly with your compiler `CC`
~~~~~~~~~~~~~~~~~~~~ sh
CC -I. -o tr7i examples/tr7i.c tr7.c -lm
~~~~~~~~~~~~~~~~~~~~

### run TR7's REPL

The program implementing REPL is `tr7i`.
It's source code is in `examples/tr7i.c`.
~~~~~~~~~~~~~~~~~~~~ sh
./tr7i
Tiny R7RS interpreter TR7 2.0.0
tr7> (features)
(r7rs tr7 tr7-2.0.0 tr7-use-math tr7-srfi-136 tr7-trace tr7-debug)
tr7> (exit)
~~~~~~~~~~~~~~~~~~~~


Content and building
--------------------

The main directory contains:

  file/dir          | description
 -------------------|---------------------------------------------
  README.txt        | this text
  COPYING           | BSD-0 license
  TODO              | note of what is to be done
  Makefile          | the make file
  r7rs.pdf          | R7RS small specification
  tr7.c             | the scheme engine TR7
  tr7.h             | header for using the scheme engine
  tr7libs/          | some available libraries (see below)
  tr7libs/srfi      | some available SRFI (see below)
  tr7libs/scheme    | some available scheme libraries (see below)
  tests/            | some tests
  examples/         | some examples
  examples/tr7i.c   | standalone scheme REPL program using the engine
  examples/demo-c.c | demo of integration of TR7 in C programs

Compilation can be done using `make` or `bmake`.
The targets of the `Makefile` are:

  Target         | description
 ----------------|-------------------------------------
  all            | makes tr7i and libtr7.a
  tr7i           | a tiny scheme interpreter using the engine
  libtr7.a       | static library containing the engine
  libtr7.so      | dynamic library containing the engine
  test           | make tr7i and run default tests on it
  tr7.html       | make html page using [markdeep](https://casual-effects.com/markdeep)
  demo-c         | make the C integration demo program
  clean          | remove any generated files

When compiling TR7, some libraries or parts of the code
can be removed or added using C pre-processor defines.

For example, for removing `(scheme lazy)` the macro
`USE_SCHEME_LAZY` must be set to `0`.
This is achieved by adding the option `-DUSE_SCHEME_LAZY=0`.

When using `make`, the `Makefile` defines variables
for tuning the build. These variables are:

 variable | description
 ---------|-------------
  SFLAGS  | source conformity flags
  GFLAGS  | generation flags
  WFLAGS  | warming flags
  OFLAGS  | optimisation/debug flags
  DFLAGS  | debugging flags
  FFLAGS  | feature flags
  EFLAGS  | extra flags
  LDFLAGS | linker flags

So for example, the following command:
~~~~~~~~~~~~~~~~~~ sh
NOTR7="-DUSE_TR7_EXTENSION=0 -DUSE_TR7_EXTENSION=0"
NOTR7="$NOTR7 -DUSE_TR7_GC=0 -DUSE_TR7_TRACE=0 -DUSE_TR7_DEBUG=0"
make FFLAGS="$NOTR7" OFLAGS=-Os EFLAGS="-Wl,--strip-all" -B tr7i
ls -l tr7i
~~~~~~~~~~~~~~~~~~

removes all TR7 specific features, compiles optimizing the size
and strip all symbols.



Scheme Reference
----------------

TR7 fully implements R7RS small (see `r7rs.pdf` file or
[r7rs-small](https://small.r7rs.org/)).

R7RS doesn't enforce the following features that are currently missing:

- immutable pairs

- big integers: to be implemented. Because it is not
  implemented, the arithmetic on integer is limited.
  At the moment, arithmetic is wrapping around.
  Detection of overflows is possible on option but the
  treatment of overflows is to use doubles, not integers

- rationals: to be implemented

  Use `(cond-expand ((ratios) ...` for checking.

- complex numbers: to be implemented

  Use `(cond-expand ((library (scheme complex)) ...` for checking

- implementation of exactness is basic: double versus int.

- jiffies precision is 1 second

  Help wanted for improving the accuracy on Windows(c)

If something seems to be missing or buggy, please use gitlab's
[tracker](https://gitlab.com/jobol/tr7/-/issues).

TR7 has a built-in implementation of
[SRFI-136](https://srfi.schemers.org/srfi-136/srfi-136.html).

Current builtin libraries from R7RS-small and other:

   library                   | availability
 ----------------------------|------------------------------
   (scheme base)             | always
   (scheme case-lambda)      | yes except if USE_SCHEME_CASE_LAMBDA=0
   (scheme char)             | yes except if USE_SCHEME_CHAR=0
   (scheme complex)          | no, not at the moment
   (scheme cxr)              | yes except if USE_SCHEME_CXR=0
   (scheme eval)             | yes except if USE_SCHEME_EVAL=0
   (scheme file)             | yes except if USE_SCHEME_FILE=0
   (scheme inexact)          | yes except if USE_SCHEME_INEXACT=0
   (scheme lazy)             | yes except if USE_SCHEME_LAZY=0
   (scheme load)             | yes except if USE_SCHEME_LOAD=0
   (scheme process-context)  | yes except if USE_SCHEME_PROCESS_CONTEXT=0
   (scheme read)             | yes except if USE_SCHEME_READ=0
   (scheme repl)             | yes except if USE_SCHEME_REPL=0
   (scheme time)             | yes except if USE_SCHEME_TIME=0
   (scheme write)            | yes except if USE_SCHEME_WRITE=0
   (scheme box)              | yes except if USE_SCHEME_BOX=0
   (srfi 136)                | yes except if USE_SRFI_136=0
   (tr7 misc)                | yes except if USE_TR7_MISC=0
   (tr7 extra)               | yes except if USE_TR7_EXTRA=0
   (tr7 environment)         | yes except if USE_TR7_EXTENSION=0
   (tr7 extension)           | no except if USE_TR7_EXTENSION is not 0
   (tr7 gc)                  | yes except if USE_TR7_GC=0
   (tr7 trace)               | yes except if USE_TR7_TRACE=0
   (tr7 debug)               | yes except if USE_TR7_DEBUG=0
   (tr7 tagged-closures)     | yes except if USE_TR7_TAGGED_CLOSURES=0


TR7 also provides some utility libraries in the directory `tr7libs`.

  library         | description
 -----------------|------------------------------------
  (srfi 1)        | lists library, [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html)
  (srfi 26)       | notation for specializing parameters without currying, [SRFI-26](https://srfi.schemers.org/srfi-26/srfi-26.html)
  (srfi 35)       | conditions, [SRFI-35](https://srfi.schemers.org/srfi-35/srfi-35.html)
  (srfi 69)       | basic hash tables, [SRFI-69](https://srfi.schemers.org/srfi-69/srfi-69.html)
  (srfi 111)      | boxes, [SRFI-111](https://srfi.schemers.org/srfi-111/srfi-111.html)
  (srfi 145)      | assumptions, [SRFI-145](https://srfi.schemers.org/srfi-145/srfi-145.html)
  (srfi 214)      | flexvectors, [SRFI-214](https://srfi.schemers.org/srfi-214/srfi-214.html)
  (srfi 232)      | flexible curried, [SRFI-232](https://srfi.schemers.org/srfi-232/srfi-232.html)
  (srfi 259)      | Tagged procedures with type safety [SRFI-259](https://srfi.schemers.org/srfi-259/srfi-259.html)
  (scheme list)   | lists library, equals (srfi 1), R7RS-large
  (scheme box)    | boxes library, equals (srfi 111), R7RS-large (but also builtin see above)
  (scheme r5rs)   | compatibility to R5RS, from R7RS-small (not built-in)

TR7 also provides few helper functions in more than the standards:

   library            | helpers
 ---------------------|------------------------------
   (scheme base)      | string-hash, hash, hash-by-identity (SRFI-69)
   (scheme char)      | string-ci-hash (SRFI-69), char-unicode?


Programmer's Reference
----------------------

The source code **examples/tr7i.c** can be used as a demo of integration.

The interpreter state is initialized with 'tr7_engine_create'.
Custom memory allocation routines can be installed by filling
the data of a 'tr7_config_t' structure passed to 'tr7_engine_create'.

Files can be loaded with 'tr7_load_file' or `tr7_run_file`,
strings containing Scheme code can be loaded with 'tr7_load_string'
or `tr7_run_string`.

The interpreter state should be deinitialized with 'tr7_engine_destroy'

The source code **examples/c.c** is showing usage of some function of
the c interface. It can be compiled with make:

~~~~~~~~~~~~~~~~~~~~~~ sh
> make demo-c
> ./demo-c
~~~~~~~~~~~~~~~~~~~~~~


Foreign Functions
-----------------

The user can add to the current scheme environment callable procedures
written in C (or accessible through C). For example, the function `square`
that squares its argument can be written as below:

~~~~~~~~~~~~~~~~~~~~~ c
tr7_C_return_t square(tr7_engine_t tsc, int nargs, tr7_t *args, void *closure)
{
   double x = tr7_to_double(args[0]);
   tr7_t result = tr7_from_double(tsc, x*x);
   return tr7_C_return_single(tsc, result);
}
~~~~~~~~~~~~~~~~~~~~~

These C functions are defined using structures of type `tr7_C_func_def_t`
as below:

~~~~~~~~~~~~~~~~~~~~~ c
static const tr7_C_func_def_t cexports[] =
{
   ...
   {
      .name = "square",           /* name of the function */
      .func = square,             /* pointer to the function */
      .closure = NULL,            /* closure to the function */
      .typeargs = TR7ARG_STRING,  /* description of arguments' types */
      .min_args = 1,              /* minimal argument count */
      .max_args = 1               /* maximal argument count */
   },
   ...
};
~~~~~~~~~~~~~~~~~~~~~

The valid count of arguments must be given and, optionally, their expected types
can also be described.
These values are checked and the function is called only if the effective
arguments are matching the criteria. Otherwise an error is raised.

The C functions are added to the current environment using the function
`tr7_register_C_functions` as below:

~~~~~~~~~~~~~~~~~~~~~ c
static void init(tr7_engine_t tsc)
{
   ...
   tr7_register_C_functions(tsc, cexports, sizeof cexports / sizeof *cexports);
   ...
}
~~~~~~~~~~~~~~~~~~~~~

DLLs/shared-objects dynamically loaded when **USE_TR7_EXTENSION**
is set to 1 must contain a static array of `tr7_C_func_def_t` (as `cexports` above)
named `_tr7_C_functions_` and whose last element's name is NULL.


tr7i
----

`tr7i` is the main example of embedding TR7 engine.
It just wrap argument processing on top of TR7's REPL.

When called with `-?` it gives the following help:

~~~~~~~~~~~~~~~~~~~~~ sh
$ ./tr7i -?
Usage: ./tr7i -?
or:    ./tr7i [<file1> <file2> ...]
followed by
          -1 <file> [<arg1> <arg2> ...]
          -c <Scheme code> [<arg1> <arg2> ...]
Use - as filename for stdin.
~~~~~~~~~~~~~~~~~~~~~

`tr7i` command line is inherited of its ancestor TinyScheme.

The usage is as follow:

- `tr7i`: start the interactive REPL

- `tr7i <file> ...`: interpret files in sequence

- `tr7i [<file> ...] -c <scheme code> [<arg> ...]`:
  interpret files in sequence and then evaluate
  the scheme code with given arguments

- `tr7i [<init> ...] -1 <file> [<arg> ...]`:
  interpret init in sequence and then
  the file with given arguments

The `-1` flag is meant for #! usage in shell scripts.
If you specify

~~~~~~~~~~~~~~~~~~~~ scheme
#! /somewhere/tr7i -1
;; here my scheme program, example:
(import (scheme process-context) (scheme write))
(write (command-line))
(newline)
~~~~~~~~~~~~~~~~~~~~

then `tr7i` will be called to process the file.
On the example, it echoes its arguments.
See *examples/my-args*.

The following environment variables are understood by `tr7i`:

  variable        |  meaning
 -----------------|------------------------------------------
  TR7_PATH        |  path for 'load' and fallback path
  TR7_LIB_PATH    |  path for importing libraries
  TR7_INC_PATH    |  path for including files
  TR7_EXT_PATH    |  path for 'load-extension'
  TR7_PROMPT      |  string of the prompt (default is "tr7> ")
  TR7INIT         |  path of the initialisation file (default: tr7-init.scm)

When started, `tr7i` automatically loads the initialisation
script if it exists. The default initialisation script is
`tr7-init.scm`. It can be changed using the environment variable
`TR7INIT`.



TR7 libraries
-------------

Some available extra features specific to TR7 are listed below.
Note that because of the youth of TR7, these extra features may change
in later versions.

### library (tr7 expr)

~~~~~~~~~~~~~~~~ Scheme
(expr ...)
~~~~~~~~~~~~~~~~

   Syntax for computing arithmetic expressions.

   Example: `(expr ((2 + 3) * (5 - 4) + 10) % 7)` produces `1`.


### library (tr7 misc)

~~~~~~~~~~~~~~~~ Scheme
(tr7-id)
~~~~~~~~~~~~~~~~

   Returns the string identification of current tr7 version (ex: "tr7-2.0.0")

~~~~~~~~~~~~~~~~ Scheme
(tr7-version)
~~~~~~~~~~~~~~~~

   Returns the string version of current tr7 version (ex: "2.0.0")

~~~~~~~~~~~~~~~~ Scheme
(scheme-paths)
~~~~~~~~~~~~~~~~

   Returns the list of paths, as strings, used for searching libraries


### library (tr7 extra)

~~~~~~~~~~~~~~~~ Scheme
(car+cdr PAIR)
~~~~~~~~~~~~~~~~

   Returns the values of car and cdr of PAIR (see srfi-1)

~~~~~~~~~~~~~~~~ Scheme
(length* LIST)
~~~~~~~~~~~~~~~~

   Return -1 if circular list, a positive or null value for
   proper lists, and for dotted-lists, -1 minus the count of pairs.

~~~~~~~~~~~~~~~~ Scheme
(append-reverse LIST TAIL)
~~~~~~~~~~~~~~~~

   Equivalent to `(append (reverse LIST) TAIL)` (see srfi-1)

~~~~~~~~~~~~~~~~ Scheme
(append-reverse! LIST TAIL)
~~~~~~~~~~~~~~~~

   Equivalent to `(append! (reverse! LIST) TAIL)` (see srfi-1)

~~~~~~~~~~~~~~~~ Scheme
(cons* ELT1 ELT2 ...)
~~~~~~~~~~~~~~~~

   Like `list`, but the last argument provides the tail of the
   constructed list (see srfi-1)

~~~~~~~~~~~~~~~~ Scheme
(list-copy* LIST [START [END [ELEM]]])
~~~~~~~~~~~~~~~~

   START and END are interger indexes in the range 0 to length
   of LIST with the constraint that START is lesser of equal to END:
   0 ≤ START ≤ END ≤ (length LIST).

   `(list-copy* LIST)` is like `(list-copy LIST)`

   `(list-copy* LIST START)` is like `(list-copy (list-tail LIST START))`

   `(list-copy* LIST START END)` returns a new list of with END - START
   elements having the property `(eq? (list-ref RESULT i) (list-ref LIST
   (+ i START))`

   When ELEM is given, END can be greater than length of LIST.
   In that case, the result is like
   `(append (list-copy* LIST START (length LIST))
            (make-list (- END (length LIST)) ELEM))`


~~~~~~~~~~~~~~~~ Scheme
(check-types BOOLEAN)
~~~~~~~~~~~~~~~~

   This is a VERY SPECIAL and UNSAFE syntaxic directive.

   When `(check-types #f)` is encountered, TR7 builtin procedures
   are called without type checking what can lead to disastrous
   unexpected behaviors like SEGMENTATION FAULT or strange
   computation results (ex: `(+ #f #f)` -> `+nan.0`).

   However when types of arguments passed to builtin procedures
   match types expected, the execution speed of programs can be
   improved.

   For conveniency, `check-types` can be used at top level or
   in definition's body. It should be used in pair as here:
   `(check-types #f) OPTIMIZED TYPE (UN)SAFE CODE HERE (check-types #t)`

### library (tr7 environment)

~~~~~~~~~~~~~~~~ Scheme
(environment? ANY)
~~~~~~~~~~~~~~~~

   is ANY an environment?

~~~~~~~~~~~~~~~~ Scheme
(defined? SYMBOL [ENV])
~~~~~~~~~~~~~~~~

   is SYMBOL attached to a value in ENV (current environment by default)

~~~~~~~~~~~~~~~~ Scheme
(symbols-set)
~~~~~~~~~~~~~~~~

   list of known symbols

~~~~~~~~~~~~~~~~ Scheme
(current-environment)
~~~~~~~~~~~~~~~~

   the current environment

~~~~~~~~~~~~~~~~ Scheme
(tr7-environment->list [ENV [DEPTH]])
~~~~~~~~~~~~~~~~

   list the symbols of the environment ENV (current environment by default)
   until the DEPTH (zero or default means as much as possible)


### library (tr7 extension)

~~~~~~~~~~~~~~~~ Scheme
(load-extension STRING [LIBNAME])
~~~~~~~~~~~~~~~~

   loads the shared library of path STRING in the library designed by LIBNAME.
   here the default libname is "tr7/foreigns" that can be imported using
   `(import (tr7 foreign))`


### library (tr7 gc)

~~~~~~~~~~~~~~~~ Scheme
(tr7-gc [VERBOSE])
~~~~~~~~~~~~~~~~

   performs a garbage collection
   makes it verbosely if VERBOSE is given and not #f

~~~~~~~~~~~~~~~~ Scheme
(tr7-gc-verbose [VERBOSE])
~~~~~~~~~~~~~~~~

   if VERBOSE is given, set GC verbosity to its value
   returns the value of GC verbosity before the new setting

~~~~~~~~~~~~~~~~ Scheme
(new-segment [COUNT])
~~~~~~~~~~~~~~~~

   allocates COUNT segments (1 if not COUNT is given) to the memory pool


### library (tr7 trace)

~~~~~~~~~~~~~~~~ Scheme
(tr7-tracing NUM)
~~~~~~~~~~~~~~~~

   activates tracing if num is not 0

~~~~~~~~~~~~~~~~ Scheme
(tr7-show-prompt [BOOL])
~~~~~~~~~~~~~~~~

   without argument, return the current show-prompt status
   with an argument, set the show-prompt status and return the previous one
   when show-prompt is set to true, the is shown before reading the s-expr

~~~~~~~~~~~~~~~~ Scheme
(tr7-show-eval [BOOL])
~~~~~~~~~~~~~~~~

   without argument, return the current show-eval status
   with an argument, set the show-eval status and return the previous one
   when show-eval is set to true, the s-expr evaluated is shown

~~~~~~~~~~~~~~~~ Scheme
(tr7-show-compile [BOOL])
~~~~~~~~~~~~~~~~

   without argument, return the current show-compile status
   with an argument, set the show-compile status and return the previous one
   when show-compile is set to true, the compiled code is shown

~~~~~~~~~~~~~~~~ Scheme
(tr7-show-result [BOOL])
~~~~~~~~~~~~~~~~

   without argument, return the current show-result status
   with an argument, set the show-result status and return the previous one
   when show-result is set to true, the result code is shown

~~~~~~~~~~~~~~~~ Scheme
(tr7-keep-playing [BOOL])
~~~~~~~~~~~~~~~~

   without argument, return the current keep-playing status
   with an argument, set the keep-playing status and return the previous one
   when keep-playing is set to true, programs are not stopped on uncatched errors


### library (tr7 debug)

~~~~~~~~~~~~~~~~ Scheme
(tr7-call-stack)
~~~~~~~~~~~~~~~~

   returns a representation of the current calling stack

~~~~~~~~~~~~~~~~ Scheme
(tr7-exec-stack)
~~~~~~~~~~~~~~~~

   returns a representation of the current execution stack

~~~~~~~~~~~~~~~~ Scheme
(error-object-stack ERROR)
~~~~~~~~~~~~~~~~

   returns a representation of the stack when the error occurred

~~~~~~~~~~~~~~~~ Scheme
(compile expression)
~~~~~~~~~~~~~~~~

   return the program given by compiling expression

~~~~~~~~~~~~~~~~ Scheme
(disass object [port])
~~~~~~~~~~~~~~~~

   print on port (default is current-output-port) the disassembly
   of the object

### library (tr7 tagged-closures)

This library is a low-level library suitable for implementing SRFI-259
or SRFI-229.

~~~~~~~~~~~~~~~~ Scheme
(closure? x)
~~~~~~~~~~~~~~~~

Returns `#t` if `x` is a closure object: i.e. a procedure that has a tag.
Closures are created with `lambda` and `case-lambda`.

~~~~~~~~~~~~~~~~ Scheme
(closure-get-tag x)
~~~~~~~~~~~~~~~~

Returns the closure tag associated with the closure `x`. An exception will
be raised if `x` is not `closure?`. The default tag is the empty list.

~~~~~~~~~~~~~~~~ Scheme
(closure-set-tag! x value)
~~~~~~~~~~~~~~~~

Set the closure tag in `x` to `value`. An exception will be raised if
`x` is not `closure?`. `value` can be any Scheme object.

~~~~~~~~~~~~~~~~ Scheme
(closure-copy x)
~~~~~~~~~~~~~~~~

Copy the closure `x` to a newly allocated closure `y`. An exception will
be raised if `x` is not `closure?`.
