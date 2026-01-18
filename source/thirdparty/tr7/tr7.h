/*
*****************************************************************************
* TR7.H
* =====
*
* This header comes with tr7, a tiny scheme interpreter.
*
* SPDX-License-Identifier: 0BSD
*
* https://gitlab.com/jobol/tr7
*
* Summary of conventions used:
*   - names in upper case only are macro, so use it, but with caution
*     because pre/post increments might have unexpected effect
*   - names with capital (upper case first letters) are enumerations
*   - names starting with 'tr7_' are public functions and types
*   - names ending with '_t' are types from typedef
*/
#ifndef _TR7_H
#define _TR7_H
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
*****************************************************************************
* Default values of TR7_EXPORT used for exporting symbols.
*
* When compiling for static library or for UNIX/BSD systems, there is
* no need to define TR7_EXPORT that is by default defined to `extern`.
*
* If you are compiling for windows, you may need to TR7_EXPORT as either:
*  - __declspec(dllexport) when creating/compiling the DLL
*  - __declspec(dllimport) when compiling the client program
*/
#ifndef TR7_EXPORT
#define TR7_EXPORT extern
#endif
/*
*****************************************************************************
* Defining tr7_t types
* --------------------
*
* The basic public type exposed by TR7 is tr7_t.
*
* TR7 manages many different kind of values: characters, atoms, strings,
* numbers, pairs, ... To achieve it, the most important types
* for TR7 are: tr7_t, tr7_pair_t, tr7_cell_t, tr7_double_t
*
* Within TR7, tr7_pair_t is a pointer to a pair (a pair in the common scheme
* meaning, i.e. a pair of values). A tr7_cell_t is a pointer to one of the
* internal data used by tr7. A tr7_double_t is a pointer to a double.
*/
typedef double            *tr7_double_t;
typedef intptr_t           tr7_int_t;
typedef uintptr_t          tr7_uint_t;
typedef int_fast32_t       tr7_char_t;
typedef struct tr7_pair   *tr7_pair_t;
typedef union  tr7_cell   *tr7_cell_t;
typedef struct tr7_engine *tr7_engine_t;
/*
*****************************************************************************
* Defining `tr7_t`
* ---------------
*
* A `tr7_t` value is a pointer with encoded low bits, a common technique
* to save memory and bits. And saving memory and bits a good thing no?
*
* The bad thing is that in most cases accessing the memory referenced
* directly using the pointer is just not possible!
*
* For that reason, defining `tr7_t` as a pointer is a bad idea. So
* tr7_t is defined as a signed integer of the size of a pointer.
*
* But using integer instead of pointers can be an issue when debugging
* internals of TR7. TODO: facilities for debugging using C debugger.
*/
typedef intptr_t tr7_t;
/*
* Define `TR7WIDTH` the count of bits of tr7_t values
*/
#if INTPTR_MAX == INT64_MAX
#define TR7WIDTH 64
#elif INTPTR_MAX == INT32_MAX
#define TR7WIDTH 32
#else
#error "unexpected size for pointers"
#endif
/*
* Managing to cast pointer and integer is obvious in assembly language
* and easy in C. But to clarify the code, improve its resilience and
* allow to have pointers to cells instead of integers, the below
* macros are used to cast:
*
* - `TR72I(t)`: casts a tr7_t value `t` to a intptr_t value
* - `I2TR7(i)`: casts a intptr_t value `i` to a tr7_t value
* - `TR72U(t)`: casts a tr7_t value `t` to a uintptr_t value
* - `U2TR7(u)`: casts a uintptr_t value `u` to a tr7_t value
* - `TR7EQ(a,b)`: returns a boolean value true if `a` == `b`
*/
#define TR72I(t)          ((intptr_t)(t))
#define I2TR7(i)          ((tr7_t)(intptr_t)(i))
#define TR72U(t)          ((uintptr_t)(t))
#define U2TR7(u)          ((tr7_t)(uintptr_t)(u))
#define TR7EQ(a,b)        ((a) == (b))
/*
* ### Comparison with `tr7_eq`
*/
TR7_EXPORT int tr7_eq(tr7_t a, tr7_t b);
/*
* The function `tr7_eq` is equivalent to the *Scheme* predicate `eq?`.
* It returns a non zero value when `a` and `b` are equals for `eq?`.
*
* It can compare small integers, characters, booleans and empty list.
*
* The function `tr7_eq` uses internal the C macro `TR7EQ`
* but it can be used as a callback or it can ensure arguments
* are not expanded more than one time.
*
* ### Comparison with `tr7_eqv`
*/
TR7_EXPORT int tr7_eqv(tr7_t a, tr7_t b);
/*
* The function `tr7_eqv` is equivalent to the *Scheme* predicate `eqv?`.
* It returns a non zero value when `a` and `b` are equals for `eqv?`.
*
* ### Comparison with `tr7_equal`
*/
TR7_EXPORT int tr7_equal(tr7_t a, tr7_t b);
/*
* The function `tr7_equal` is equivalent to the *Scheme* predicate `equal?`.
* It returns a non zero value when `a` and `b` are equals for `equal?`.
*
* ### Testing immutability, `tr7_is_immutable`
*/
TR7_EXPORT int tr7_is_immutable(tr7_t t);
/*
* The function `tr7_is_immutable` returns 1 if the value `t` is
* immutable and return 0 if `t` is mutable.
*
* !!! CAUTION
*     only strings, vectors, bytevectors and records can be immutable
*     in current and previous versions of TR7.
*
* ### Setting immutability, `tr7_set_immutable`
*/
TR7_EXPORT void tr7_set_immutable(tr7_t t);
/*
* Make the value `t` immutable.
*
* !!! CAUTION
*     only strings, vectors, bytevectors and records can be immutable
*     in current and previous versions of TR7.
*
*****************************************************************************
* Defining `TR7_VOID`
* -------------------
*
* The special value `TR7_VOID` is intended to represent
* an invalid value of TR7. On purpose, its value is equal or
* equivalent to the C value `NULL`.
*
* The value `TR7_VOID` is used internally for indicating
* a non-value value (or an undefined value).
*/
#define TR7_VOID          I2TR7(0)
/*
* This macro test equality to `TR7_VOID`:
*
* - `TR7_IS_VOID(t)`: returns true if `t` is the predefined `TR7_VOID`
*/
#define TR7_IS_VOID(t)    TR7EQ(t, TR7_VOID)
/*
*****************************************************************************
* Defining TR7's pointer tagging
* ------------------------------
*
* As written above, `tr7_t` use the lower bits of a pointer compatible integer
* to encode the kind of data it represent. A technique known as pointer
* tagging.
*
* The count of bits used for tags is fixed to 3 as shown below:
*
* ******************************************************************
* *              <---- 29 or 61 upper bits ---->  2   1   0
* *             +---+---+ - - - - - +---+---+---+---+---+---+
* *   tr7_t     |   |   |           |   |   |   | x   x   x |
* *             +---+---+ - - - - - +---+---+---+---+---+---+
* ******************************************************************
*
* Using 3 bits is not worrying on 64 bits processors because the natural
* alignment of pointers is 8 bytes, implying that the 3 least significands
* bits are 0.
*
* However, on 32 bits processors, the natural alignment of 4 bytes let
* only 2 least significant bits. But tr7 takes care of that issue and
* ensures that underlying pointers are aligned on 8 bytes boundaries.
* Note that the alignment on 8 byte is natural for doubles but not guaranteed.
* But take care of it if you intend to forge your own tr7_t values.
*
* This is in fact described by the values below:
*
* - `TR7_WIDTH_TAG`: width of the tag in count of bits: 3
* - `TR7_ALIGNMENT`: the required alignment
* - `TR7_MASK_TAG`:  mask returning the tag
*/
#define TR7_WIDTH_TAG      3
#define TR7_ALIGNMENT      (1 << (TR7_WIDTH_TAG))
#define TR7_MASK_TAG       (TR7_ALIGNMENT - 1)
/*
* !!! Tip
*    The above definitions are respecting a rule in naming
*    for TR7: the width or the mask prefixes the name attributed. Then
*    the name can be used as prefix for its values, as below
*
* For `TR7_WIDTH_TAG` = 3, a tr7 value is made of:
*   - a tag on its 3 lower bits
*   - a value on its 29 or 61 upper bits
*
* To help manipulating TR7 values the following macro are defined.
*
* - `TR7_TAG(t)`: returns a `intptr_t` equals to the tag of tr7 value `t`
* - `TR7_IS_TAG(t,k)`: return true if tag of tr7 value `t` is `k`
* - `TR7_MAKE_EQ(v,k)`: make a tr7 of tag `k` and value `v` not shifted.
*                       it is asserted that `0 == TR7_TAG(v)`
*
* !!! WARNING
*     for TR7_MAKE_EQ the 3 lower bits of v MUST be zero
*/
#define TR7_TAG(t)        (TR72I(t) & TR7_MASK_TAG)
#define TR7_IS_TAG(t,k)   (TR7_TAG(t) == (k))
#define TR7_MAKE_EQ(v,k)  I2TR7((intptr_t)(v) | (intptr_t)(k))
/*
* When interpreting the meaning of a `tr7_t` value, it is may worth
* to distinguish between values that must be shifted and values that
* must not be shifted. Pointers must not be shifted, while integrals
* should generally be shifted.
*
* For this reason and maybe some other, the tags are partitioned
* in two sets, using the lowest bit: the set of tagged pointers
* and the set of tagged integrals.
*
* ******************************************************************
* *                   <--- 29 or 61 upper bits ---->   2   1   0
* *                  +---+---+ - - - - - +---+---+---+---+---+---+
* * tagged pointers  |   |   |           |   |   |   | x   x   0 |
* *                  +---+---+ - - - - - +---+---+---+---+---+---+
* * tagged integrals |   |   |           |   |   |   | x   x   1 |
* *                  +---+---+ - - - - - +---+---+---+---+---+---+
* ******************************************************************
*
* The lower bit tells if the value is a pointer or not.
*
* If lower bit is 0 this is a pointer
* If lower bit is 1 the value is not a pointer
*
* This is in fact described by the values below:
*
* - `TR7_WIDTH_PTR`: width of the pointer's tag in count of bits: 1
* - `TR7_MASK_PTR`:  mask returning the pointer's tag
* - `TR7_TAG_PTR`:   value of the pointer's tag identifying pointers
*/
#define TR7_WIDTH_PTR        1
#define TR7_MASK_PTR         001
#define TR7_TAG_PTR          000
/*
* Macros for manipulating pointers are:
*
* - `TR7_IS_PTR(t)`: returns a boolean true if the tr7 value `t` is a pointer
* - `TR7_IS_PTR_TAG(t,k)`: returns true if the tr7 value `t` is of
*                          tag `k` and not `NULL`
* - `TR7_TO_PTR(t,k)`: returns the void* given by the tr7 value `t` of tag `k`
* - `TR7_AS_PTR(t,k)`: returns the void* given by the tr7 value `t` if of
*                      the given tag `k`, or otherwise returns NULL
* - `TR7_FROM_PTR(p,k)`: returns the tr7_t value for the pointer `p` of tag `k`
*
* !!! WARNING
*    for `TR7_TO_PTR` the 3 lower bits of `p` MUST of tag `k` otherwise the
*    result is undefined. This is because removing the tag is done using
*    subtraction that does not require neither "and not" instruction nor
*    correct size of values.
*
* !!! WARNING
*    for `TR7_FROM_PTR` the 3 lower bits of `p` MUST be zero
*/
#define TR7_IS_PTR(t)        ((TR72I(t) & TR7_MASK_PTR) == TR7_TAG_PTR)
#define TR7_IS_PTR_TAG(t,k)  (TR7_IS_TAG(t,k) && ((k) || (t)))
#define TR7_TO_PTR(t,k)      ((void*)(TR72I(t)-(k)))
#define TR7_AS_PTR(t,k)      (TR7_TAG(t) == (k) ? TR7_TO_PTR(t,k) : NULL)
#define TR7_FROM_PTR(p,k)    TR7_MAKE_EQ(p,k)
/*
* The basic dichotomy between tagged pointers and tagged integrals
* is then refined as below:
*
* **Tagged pointers**
* ******************************************************************
* *                  <--- 29 or 61 upper bits ---->   2   1   0
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* * tr7_pair_t      |   |   |           |   |   |   | 0   0   0 |
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* * tr7_cell_t      |   |   |           |   |   |   | 0   1   0 |
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* * tr7_double_t    |   |   |           |   |   |   | 1   0   0 |
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* * SPARE POINTER   |   |   |           |   |   |   | 1   1   0 |
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* ******************************************************************
*
* **Tagged integrals**
* *******************************************************************
* *                  <--- 29 or 61 upper  bits ---->  2   1   0
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* * tr7_int_t even  |   |   |           |   |   |   | 0   0   1 |
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* * SPECIALS        |   |   |           |   |   |   | 0   1   1 |
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* * tr7_int_t odd   |   |   |           |   |   |   | 1   0   1 |
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* * SPARE INTEGRAL  |   |   |           |   |   |   | 1   1   1 |
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* *******************************************************************
*
* This leads to the attribution of tags below:
*/
#define TR7_TAG_PAIR      000 /* pointer to a pair or NIL */
#define TR7_TAG_EINT      001 /* even integers */
#define TR7_TAG_CELL      002 /* pointer to a cell */
#define TR7_TAG_SPECIAL   003 /* special cases */
#define TR7_TAG_DOUBLE    004 /* pointer to a double */
#define TR7_TAG_OINT      005 /* odd integers (must be 4 + `TR7_TAG_EINT`) */
#define TR7_TAG_SPARE1    006 /* spare pointer */
#define TR7_TAG_SPARE2    007 /* spare value */
/*
*****************************************************************************
* Defining basic integers
* -----------------------
*
* Integers use 2 tags, this let one more bit for encoding integer values,
* leading integer values of 30 or 62 bits depending on the target.
* For this reason, the equality 4 == `TR7_TAG_OINT` - `TR7_TAG_EINT`
* must be kept.
*
* For performance, it is needed to define a sub class of tr7_t that merge
* the two integer tags.
*
* The idea is to implement the following schema:
*
* *******************************************************************
* *                  <----- 30 or 62 upper  bits ------>  1   0
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* * tr7_int_t       |   |   |           |   |   |   |   | 0 | 1 |
* *                 +---+---+ - - - - - +---+---+---+---+---+---+
* *******************************************************************
*
* This is in fact described by the values below:
*
* - `TR7_WIDTH_INT`: width of the integer's tag in count of bits: 2
* - `TR7_MASK_INT`:  mask returning the integer's tag
* - `TR7_TAG_INT`:   value of the integer's tag identifying integers
* - `TR7_INT_WIDTH`: significant count of bits for the integer's values
*/
#define TR7_WIDTH_INT      2
#define TR7_MASK_INT       003
#define TR7_TAG_INT        001
#define TR7_INT_WIDTH      (TR7WIDTH - TR7_WIDTH_INT)
/*
* ### Macros for manipulating integers are:
*
* - `TR7_IS_INT(t)`: returns a boolean true if the tr7 value `t`
*                    is `tr7_(u)int_t`
* - `TR7_TO_INT(t)`: returns the `tr7_int_t` value of the tr7 value `t`
* - `TR7_FROM_INT(i)`: returns the `tr7_t` value for the `tr7_int_t` `i`
* - `TR7_TO_UINT(t)`: returns the `tr7_uint_t` value of the tr7 value `t`
* - `TR7_FROM_UINT(u)`: returns the `tr7_t` value for the `tr7_uint_t` `u`
*
* NOTE: the system doesn't provide a way to distinguish between
* signed and unsigned version of the wrapped integer. So it should
* be given by the context. Without context, signed integer is the
* default.
*/
#define TR7_IS_INT(t)     ((TR72I(t) & TR7_MASK_INT) == TR7_TAG_INT)
#define TR7_TO_UINT(t)    (((tr7_uint_t)TR72I(t)) >> TR7_WIDTH_INT)
#define TR7_FROM_UINT(u)  TR7_MAKE_EQ(((tr7_uint_t)(u)) << TR7_WIDTH_INT, \
                                      TR7_TAG_INT)
#define TR7_TO_INT(t)     (((tr7_int_t)TR72I(t)) >> TR7_WIDTH_INT)
#define TR7_FROM_INT(i)   TR7_FROM_UINT((tr7_int_t)(i))
/*
* ### Macros returning integer's limits
*
* The limits of the integer in that system are:
*
* - `TR7_INT_MAX`: maximum signed integer value for full type tr7_int_t
* - `TR7_INT_MIN`: minimum signed integer value for full type tr7_int_t
* - `TR7_UINT_MAX`: maximum unsigned integer value for full type tr7_uint_t
* - `TR7_INT_MAX_VAL`: maximum signed integer value
* - `TR7_INT_MIN_VAL`: minimum signed integer value
* - `TR7_UINT_MAX_VAL`: maximum unsigned integer value
*/
#define TR7_INT_MAX        INTPTR_MAX
#define TR7_INT_MIN        INTPTR_MIN
#define TR7_UINT_MAX       UINTPTR_MAX
#define TR7_INT_MAX_VAL    (TR7_INT_MAX >> TR7_WIDTH_INT)
#define TR7_INT_MIN_VAL    (TR7_INT_MIN >> TR7_WIDTH_INT)
#define TR7_UINT_MAX_VAL   (TR7_UINT_MAX >> TR7_WIDTH_INT)
/*
* Macro for checking if an integer value fits the integer encoding
*
* - `TR7_FIT_INT(x)`:  return true if TR7_INT_MIN <= x <= TR7_INT_MAX
* - `TR7_FIT_UINT(x)`: return true if 0 <= x <= TR7_UINT_MAX
*/
#if TR7_WIDTH_INT == 2
#define TR7_FIT_INT(x)     (((x) ^ ((x) << 1)) >= 0)
#else
#define TR7_FIT_INT(x)     (0 == (((x) ^ ((tr7_int_t)(x) >> 1)) >> TR7_INT_WIDTH)
#endif
#define TR7_FIT_UINT(x)    (0 == ((x) >> TR7_INT_WIDTH))
/*
* ### functions for manipulating integers
*/
TR7_EXPORT int tr7_is_integer(tr7_t item);
TR7_EXPORT tr7_t tr7_from_int(tr7_engine_t tsc, tr7_int_t value);
TR7_EXPORT tr7_int_t tr7_to_int(tr7_t item);
/*
* The function `tr7_is_integer` test if `item` is a TR7 integer and returns
* 1 if it is the case and 0 otherwise.
*
* The function `tr7_from_int` returns the tr7_t instance for the integer
* of 'value'.
*
* The function `tr7_to_int` returns the tr7_int_t value of the 'item'.
* It assumes that 'item' is a TR7 integer.
*
*****************************************************************************
* Defining doubles
* ----------------
*
* Doubles values are presented by pointers to a double. Such pointer
* are normally aligned on 8 bytes boundaries, leading to pointer
* having their 3 lower bits zeroed. But take care the pointer MUST have
* its 3 lower bits zeroed!
*
* ### Macros for manipulating reals, `tr7_t` value wraps a pointer to a double
*
* - `TR7_IS_DOUBLE(t)`: returns a boolean true if the tr7 value `t` wraps
*                       a `tr7_double_t`
* - `TR7_TO_DOUBLE(t)`: returns the `tr7_double_t` of the tr7 value `t`
* - `TR7_AS_DOUBLE(t)`: returns the `tr7_double_t` of the tr7 value `t`
*                       if of the kind real, or otherwise,
*                       if t isn't a real, returns `NULL`
* - `TR7_FROM_DOUBLE(p)`: returns the `tr7_t` value for the `tr7_double_t` `p`
*/
#define TR7_IS_DOUBLE(t)    TR7_IS_PTR_TAG(t,TR7_TAG_DOUBLE)
#define TR7_TO_DOUBLE(t)    ((tr7_double_t)TR7_TO_PTR(t,TR7_TAG_DOUBLE))
#define TR7_AS_DOUBLE(t)    ((tr7_double_t)TR7_AS_PTR(t,TR7_TAG_DOUBLE))
#define TR7_FROM_DOUBLE(p)  TR7_FROM_PTR(p,TR7_TAG_DOUBLE)
/*
* ### functions for manipulating doubles
*/
TR7_EXPORT tr7_t tr7_from_double(tr7_engine_t tsc, double value);
TR7_EXPORT double tr7_to_double(tr7_t item);
/*
* The function `tr7_from_double` returns the tr7_t instance for the double
* of 'value'.
*
* The function `tr7_to_double` returns the double value of the 'item'.
* It assumes that 'item' is a TR7 number.
*
*****************************************************************************
* Defining specials
* -----------------
*
* The kind "special" (TR7_TAG_SPECIAL) is subdivided in 4 sub kinds
* called "very special cases" and noted kind of VSP.
*
* VSP are used to encode NIL, booleans, eof, procedure, syntaxes and characters
*
* The 4 VSP are defined by taking the 2 least significant bits of the
* value of specials.
*
* The idea is to implement the following schema:
*
* *******************************************************************
* *                  <- 27 or 59 bits ->  4   3   2   1   0
* *                 +---+---+ - - - - - +---+---+---+---+---+
* *                 |   |   |           | x   x |  SPECIAL  |
* *                 +---+---+ - - - - - +---+---+---+---+---+
* * constants       |   |   |           | 0   0 | 0   1   1 |
* *                 +---+---+ - - - - - +---+---+---+---+---+
* * characters      |   |   |           | 0   1 | 0   1   1 |
* *                 +---+---+ - - - - - +---+---+---+---+---+
* * internals       |   |   |           | 1   0 | 0   1   1 |
* *                 +---+---+ - - - - - +---+---+---+---+---+
* * SPARE VSP       |   |   |           | 1   1 | 0   1   1 |
* *                 +---+---+ - - - - - +---+---+---+---+---+
* *******************************************************************
*
* To improve computing, the 5 lower bits of `tr7_t` items are treated
* altogether.
*/
#define TR7_WIDTH_VSP     (2 + TR7_WIDTH_TAG)
#define TR7_MASK_VSP      (030 | TR7_MASK_TAG )
/*
* Macros for manipulating very specials are:
*
* - `TR7_TAG_VSP(k)`: returns the VSP tag of the for the VSP kind `k`
* - `TR7_VSP_TAG(t)`: returns the VSP tag of a `tr7_t` `t`
* - `TR7_IS_VSP(k,t)`: returns true if the `tr7_t` `t` is a VSP of kind `k`
* - `TR7_VSP_VALUE(t)`: returns the signed value of a special `tr7_t` `t`
* - `TR7_VSP_UVALUE(t)`: returns the unsigned value of a special `tr7_t` `t`
* - `TR7_MAKE_VSP(k,v)`: returns a special `tr7_t` of kind `k` and value `v`
*/
#define TR7_TAG_VSP(k)      (((k) << TR7_WIDTH_TAG) | TR7_TAG_SPECIAL)
#define TR7_VSP_TAG(t)      (TR72I(t) & TR7_MASK_VSP)
#define TR7_IS_VSP(k,t)     (TR7_VSP_TAG(t) == TR7_TAG_VSP(k))
#define TR7_VSP_VALUE(t)    (TR72I(t) >> TR7_WIDTH_VSP)
#define TR7_VSP_UVALUE(t)   (TR72U(t) >> TR7_WIDTH_VSP)
#define TR7_MAKE_VSP(k,v)   I2TR7(((intptr_t)(v) << TR7_WIDTH_VSP) \
                                  | (intptr_t)TR7_TAG_VSP(k))
#define TR7_VSP_VALUE_MAX   (INTPTR_MAX >> TR7_WIDTH_VSP)
#define TR7_VSP_UVALUE_MAX  (UINTPTR_MAX >> TR7_WIDTH_VSP)
/*
* Attribution of very special numbers
*/
#define TR7_VSP_CONSTANT     0    /* constants */
#define TR7_VSP_CHARACTER    1    /* character */
#define TR7_VSP_INTERNAL     2    /* internal  */
#define TR7_VSP_SPARE        3    /* spare */
/*
* Check the kind of the special
*
* - `TR7_IS_CONSTANT(t)`: returns true if `t` is a VSP constant
* - `TR7_IS_CHARACTER(t)`: returns true if `t` is a character
* - `TR7_IS_INTERNAL(t)`: returns true if `t` is a VSP internal
* - `TR7_IS_SPARE(t)`:  returns true if `t` is a VSP SPARE
*/
#define TR7_IS_CONSTANT(t)   TR7_IS_VSP(TR7_VSP_CONSTANT,(t))
#define TR7_IS_CHARACTER(t)  TR7_IS_VSP(TR7_VSP_CHARACTER,(t))
#define TR7_IS_INTERNAL(t)   TR7_IS_VSP(TR7_VSP_INTERNAL,(t))
#define TR7_IS_SPARE(t)      TR7_IS_VSP(TR7_VSP_SPARE,(t))
/*
*****************************************************************************
* The VSP kind VSP_CONSTANT
* -------------------------
*
* It is used to encode some well known constants:
*
*  - `TR7_NIL`:    NIL value is used for `'()` the empty list
*  - `TR7_FALSE`:  the boolean value #false
*  - `TR7_TRUE`:   the boolean value #true
*  - `TR7_EOF`:    the EOF (end-of-file) mark
*/
#define TR7_NIL           TR7_MAKE_VSP(TR7_VSP_CONSTANT,0)
#define TR7_FALSE         TR7_MAKE_VSP(TR7_VSP_CONSTANT,1)
#define TR7_TRUE          TR7_MAKE_VSP(TR7_VSP_CONSTANT,2)
#define TR7_EOF           TR7_MAKE_VSP(TR7_VSP_CONSTANT,3)
/*
* Macros for manipulating VSP kind VSP_INTERNAL are:
*
* - `TR7_IS_NIL(t)`: returns true if `t` is the predefined `TR7_NIL`
* - `TR7_IS_FALSE(t)`: returns true if `t` is the predefined `TR7_FALSE`
* - `TR7_IS_TRUE(t)`: returns true if `t` is the predefined `TR7_TRUE`
* - `TR7_IS_BOOLEAN(t)`: returns true if `t` is `TR7_TRUE` or `TR7_FALSE`
* - `TR7_IS_EOF(t)`: returns true if `t` is the predefined `TR7_EOF`
*/
#define TR7_IS_NIL(t)     TR7EQ(t, TR7_NIL)
#define TR7_IS_FALSE(t)   TR7EQ(t, TR7_FALSE)
#define TR7_IS_TRUE(t)    TR7EQ(t, TR7_TRUE)
#define TR7_IS_BOOLEAN(t) (TR7_IS_FALSE(t) || TR7_IS_TRUE(t))
#define TR7_IS_EOF(t)     TR7EQ(t, TR7_EOF)
/*
*****************************************************************************
* The VSP kind VSP_CHARACTER
* --------------------------
*
* It is used to encode characters. The wrapped value is unsigned.
*
* Macros for manipulating VSP of this kind are:
*
* - `TR7_IS_CHAR(t)`: returns true is tr7_t `t` wraps a characters
* - `TR7_TO_CHAR(t)`: returns the tr7_char_t character wrapped by tr7_t `t`
* - `TR7_FROM_CHAR(c)`: returns a tr7_t wrapping the tr7_char_t `c`
* - `TR7_CHAR_MAX`: maximum character code point
*/
#define TR7_IS_CHAR(t)    TR7_IS_CHARACTER(t)
#define TR7_TO_CHAR(t)    ((tr7_char_t)TR7_VSP_UVALUE(t))
#define TR7_FROM_CHAR(c)  TR7_MAKE_VSP(TR7_VSP_CHARACTER,c)
#define TR7_CHAR_MAX      (TR7_VSP_UVALUE_MAX > UINT_MAX ? UINT_MAX \
                                                         : TR7_VSP_UVALUE_MAX)
/*
* ### Some predefined characters
*/
#define TR7_CHAR_REPLACEMENT         ((tr7_char_t)0xFFFD)
#define TR7_IS_CHAR_REPLACEMENT(c)   (((tr7_char_t)(c)) == TR7_CHAR_REPLACEMENT)
#define TR7_REPLACEMENT_CHAR         TR7_FROM_CHAR(TR7_CHAR_REPLACEMENT)
#define TR7_IS_REPLACEMENT_CHAR(t)   TR7EQ(t, TR7_REPLACEMENT_CHAR)
/*
* UNICODE defines the 'REPLACEMENT CHARACTER' (U+FFFD) that is used
* to replace a character whose value is unrepresentable in unicode.
* It is given here as a tr7_char_t constant: TR7_CHAR_REPLACEMENT
* and as a tr7_t value: TR7_REPLACEMENT_CHAR.
*/
#define TR7_CHAR_BOM                 ((tr7_char_t)0xFEFF)
#define TR7_IS_CHAR_BOM(c)           (((tr7_char_t)(c)) == TR7_CHAR_BOM)
#define TR7_BOM_CHAR                 TR7_FROM_CHAR(TR7_CHAR_BOM)
#define TR7_IS_BOM_CHAR(t)           TR7EQ(t, TR7_BOM_CHAR)
/*
* BOM stands for Byte Order Mark. It is defined here for convenience
* but is not used internaly.
*/
#define TR7_CHAR_EOF                 ((tr7_char_t)EOF)
#define TR7_IS_CHAR_EOF(c)           (((tr7_char_t)(c)) == TR7_CHAR_EOF)
#define TR7_EOF_CHAR                 TR7_FROM_CHAR(TR7_CHAR_EOF)
#define TR7_IS_EOF_CHAR(t)           TR7EQ(t, TR7_EOF_CHAR)
/*
* The representation of EOF as a character is used internally in TR7.
* It should normally never be returned by any function.
* Note that it is a negative value.
*
* ### functions for manipulating characters
*/
TR7_EXPORT int tr7_is_char_unicode(tr7_char_t value);
TR7_EXPORT tr7_t tr7_from_character(tr7_engine_t tsc, tr7_char_t value);
TR7_EXPORT tr7_char_t tr7_to_character(tr7_t item);
TR7_EXPORT int tr7_is_character(tr7_t item);
TR7_EXPORT int tr7_is_unicode_character(tr7_t item);
/*
* The function `tr7_is_char_unicode` checks if the given 'value' is a valid
* UNICODE code point.
*
* The function `tr7_from_character` returns the tr7_t instance for the character
* of 'value'.
*
* The function `tr7_to_character` returns the character value of the 'item'.
* It assumes that 'item' is a TR7 character.
*
* The function `tr7_is_character` test if `item` is a TR7 character and returns
* 1 if it is the case and 0 otherwise.
*
* The function `tr7_is_unicode_character` test if `item` is a TR7 character
* and is a valid unicode codepoint, 1 if it is the case and 0 otherwise.
*
*****************************************************************************
* Defining tr7_pair_t
* -------------------
*
* A pair is a simple structure having 2 fields: car and cdr.
*/
struct tr7_pair
{
   tr7_t cdr;  /* the CDR */
   tr7_t car;  /* the CAR */
};
/*
* ### Macros for manipulating pairs from tr7_pair_t values
*
* - `TR7_PAIR_CAR(p)`: returns the tr7_t car of the tr7_pair_t `p`
* - `TR7_PAIR_CDR(p)`: returns the tr7_t cdr of the tr7_pair_t `p`
*/
#define TR7_PAIR_CAR(p)  (p)->car
#define TR7_PAIR_CDR(p)  (p)->cdr
/*
* ### Macros for manipulating pairs from tr7_t values.
*
* - `TR7_IS_PAIR(t)`: returns a boolean true if the tr7 value `t` points a pair
* - `TR7_TO_PAIR(t)`: returns the tr_pair_t (pointer to pair) of the tr7
*                     value `t`
* - `TR7_AS_PAIR(t)`: returns the tr7_pair_t of the tr7 value `t` if of the
*                     kind pair, or otherwise, if `t` isn't a pair,
*                     returns `NULL`
* - `TR7_FROM_PAIR(p)`: returns the tr7_t value for the pointer `p`
*/
#define TR7_IS_PAIR(t)    TR7_IS_PTR_TAG(t,TR7_TAG_PAIR)
#define TR7_TO_PAIR(t)    ((tr7_pair_t)TR7_TO_PTR(t,TR7_TAG_PAIR))
#define TR7_AS_PAIR(t)    ((tr7_pair_t)TR7_AS_PTR(t,TR7_TAG_PAIR))
#define TR7_FROM_PAIR(p)  TR7_FROM_PTR(p,TR7_TAG_PAIR)
/*
* ### Macros `TR7_CXR`
*
* Level 1 helper macros for accessing pairs.
*
* !!! WARNING
*    !Unsafe! These macros are not checking anything
*    Use `tr7_cXr_or_void` functions when safety is required.
*
* - `TR7_CAR(t)`: returns the tr7_t car of the tr7_t `t`
* - `TR7_CDR(t)`: returns the tr7_t cdr of the tr7_t `t`
*/
#define TR7_CAR(t)       TR7_PAIR_CAR(TR7_TO_PAIR(t))
#define TR7_CDR(t)       TR7_PAIR_CDR(TR7_TO_PAIR(t))
/*
* Level 2 helpers
*
* !!! WARNING
*    !Unsafe! These macros are not checking anything
*    Use `tr7_cXr_or_void` functions when safety is required.
*/
#define TR7_CAAR(t)      TR7_CAR(TR7_CAR(t))
#define TR7_CADR(t)      TR7_CAR(TR7_CDR(t))
#define TR7_CDAR(t)      TR7_CDR(TR7_CAR(t))
#define TR7_CDDR(t)      TR7_CDR(TR7_CDR(t))
/*
* Level 3 helpers
*
* !!! WARNING
*    !Unsafe! These macros are not checking anything
*    Use `tr7_cXr_or_void` functions when safety is required.
*/
#define TR7_CAAAR(t)     TR7_CAR(TR7_CAAR(t))
#define TR7_CAADR(t)     TR7_CAR(TR7_CADR(t))
#define TR7_CADAR(t)     TR7_CAR(TR7_CDAR(t))
#define TR7_CADDR(t)     TR7_CAR(TR7_CDDR(t))
#define TR7_CDAAR(t)     TR7_CDR(TR7_CAAR(t))
#define TR7_CDADR(t)     TR7_CDR(TR7_CADR(t))
#define TR7_CDDAR(t)     TR7_CDR(TR7_CDAR(t))
#define TR7_CDDDR(t)     TR7_CDR(TR7_CDDR(t))
/*
* Level 4 helpers
*
* !!! WARNING
*    !Unsafe! These macros are not checking anything
*    Use `tr7_cXr_or_void` functions when safety is required.
*/
#define TR7_CAAAAR(t)    TR7_CAR(TR7_CAAAR(t))
#define TR7_CAAADR(t)    TR7_CAR(TR7_CAADR(t))
#define TR7_CAADAR(t)    TR7_CAR(TR7_CADAR(t))
#define TR7_CAADDR(t)    TR7_CAR(TR7_CADDR(t))
#define TR7_CADAAR(t)    TR7_CAR(TR7_CDAAR(t))
#define TR7_CADADR(t)    TR7_CAR(TR7_CDADR(t))
#define TR7_CADDAR(t)    TR7_CAR(TR7_CDDAR(t))
#define TR7_CADDDR(t)    TR7_CAR(TR7_CDDDR(t))
#define TR7_CDAAAR(t)    TR7_CDR(TR7_CAAAR(t))
#define TR7_CDAADR(t)    TR7_CDR(TR7_CAADR(t))
#define TR7_CDADAR(t)    TR7_CDR(TR7_CADAR(t))
#define TR7_CDADDR(t)    TR7_CDR(TR7_CADDR(t))
#define TR7_CDDAAR(t)    TR7_CDR(TR7_CDAAR(t))
#define TR7_CDDADR(t)    TR7_CDR(TR7_CDADR(t))
#define TR7_CDDDAR(t)    TR7_CDR(TR7_CDDAR(t))
#define TR7_CDDDDR(t)    TR7_CDR(TR7_CDDDR(t))
/*
* ### Functions `tr7_cXr_or_void`
*
* All these functions, as the end of their names `_or_void` indicates are
* returning `TR7_VOID` if requested value can not be returned from the
* given value. These functions must be called if a check as to be done on `t`.
*/
TR7_EXPORT tr7_t tr7_car_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdr_or_void(tr7_t t);

TR7_EXPORT tr7_t tr7_caar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cadr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cddr_or_void(tr7_t t);

TR7_EXPORT tr7_t tr7_caaar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_caadr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cadar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_caddr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdaar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdadr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cddar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdddr_or_void(tr7_t t);

TR7_EXPORT tr7_t tr7_caaaar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_caaadr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_caadar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_caaddr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cadaar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cadadr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_caddar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cadddr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdaaar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdaadr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdadar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdaddr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cddaar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cddadr_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cdddar_or_void(tr7_t t);
TR7_EXPORT tr7_t tr7_cddddr_or_void(tr7_t t);
/*
* ### Function `tr7_cons`
*/
TR7_EXPORT tr7_t tr7_cons(tr7_engine_t tsc, tr7_t a, tr7_t b);
/*
* This function returns the `tr7_t` value representing the
* pair `( a . b )` or returns TR7_NIL if it fails.
*
* !!! CAUTION
*    Can trigger garbage collection
*
* ### Function `tr7_cons_n`
*/
TR7_EXPORT tr7_t tr7_cons_n(tr7_engine_t tsc, int count, tr7_t cars[], tr7_t cdr);
/*
* This function returns the `tr7_t` value representing the
* list `( car[0] car[1] ... car[count-1] . cdr )` or returns `TR7_NIL` if it
* fails. When count is zero, `cdr` is returned.
*
* The function `tr7_cons_n` tries to be efficient when it reclaims pairs
* from memory. So it is recommended to use it instead of calling repeatedly
* `tr7_cons`. The helpers macros `TR7_CONSn` and `TR7_LISTn` should be used
* to make calls simpler.
*
* !!! CAUTION
*    Can trigger garbage collection
*
* ### Macro `TR7_LIST_N`
*/
#define TR7_LIST_N(tsc,count,cars)  tr7_cons_n((tsc),(count),(cars),TR7_NIL)
/*
* This macro returns the list `( car[0] car[1] ... car[count-1] )`
* or `TR7_NIL` on failure.
*
* !!! CAUTION
*    Can trigger garbage collection
*
* ### Macros `TR7_CONSn`
*
* Macros `TR7_CONSn` are wrappers to call the `tr7_cons` or `tr7_cons_n`.
*
* !!! CAUTION
*    Any of these macro can trigger garbage collection
*
* Example: the macro `TR7_CONS4(tsc,a,b,c,d)` returns the list `( a b c . d )`
*/
#define TR7_CONS2(tsc,a,b)           tr7_cons(tsc,a,b)
#define TR7_CONS3(tsc,a,b,c)         tr7_cons_n(tsc,2,(tr7_t[2]){a,b},c)
#define TR7_CONS4(tsc,a,b,c,d)       tr7_cons_n(tsc,3,(tr7_t[3]){a,b,c},d)
#define TR7_CONS5(tsc,a,b,c,d,e)     tr7_cons_n(tsc,4,(tr7_t[4]){a,b,c,d},e)
#define TR7_CONS6(tsc,a,b,c,d,e,f)   tr7_cons_n(tsc,5,(tr7_t[5]){a,b,c,d,e},f)
#define TR7_CONS7(tsc,a,b,c,d,e,f,g) tr7_cons_n(tsc,6,(tr7_t[6]){a,b,c,d,e,f},g)
#define TR7_CONS8(tsc,a,b,c,d,e,f,g,h) \
                                   tr7_cons_n(tsc,7,(tr7_t[7]){a,b,c,d,e,f,g},h)
#define TR7_CONS9(tsc,a,b,c,d,e,f,g,h,i) \
                                 tr7_cons_n(tsc,8,(tr7_t[8]){a,b,c,d,e,f,g,h},i)
#define TR7_CONS10(tsc,a,b,c,d,e,f,g,h,i,j) \
                               tr7_cons_n(tsc,9,(tr7_t[9]){a,b,c,d,e,f,g,h,i},j)
#define TR7_CONS11(tsc,a,b,c,d,e,f,g,h,i,j,k) \
                           tr7_cons_n(tsc,10,(tr7_t[10]){a,b,c,d,e,f,g,h,i,j},k)
#define TR7_CONS12(tsc,a,b,c,d,e,f,g,h,i,j,k,l)   \
                         tr7_cons_n(tsc,11,(tr7_t[11]){a,b,c,d,e,f,g,h,i,j,k},l)
#define TR7_CONS13(tsc,a,b,c,d,e,f,g,h,i,j,k,l,m) \
                       tr7_cons_n(tsc,12,(tr7_t[12]){a,b,c,d,e,f,g,h,i,j,k,l},m)
/*
* ### Macros `TR7_LISTn`
*
* Macros `TR7_LISTn` are wrappers to call the `tr7_cons` or `tr7_cons_n`.
*
* !!! CAUTION
*    Any of these macro can trigger garbage collection
*
* Example: the macro `TR7_LIST4(tsc,a,b,c,d)` returns the list `( a b c d )`
*/
#define TR7_LIST1(tsc,a)               tr7_cons(tsc,a,TR7_NIL)
#define TR7_LIST2(tsc,a,b)             TR7_CONS3(tsc,a,b,TR7_NIL)
#define TR7_LIST3(tsc,a,b,c)           TR7_CONS4(tsc,a,b,c,TR7_NIL)
#define TR7_LIST4(tsc,a,b,c,d)         TR7_CONS5(tsc,a,b,c,d,TR7_NIL)
#define TR7_LIST5(tsc,a,b,c,d,e)       TR7_CONS6(tsc,a,b,c,d,e,TR7_NIL)
#define TR7_LIST6(tsc,a,b,c,d,e,f)     TR7_CONS7(tsc,a,b,c,d,e,f,TR7_NIL)
#define TR7_LIST7(tsc,a,b,c,d,e,f,g)   TR7_CONS8(tsc,a,b,c,d,e,f,g,TR7_NIL)
#define TR7_LIST8(tsc,a,b,c,d,e,f,g,h) TR7_CONS9(tsc,a,b,c,d,e,f,g,h,TR7_NIL)
#define TR7_LIST9(tsc,a,b,c,d,e,f,g,h,i) \
                                       TR7_CONS10(tsc,a,b,c,d,e,f,g,h,i,TR7_NIL)
#define TR7_LIST10(tsc,a,b,c,d,e,f,g,h,i,j) \
                                     TR7_CONS11(tsc,a,b,c,d,e,f,g,h,i,j,TR7_NIL)
#define TR7_LIST11(tsc,a,b,c,d,e,f,g,h,i,j,k) \
                                   TR7_CONS12(tsc,a,b,c,d,e,f,g,h,i,j,k,TR7_NIL)
#define TR7_LIST12(tsc,a,b,c,d,e,f,g,h,i,j,k,l) \
                                 TR7_CONS13(tsc,a,b,c,d,e,f,g,h,i,j,k,l,TR7_NIL)
/*
* ### Function `tr7_list_length` and `tr7_unsafe_list_length`
*/
TR7_EXPORT int        tr7_list_length(tr7_t a);
TR7_EXPORT int        tr7_unsafe_list_length(tr7_t a);
/*
* The function `tr7_list_length` returns the length of the list `a`
* if `a` is a proper list.
* Returns -1 if a cycle is detected. Returns -2 minus length before the dot
* if the list is not terminated by `'()`. Note that it returns -2 when `a`
* isn't either a pair or null.
*
* Examples (see demo-c, show_tr7_list_length):
* **************************************************
* * tr7_list_length( '(a b c) )        -->  3
* * tr7_list_length( '(a b . c) )      --> -4
* * tr7_list_length( '#1=(a b . #1#) ) --> -1
* * tr7_list_length( 42 ) --> -2
* **************************************************
*
* The function `tr7_unsafe_list_length` returns the length
* of the proper list `a`. If `a` is neither a proper list nor
* a circular list, return -1 minus length befor the dot.
* It is an error if `a` is a circular list because the function never returns.
*
* Examples (see demo-c, show_tr7_unsafe_list_length):
* **************************************************
* * tr7_list_length( '(a b c) )        -->  3
* * tr7_list_length( '(a b . c) )      --> -3
* * tr7_list_length( 42 ) --> -1
* **************************************************
*
* ### Function `tr7_reverse_in_place`
*/
TR7_EXPORT tr7_t      tr7_reverse_in_place(tr7_t head, tr7_t tail);
/*
* The function `tr7_reverse_in_place` reverse the list given by `head`,
* putting `tail` at its end. Returns the head of the reversed list or
* returns `TR7_VOID` if `head` is not a proper list.
*
* The pairs element of the given list are directly changed to reverse
* the list. There is no allocation.
*
* !!! WARNING
*    This function doesn't detect circular list. If called on circular
*    list, the behaviour is strange, see below example.
*
* !!! WARNING
*    If called with a dot terminated list, `TR7_VOID` is returned
*    but the given list is mostly reversed in memory!
*
* Examples (see demo-c, show_tr7_reverse_in_place):
* **************************************************************************
* * tr7_reverse_in_place( '(a b c), '(A B C)) -->  '(c b a A B C)
* * tr7_reverse_in_place( '(a b . c), '(A))   -->  TR7_VOID
* * tr7_reverse_in_place( '(a . #1=(b c . #1#)), '(X Y))
* *                                           --> '(Y X a . #1=(b c . #1#))
* **************************************************************************
*
* ### Function `tr7_reverse`
*/
TR7_EXPORT tr7_t      tr7_reverse(tr7_engine_t tsc, tr7_t head, tr7_t tail);
/*
* The function `tr7_reverse` returns a new list that contains the reverse
* of the given list `head`, putting `tail` (not a copy) at its end.
* Returns the head of the reversed list or returns `TR7_VOID`
* if `head` is not a proper list.
*
* !!! CAUTION
*    Can trigger garbage collection
*
* !!! WARNING
*    This function doesn't detect circular list. If called on circular
*    list, it doesn't return until memory is exhausted.
*
* Examples:
* ***********************************************************************
* * tr7_reverse(tsc, '(a b c), '(A B C))            --> '(c b a A B C)
* * tr7_reverse(tsc, '(a b . c), '(A))              --> TR7_VOID
* * tr7_reverse(tsc, '(a . #1=(b c . #1#)), '(X Y)) --> CRASH, DON'T DO!
* ***********************************************************************
*
* ### Function `tr7_append`
*/
TR7_EXPORT tr7_t      tr7_append(tr7_engine_t tsc, int nitem, tr7_t items[]);
/*
* The function `tr7_append` appends the `nitems` lists of the array `items`
* in a single list and returns the created new one. As for the scheme
* procedure, the latest item is shared with the result and does not need
* to be a neither a list nor a proper list.
*
* If any item before the last is not a proper list, TR7_FALSE is returned.
*
* !!! CAUTION
*    Can trigger garbage collection
*
* Example (see demo-c, show_tr7_append):
* *******************************************************************
* * tr7_t items[] = {
* *         tr7_from_utf8(tsc, "#1=(A B C . #1#)"),
* *         tr7_from_utf8(tsc, "(A B C)"),
* *         tr7_from_utf8(tsc, "(M N O)"),
* *         tr7_from_utf8(tsc, "(X Y Z)"),
* *         tr7_from_int(tsc, 345) };
* * tr7_append(tsc, 2, &items[1])    --> '(A B C M N O)
* * tr7_append(tsc, 3, &items[2])    --> '(M N O X Y Z . 345)
* * tr7_append(tsc, 5, &items[0])    --> TR7_FALSE
* *******************************************************************
*
* ### Function `tr7_get_list_pairs`
*/
TR7_EXPORT int        tr7_get_list_pairs(tr7_t list, int count, tr7_pair_t pairs[]);
/*
* The function `tr7_get_list_pairs` extracts from the given `list` the first
* `count` pairs at most and store it in `pairs`. It returns the count of pairs
* extracted that can be less than count and even zero when `list` isn't a pair.
*
* Example (see demo-c, show_tr7_get_list_pairs):
* **************************************************************************
* *       +---------+          list
* * pairs |    *----+------->+-------+-------+
* *       +---------+        |   A   |   *   |
* *       |    *----+---.    +-------+---+---+
* *       +---------+    \               v
* *       |    *----+-.   '------------->+-------+-------+
* *       +---------+  \                 |   B   |   *   |
* *       |         |   \                +-------+---+---+
* *       +---------+    \                           v
* *       |         |     '------------------------->+-------+-------+
* *       +---------+                                |   C   |   /   |
* *       |         |                                +-------+-------+
* *       +---------+
* *
* * tr7_get_list_pairs( '1, pairs, 6)         --> 0
* * tr7_get_list_pairs( '(A B C), pairs, 6)   --> 3
* * tr7_get_list_pairs( '(A B C), pairs, 2)   --> 2
* * tr7_get_list_pairs( '(A B . C), pairs, 6) --> 2
* **************************************************************************
*
* ### Function `tr7_get_list_cars`
*/
TR7_EXPORT int        tr7_get_list_cars(tr7_t list, int count, tr7_t cars[], tr7_t *cdr);
/*
* The function `tr7_get_list_cars` extracts from the given `list` the first
* `count` CARs at most and store it in `cars`. If `cdr` is not NULL,
* it receives the CDR value corresponding to the last stored CAR.
*
* It returns the count of cars extracted that can be less than count.
*
* If `list` is not a pair, zero is returned and if not NULL `cdr`
* is set to `list`.
*
* Example (see demo-c, show_tr7_get_list_cars):
* **************************************************************************
* *       +---------+          list
* * cars  |    A    |        +-------+-------+
* *       +---------+        |   A   |   *   |
* *       |    B    |        +-------+---+---+
* *       +---------+                    v
* *                                      +-------+-------+
* * cdr *-.                              |   B   |   *   |
* *        |                             +-------+---+---+
* *        v                                         v
* *       +---------+                                +-------+-------+
* *       |    *----+------------------------------->|   C   |   /   |
* *       +---------+                                +-------+-------+
* *
* *
* * tr7_get_list_cars( '1, cars, 6, &cdr)         --> 0
* * tr7_get_list_cars( '(A B C), cars, 6, &cdr)   --> 3
* * tr7_get_list_cars( '(A B C), cars, 2, &cdr)   --> 2
* * tr7_get_list_cars( '(A B . C), cars, 6, &cdr) --> 2
* **************************************************************************
*
* ### Functions `tr7_unsafe_assq_pair`, `tr7_unsafe_memq_pair` and `tr7_unsafe_memv_pair`
*/
TR7_EXPORT tr7_pair_t tr7_unsafe_assq_pair(tr7_t x, tr7_t list);
TR7_EXPORT tr7_pair_t tr7_unsafe_memq_pair(tr7_t x, tr7_t list);
TR7_EXPORT tr7_pair_t tr7_unsafe_memv_pair(tr7_t x, tr7_t list);
/*
* The functions `tr7_unsafe_assq_pair`, `tr7_unsafe_memq_pair
* and `tr7_unsafe_memv_pair`
* are equivalent to the scheme procedure `assq`, `memq` and `memv`
* but differ in two ways:
*
* - it returns a pointer to the pair found or NULL if not found
* - it does not check if the list is circular, that is the reason
*   why it is tagged as 'unsafe'
*
* ### Functions `tr7_assoc_pair` and `tr7_member_pair`
*/
TR7_EXPORT tr7_pair_t tr7_assoc_pair(tr7_t x, tr7_t list, int (*eq)(tr7_t, tr7_t));
TR7_EXPORT tr7_pair_t tr7_member_pair(tr7_t x, tr7_t list, int (*eq)(tr7_t, tr7_t));
/*
* The functions `tr7_assoc_pair` and `tr7_member_pair`
* are equivalent to the scheme procedure `assoc` and `member`
* but differ in two ways:
*
* - they return a pointer to the pair found or NULL if not found
* - they take a C function as its equality predicate, that function
*   must return 0 if the 2 values are not equal or else, if equal,
*   must return a non zero value.
*
* Both detect the circular lists, returning NULL in that case.
*
* ### Function `tr7_ass?_pair` and `tr7_mem?_pair`
*/
TR7_EXPORT tr7_pair_t tr7_assq_pair(tr7_t x, tr7_t list);
TR7_EXPORT tr7_pair_t tr7_assv_pair(tr7_t x, tr7_t list);
TR7_EXPORT tr7_pair_t tr7_asse_pair(tr7_t x, tr7_t list);
TR7_EXPORT tr7_pair_t tr7_memq_pair(tr7_t x, tr7_t list);
TR7_EXPORT tr7_pair_t tr7_memv_pair(tr7_t x, tr7_t list);
TR7_EXPORT tr7_pair_t tr7_meme_pair(tr7_t x, tr7_t list);
/*
* These functions are equivalent equivalent to the scheme
* procedure `assq`, `assv`, `asse`, `memq`, `memv`, `meme`
* but differ in one way:
*
* - they return a pointer to the pair found or NULL if not found
*
* They detect the circular lists, returning TR7_FALSE in that case.
*
* ### Function `tr7_assoc` and `tr7_member`
*/
TR7_EXPORT tr7_t      tr7_assoc(tr7_t x, tr7_t list, int (*eq)(tr7_t, tr7_t));
TR7_EXPORT tr7_t      tr7_member(tr7_t x, tr7_t list, int (*eq)(tr7_t, tr7_t));
/*
* The functions `tr7_assoc` and `tr7_member` are equivalent
* to the scheme procedure `assoc` and `member` but differ in one ways:
*
* - they take a C function as its equality predicate, that function
*   must return 0 if the 2 values are not equal or else, if equal,
*   must return a non zero value.
*
* Both detect the circular lists, returning TR7_FALSE in that case.
*
* ### Functions `tr7_ass?` and `tr7_mem?`
*/
TR7_EXPORT tr7_t      tr7_assq(tr7_t x, tr7_t list);
TR7_EXPORT tr7_t      tr7_assv(tr7_t x, tr7_t list);
TR7_EXPORT tr7_t      tr7_asse(tr7_t x, tr7_t list);
TR7_EXPORT tr7_t      tr7_memq(tr7_t x, tr7_t list);
TR7_EXPORT tr7_t      tr7_memv(tr7_t x, tr7_t list);
TR7_EXPORT tr7_t      tr7_meme(tr7_t x, tr7_t list);
/*
* These functions are equivalent equivalent to the scheme
* procedure `assq`, `assv`, `asse`, `memq`, `memv`, `meme`.
*
* They detect the circular lists, returning TR7_FALSE in that case.
*
*****************************************************************************
* Defining tr7_head_t
* -------------------
*
* Any cell starts with a head that fits an intptr_t to be compatible
* in size and alignment with pointers being in the same heap.
*/
typedef intptr_t tr7_head_t;
/*
* The 6 least significant bits are used to hold flags.
* The remaining bits can be used to store any value.
*
* The idea is to implement the following schema:
*
* *******************************************************************
* *     <- 26 or 58 bits ->  5   4   3   2   1   0
* *    +---+---+ - - - - - +---+---+---+---+---+---+
* *    |   V A L U E       |MUT|    K I N D        |
* *    +---+---+ - - - - - +---+---+---+---+---+---+
* *                        <--------HEAD----------->
* *******************************************************************
*
* Where:
* - KIND    indicates the kind of cell (string, vector, ...)
* - MUT     if set indicates immutability of the cell (IMMUTABLE)
* - VALUE   any value having mean for the kind
*
* Values for bit field KIND
*/
#define TR7_SHIFT_HEAD_KIND        0
#define TR7_WIDTH_HEAD_KIND        5
#define TR7_MASK_HEAD_KIND         (((1 << TR7_WIDTH_HEAD_KIND) - 1) << TR7_SHIFT_HEAD_KIND)
/*
* Values for bit field IMMUTABLE
*/
#define TR7_SHIFT_HEAD_IMMUTABLE   (TR7_SHIFT_HEAD_KIND + TR7_WIDTH_HEAD_KIND)
#define TR7_WIDTH_HEAD_IMMUTABLE   1
#define TR7_MASK_HEAD_IMMUTABLE    (((1 << TR7_WIDTH_HEAD_IMMUTABLE) - 1) << TR7_SHIFT_HEAD_IMMUTABLE)
/*
* Values for meta bit field FLAGS
*/
#define TR7_WIDTH_HEAD_FLAGS       (TR7_WIDTH_HEAD_KIND + TR7_WIDTH_HEAD_IMMUTABLE)
#define TR7_MASK_HEAD_FLAGS        (((1 << TR7_WIDTH_HEAD_FLAGS) - 1) << TR7_SHIFT_HEAD_KIND)
/*
* Extract the kind of a header
*/
#define TR7_HEAD_KIND(h)           ((h) & TR7_MASK_HEAD_KIND)
/*
* Get and set the IMMUTABLE value of a header
*/
#define TR7_HEAD_IS_IMMUTABLE(h)   ((h) & TR7_MASK_HEAD_IMMUTABLE)
#define TR7_HEAD_SET_IMMUTABLE(h)  ((h) | TR7_MASK_HEAD_IMMUTABLE)
/*
* Get the meta values of FLAGS of a header
*/
#define TR7_HEAD_FLAGS(h)          ((h) & TR7_MASK_HEAD_FLAGS)
/*
* Make a cell's header from its VALUE and its FLAGS
*/
#define TR7_MAKE_HEAD(v,f)         ((tr7_head_t)(((v) << TR7_WIDTH_HEAD_FLAGS) | (f)))
/*
* Get and set signed VALUE of a header
*/
#define TR7_HEAD_VALUE(h)          (((tr7_int_t)(h)) >> TR7_WIDTH_HEAD_FLAGS)
#define TR7_HEAD_SET_VALUE(h,v)    TR7_MAKE_HEAD(v,TR7_HEAD_FLAGS(h))
/*
* Get and set unsigned VALUE of a header
*/
#define TR7_HEAD_UVALUE(h)         (((tr7_uint_t)(h)) >> TR7_WIDTH_HEAD_FLAGS)
#define TR7_HEAD_SET_UVALUE(h,v)   TR7_MAKE_HEAD(v,TR7_HEAD_FLAGS(h))
/*
**************************************************************************
* Defining kinds of heads
* -----------------------
*
* The following values are used as kinds for the TR7 internal cells.
*/
typedef enum tr7_head_kind
{
   Tr7_Head_Kind_None,           /* reserved */
   Tr7_Head_Kind_String,         /* strings */
   Tr7_Head_Kind_Symbol,         /* symbols */
   Tr7_Head_Kind_Byte_Vector,    /* bytevectors */
   Tr7_Head_Kind_Port,           /* ports */
   Tr7_Head_Kind_CFunction,      /* callable foreign functions */
   Tr7_Head_Kind_CPointer,       /* foreign pointer */
   Tr7_Head_Kind_Continuation,   /* continuations */
   Tr7_Head_Kind_Rational,       /* reserved for encoding rational numbers */
   Tr7_Head_Kind_Complex,        /* reserved for encoding complex numbers */
   Tr7_Head_Kind_Lambda,         /* callable lambdas */
   Tr7_Head_Kind_Case_Lambda,    /* callable case-lambdas */
   Tr7_Head_Kind_Promise,        /* promises */
   Tr7_Head_Kind_Parameter,      /* parameters */
   Tr7_Head_Kind_Transform,      /* defined syntaxes */
   Tr7_Head_Kind_Environment,    /* environments */
   Tr7_Head_Kind_Box,            /* boxes */
   Tr7_Head_Kind_Record,         /* records */
   Tr7_Head_Kind_Vector,         /* vectors */
   Tr7_Head_Kind_Big_Int,        /* reserved for encoding big integers */
   Tr7_Head_Kind_Program,        /* compiled code */
   Tr7_Head_Kind_RecFun,         /* functions for record access */
   _Tr7_Head_Kind_Count_         /* total count of kinds */
}
   tr7_head_kind_t;
/*
*****************************************************************************
* Defining tr7_cell_t
* -------------------
*
* Cell could have been a pointer to a tr7_head_t, it would be enough.
* Though defining it with the union is safer and allows more cleaner
* code.
*/
union tr7_cell
{
   tr7_head_t head;  /* common case, standard head of a cell */
   tr7_t      ____;  /* not used, here to ensure the union match a tr7_t */
};
/*
* Macros for manipulating cells, tr7_t value wraps a pointer to a cell
*
* - `TR7_IS_CELL(t)`: returns a boolean true if the tr7_t value `t` is for a cell
* - `TR7_TO_CELL(t)`: returns the tr_cell_t (pointer to cell) of the tr7_t value `t`
* - `TR7_AS_PAIR(t)`: returns the tr7_cell_t of the tr7_t value `t` if `t` is for a
*                   cell, or otherwise, if `t` isn't a cell, returns `NULL`
* - `TR7_FROM_CELL(c)`: returns the tr7_t value for the tr7_cell_t `c`
*/
#define TR7_IS_CELL(t)         TR7_IS_PTR_TAG(t,TR7_TAG_CELL)
#define TR7_TO_CELL(t)         ((tr7_cell_t)TR7_TO_PTR(t,TR7_TAG_CELL))
#define TR7_AS_CELL(t)         ((tr7_cell_t)TR7_AS_PTR(t,TR7_TAG_CELL))
#define TR7_FROM_CELL(c)       TR7_FROM_PTR(c,TR7_TAG_CELL)
/*
* Macros for manipulating cell's head from tr7_cell_t pointers
*
* - `TR7_CELL_HEAD(c)`: Returns the head of the tr7_cell_t `c`
* - `TR7_CELL_KIND(c)`: Returns the kind of the tr7_cell_t `c`
* - `TR7_CELL_IS_KIND(c,k)`: Returns a boolean true if the tr7_cell_t `c` is of kind `k`
* - `TR7_CELL_IS_IMMUTABLE(c)`: Returns a boolean true if the tr7_cell_t `c` is immutable
* - `TR7_CELL_SET_IMMUTABLE(c)`: makes the tr7_cell_t `c` immutable
* - `TR7_CELL_VALUE(c)`: Returns the head's signed value of the tr7_cell_t `c`
* - `TR7_CELL_UVALUE(c)`: Returns the head's unsigned value of the tr7_cell_t `c`
* - `TR7_CELL_SET_VALUE(c,v)`: Sets the head's signed value of the tr7_cell_t `c` to `v`
* - `TR7_CELL_SET_UVALUE(c,v)`: Sets the head's unsigned value of the tr7_cell_t `c` to `v`
*/
#define TR7_CELL_HEAD(c)          ((c)->head)
#define TR7_CELL_KIND(c)          TR7_HEAD_KIND(TR7_CELL_HEAD(c))
#define TR7_CELL_IS_KIND(c,k)     (TR7_CELL_KIND(c) == (k))
#define TR7_CELL_IS_IMMUTABLE(c)  TR7_HEAD_IS_IMMUTABLE(TR7_CELL_HEAD(c))
#define TR7_CELL_SET_IMMUTABLE(c) (TR7_CELL_HEAD(c) = TR7_HEAD_SET_IMMUTABLE(TR7_CELL_HEAD(c)))
#define TR7_CELL_VALUE(c)         TR7_HEAD_VALUE(TR7_CELL_HEAD(c))
#define TR7_CELL_UVALUE(c)        TR7_HEAD_UVALUE(TR7_CELL_HEAD(c))
#define TR7_CELL_SET_VALUE(c,v)   (TR7_CELL_HEAD(c) = TR7_HEAD_SET_VALUE(TR7_CELL_HEAD(c),v))
#define TR7_CELL_SET_UVALUE(c,v)  (TR7_CELL_HEAD(c) = TR7_HEAD_SET_UVALUE(TR7_CELL_HEAD(c),v))
/*
* Macros for manipulating cell's head from tr7_t values
*
* - TR7_HEAD_CELL(t): Returns the head of the tr7_t t
* - TR7_KIND_CELL(t): Returns the kind of the tr7_t t
* - TR7_IS_CELL_KIND(t,k): Returns a boolean true if the tr7_t t is of kind k
* - TR7_IS_IMMUTABLE_CELL(t): Returns a boolean true if the tr7_t t is immutable
* - TR7_SET_IMMUTABLE_CELL(t): makes the tr7_t t immutable
* - TR7_VALUE_CELL(t): Returns the head's signed value of the tr7_t t
* - TR7_UVALUE_CELL(t): Returns the head's unsigned value of the tr7_t t
* - TR7_SET_VALUE_CELL(t,v): Sets the head's signed value of the tr7_t t to v
* - TR7_SET_UVALUE_CELL(t,v): Sets the head's unsigned value of the tr7_t t to v
*/
#define TR7_HEAD_CELL(t)          TR7_CELL_HEAD(TR7_TO_CELL(t))
#define TR7_KIND_CELL(t)          TR7_CELL_KIND(TR7_TO_CELL(t))
#define TR7_IS_CELL_KIND(t,k)     (TR7_IS_CELL(t) && (TR7_CELL_IS_KIND(TR7_TO_CELL(t), (k))))
#define TR7_AS_CELL_KIND(t,k)     (TR7_IS_CELL_KIND(t,k) ? TR7_TO_CELL(t) : NULL)
#define TR7_IS_IMMUTABLE_CELL(t)  TR7_CELL_IS_IMMUTABLE(TR7_TO_CELL(t))
#define TR7_SET_IMMUTABLE_CELL(t) TR7_CELL_SET_IMMUTABLE(TR7_TO_CELL(t))
#define TR7_VALUE_CELL(t)         TR7_CELL_VALUE(TR7_TO_CELL(t))
#define TR7_UVALUE_CELL(t)        TR7_CELL_UVALUE(TR7_TO_CELL(t))
#define TR7_SET_VALUE_CELL(t,v)   TR7_CELL_SET_VALUE(TR7_TO_CELL(t))
#define TR7_SET_UVALUE_CELL(t,v)  TR7_CELL_SET_UVALUE(TR7_TO_CELL(t))
/*
**************************************************************************
* Defining the buffers
* --------------------
*
* Buffers are used for strings, symbols and bytevectors
*/
typedef struct tr7_buffer *tr7_buffer_t;
/*
* They are a structure made of:
*/
struct tr7_buffer {
   tr7_head_t head;   /* content length, immutability and kind */
   uint8_t *content;  /* pointer to the content */
};
/*
* Macros for manipulating buffers from tr7_buffer_t values
*
* - TR7_BUFFER_HEAD(b): Returns the head of the tr7_buffer_t b
* - TR7_BUFFER_CONTENT(b): Returns the content of the tr7_buffer_t b
* - TR7_BUFFER_LENGTH(b): Returns the unsigned length of the tr7_buffer_t b
* - TR7_BUFFER_SET_LENGTH(b,v): Sets the unsigned length of the tr7_buffer_t b to v
*/
#define TR7_BUFFER_HEAD(b)          (b)->head
#define TR7_BUFFER_CONTENT(b)       ((b)->content)
#define TR7_BUFFER_LENGTH(b)        TR7_HEAD_UVALUE(TR7_BUFFER_HEAD(b))
#define TR7_BUFFER_SET_LENGTH(b,v)  (TR7_BUFFER_HEAD(b) = TR7_HEAD_SET_UVALUE(TR7_BUFFER_HEAD(b),v))
/*
* Macros for manipulating buffers from tr7_cell_t values
*
* - TR7_CELL_TO_BUFFER(c): Returns the buffer value of the tr7_cell_t c
* - TR7_CELL_CONTENT_BUFFER(c): Returns the buffer content of the tr7_cell_t c
* - TR7_CELL_LENGTH_BUFFER(c): Returns the buffer unsigned length of the tr7_cell_t c
*/
#define TR7_CELL_TO_BUFFER(c)       ((tr7_buffer_t)(c))
#define TR7_CELL_CONTENT_BUFFER(c)  TR7_BUFFER_CONTENT(TR7_CELL_TO_BUFFER(c))
#define TR7_CELL_LENGTH_BUFFER(c)   TR7_BUFFER_LENGTH(TR7_CELL_TO_BUFFER(c))
/*
* Macros for manipulating buffers from tr7_t values
*
* - TR7_TO_BUFFER(t): Returns the buffer value of the tr7_t t
* - TR7_FROM_BUFFER(b): Returns the tr7_t value of the tr7_buffer_t b
* - TR7_CONTENT_BUFFER(t): Returns the buffer content of the tr7_t t
* - TR7_LENGTH_BUFFER(t): Returns the buffer unsigned length of the tr7_t t
* - TR7_SET_LENGTH_BUFFER(t,v): Sets the buffer unsigned length of the tr7_t t to v
*/
#define TR7_TO_BUFFER(t)            TR7_CELL_TO_BUFFER(TR7_TO_CELL(t))
#define TR7_FROM_BUFFER(b)          TR7_FROM_CELL(b)
#define TR7_CONTENT_BUFFER(t)       TR7_BUFFER_CONTENT(TR7_TO_BUFFER(t))
#define TR7_LENGTH_BUFFER(t)        TR7_BUFFER_LENGTH(TR7_TO_BUFFER(t))
#define TR7_SET_LENGTH_BUFFER(t,v)  TR7_BUFFER_SET_LENGTH(TR7_TO_BUFFER(t),v)
/*
**************************************************************************
* Defining bytevectors
* --------------------
*
* Bytevectors are implemented using buffers
*
* Macros for manipulating buffers from tr7_buffer_t values
*
* - TR7_BYTEVECTOR_CONTENT(b): Returns the content of the tr7_buffer_t b
* - TR7_BYTEVECTOR_LENGTH(b): Returns the unsigned length of the tr7_buffer_t b
* - TR7_BYTEVECTOR_SET_LENGTH(b,v): Sets the unsigned length of the tr7_buffer_t b to v
*/
#define TR7_BYTEVECTOR_CONTENT(b)      TR7_BUFFER_CONTENT(b)
#define TR7_BYTEVECTOR_LENGTH(b)       TR7_BUFFER_LENGTH(b)
#define TR7_BYTEVECTOR_SET_LENGTH(b,v) TR7_BUFFER_SET_LENGTH(b,v)
/*
* Macros for manipulating bytevectors from tr7_cell_t values
*
* - TR7_CELL_IS_BYTEVECTOR(c): Returns true if the tr7_cell_t c is a bytevector
* - TR7_CELL_TO_BYTEVECTOR(c): Returns the bytevector value of the tr7_cell_t c
* - TR7_CELL_CONTENT_BYTEVECTOR(c): Returns the bytevector content of the tr7_cell_t c
* - TR7_CELL_LENGTH_BYTEVECTOR(c): Returns the bytevector unsigned length of the tr7_cell_t c
*/
#define TR7_CELL_IS_BYTEVECTOR(c)      TR7_CELL_IS_KIND(c, Tr7_Head_Kind_Byte_Vector)
#define TR7_CELL_TO_BYTEVECTOR(c)      TR7_CELL_TO_BUFFER(c)
#define TR7_CELL_CONTENT_BYTEVECTOR(c) TR7_BYTEVECTOR_CONTENT(TR7_CELL_TO_BYTEVECTOR(c))
#define TR7_CELL_LENGTH_BYTEVECTOR(c)  TR7_BYTEVECTOR_LENGTH(TR7_CELL_TO_BYTEVECTOR(c))
/*
* Macros for manipulating bytevectors from tr7_t values
*
* - TR7_IS_BYTEVECTOR(t): Returns true if the tr7_t t is a bytevector
* - TR7_TO_BYTEVECTOR(t): Returns the bytevector value of the tr7_t t
* - TR7_AS_BYTEVECTOR(t): Returns the bytevector value of the tr7_t t
*                         or return NULL when t is not a bytevector
* - TR7_FROM_BYTEVECTOR(b): Returns the tr7_t value of the tr7_buffer_t b
* - TR7_CONTENT_BYTEVECTOR(t): Returns the bytevector content of the tr7_t t
* - TR7_LENGTH_BYTEVECTOR(t): Returns the bytevector unsigned length of the tr7_t t
* - TR7_SET_LENGTH_BYTEVECTOR(t,v): Sets the bytevector unsigned length of the tr7_t t to v
*/
#define TR7_IS_BYTEVECTOR(t)           TR7_IS_CELL_KIND((t), Tr7_Head_Kind_Byte_Vector)
#define TR7_TO_BYTEVECTOR(t)           TR7_TO_BUFFER(t)
#define TR7_AS_BYTEVECTOR(t)           (TR7_IS_BYTEVECTOR(t) ? TR7_TO_BYTEVECTOR(t) : NULL)
#define TR7_FROM_BYTEVECTOR(b)         TR7_FROM_BUFFER(b)
#define TR7_CONTENT_BYTEVECTOR(t)      TR7_BYTEVECTOR_CONTENT(TR7_TO_BYTEVECTOR(t))
#define TR7_LENGTH_BYTEVECTOR(t)       TR7_BYTEVECTOR_LENGTH(TR7_TO_BYTEVECTOR(t))
#define TR7_SET_LENGTH_BYTEVECTOR(t,v) TR7_BYTEVECTOR_SET_LENGTH(TR7_TO_BYTEVECTOR(t),v)
/*
* ### functions `tr7_make_bytevector...` for creating bytevectors
*/
TR7_EXPORT tr7_t tr7_make_bytevector(tr7_engine_t tsc, size_t len);
TR7_EXPORT tr7_t tr7_make_bytevector_fill(tr7_engine_t tsc, uint8_t byte, size_t len);
TR7_EXPORT tr7_t tr7_make_bytevector_copy(tr7_engine_t tsc, const uint8_t *bytes, size_t len);
TR7_EXPORT tr7_t tr7_make_bytevector_take(tr7_engine_t tsc, uint8_t *bytes, size_t len);
TR7_EXPORT tr7_t tr7_make_bytevector_static(tr7_engine_t tsc, uint8_t *bytes, size_t len);
/*
* The function `tr7_make_bytevector` creates a bytevector of length `len`
* but does not initializes the allocated memory that get randoms value.
*
* The function `tr7_make_bytevector_fill` creates a bytevector of
* length `len` and initialize it with the value `byte`.
*
* The function `tr7_make_bytevector_copy` creates a bytevector of
* length `len` and initialize it with the copy of `bytes`.
*
* The function `tr7_make_bytevector_take` creates a bytevector of
* length `len` holding the given buffer `bytes`. When the created value
* is not more used, the garbage collector free the given buffer using
* its free function.
*
* The function `tr7_make_bytevector_static` creates a bytevector of
* length `len` holding the given buffer `bytes`. The given buffer is
* never freed.
*
* All these functions are returning TR7_NIL when allocation fails.
*
* !!! CAUTION
*    All these functions can trigger garbage collection
*
**************************************************************************
* Defining strings
* ----------------
*
* Strings are implemented using buffers. Internal representation
* is using UTF8 zero terminated strings. But the ending zero
* is here only for convenience in interaction with C and not
* used internally. So the ending zero is not mandatory.
*
* Strings are restricted to hold only valid UNICODE characters.
* This restriction improves system interaction and simplify some
* internal algorithms.
*
* The recorded size is the size in bytes of the UTF8 string.
* It means that it is not the count of characters.
* For strings, the count of characters is the length
*
* Macros for manipulating strings from tr7_buffer_t values
*
* - TR7_STRING_CONTENT(s): Returns the content of the tr7_buffer_t s
* - TR7_STRING_SIZE(s): Returns the unsigned length of the tr7_buffer_t s
* - TR7_STRING_SET_SIZE(s,v): Sets the unsigned length of the tr7_buffer_t s
*                             to v
*/
#define TR7_STRING_CONTENT(s)     TR7_BUFFER_CONTENT(s)
#define TR7_STRING_SIZE(s)        TR7_BUFFER_LENGTH(s)
#define TR7_STRING_SET_SIZE(s,v)  TR7_BUFFER_SET_LENGTH(s,v)
/*
* Macros for manipulating strings from tr7_cell_t values
*
* - TR7_CELL_IS_STRING(c): Returns true if the tr7_cell_t c is a string
* - TR7_CELL_TO_STRING(c): Returns the string value of the tr7_cell_t c
* - TR7_CELL_CONTENT_STRING(c): Returns the string content of the tr7_cell_t c
* - TR7_CELL_SIZE_STRING(c): Returns the string unsigned length
*                            of the tr7_cell_t c
*/
#define TR7_CELL_IS_STRING(c)       TR7_CELL_IS_KIND(c, Tr7_Head_Kind_String)
#define TR7_CELL_TO_STRING(c)       TR7_CELL_TO_BUFFER(c)
#define TR7_CELL_CONTENT_STRING(c)  TR7_STRING_CONTENT(TR7_CELL_TO_STRING(c))
#define TR7_CELL_SIZE_STRING(c)     TR7_STRING_SIZE(TR7_CELL_TO_STRING(c))
/*
* Macros for manipulating strings from tr7_t values
*
* - TR7_IS_STRING(t): Returns true if the tr7_t t is a string
* - TR7_TO_STRING(t): Returns the string value of the tr7_t t
* - TR7_AS_STRING(t): Returns the string value of the tr7_t t
*                   or return NULL when t is not a string
* - TR7_FROM_STRING(s): Returns the tr7_t value of the tr7_buffer_t s
* - TR7_CONTENT_STRING(t): Returns the string content of the tr7_t t
* - TR7_SIZE_STRING(t): Returns the string unsigned length of the tr7_t t
* - TR7_SET_SIZE_STRING(t,v): Sets the string unsigned length of the tr7_t t
*                              to v
*/
#define TR7_IS_STRING(t)            TR7_IS_CELL_KIND((t), Tr7_Head_Kind_String)
#define TR7_TO_STRING(t)            TR7_TO_BUFFER(t)
#define TR7_AS_STRING(t)            (TR7_IS_STRING(t) ? TR7_TO_STRING(t) : NULL)
#define TR7_FROM_STRING(s)          TR7_FROM_BUFFER(s)
#define TR7_CONTENT_STRING(t)       TR7_STRING_CONTENT(TR7_TO_STRING(t))
#define TR7_SIZE_STRING(t)          TR7_STRING_SIZE(TR7_TO_STRING(t))
#define TR7_SET_SIZE_STRING(t,v)    TR7_STRING_SET_SIZE(TR7_TO_STRING(t),v)
/*
* ### functions `tr7_make_string...` for creating strings
*/
TR7_EXPORT tr7_t tr7_make_string_fill(tr7_engine_t tsc, tr7_char_t car, size_t ncars);
TR7_EXPORT tr7_t tr7_make_string_copy_length(tr7_engine_t tsc, const char *sutf8, size_t size);
TR7_EXPORT tr7_t tr7_make_string_copy(tr7_engine_t tsc, const char *sutf8);
TR7_EXPORT tr7_t tr7_make_string_take_length(tr7_engine_t tsc, char *sutf8, size_t size);
TR7_EXPORT tr7_t tr7_make_string_take(tr7_engine_t tsc, char *sutf8);
TR7_EXPORT tr7_t tr7_make_string_static_length(tr7_engine_t tsc, const char *sutf8, size_t size);
TR7_EXPORT tr7_t tr7_make_string_static(tr7_engine_t tsc, const char *sutf8);
TR7_EXPORT tr7_t tr7_make_string_length(tr7_engine_t tsc, const char *sutf8, size_t size, int copy);
TR7_EXPORT tr7_t tr7_make_string(tr7_engine_t tsc, const char *sutf8, int copy);
/*
* The function `tr7_make_string_fill` creates a string of `ncars` characters
* initialized to `car`. If `car` is not a valid UNICODE car, TR7_NIL is returned.
*
* The function `tr7_make_string_copy_length` creates a string of
* `size` and initialize it with the copy of the UTF8 string `sutf8`.
* If the string is not a valid UTF8 encoded UNICODE string, TR7_NIL is returned.
*
* The function `tr7_make_string_copy` compute the length of `sutf8`
* that must be zero and calls `tr7_make_string_copy_length` to create
* the string.
* If the string is not a valid UTF8 encoded UNICODE string, TR7_NIL is returned.
*
* The function `tr7_make_string_take_length` creates a string of
* `size` holding the given UTF8 string 'sutf8'. When the created value
* is not more used, the garbage collector free the given string using
* its free function.
* If the string is not a valid UTF8 encoded UNICODE string, TR7_NIL is returned.
*
* The function `tr7_make_string_take` compute the length of `sutf8`
* that must be zero and calls `tr7_make_string_take_length` to create
* the string.
* If the string is not a valid UTF8 encoded UNICODE string, TR7_NIL is returned.
*
* The function `tr7_make_string_static_length` creates a string of
* `size` holding the given UTF8 string 'sutf8'. The given string is
* never freed.
* If the string is not a valid UTF8 encoded UNICODE string, TR7_NIL is returned.
*
* The function `tr7_make_string_static` compute the length of `sutf8`
* that must be zero and calls `tr7_make_string_static_length` to create
* the string.
* If the string is not a valid UTF8 encoded UNICODE string, TR7_NIL is returned.
*
* The function `tr7_make_string_length` creates a string of
* `size` using `tr7_make_string_static_length` when `copy` is zero
* or usign `tr7_make_string_copy_length` otherwise.
* If the string is not a valid UTF8 encoded UNICODE string, TR7_NIL is returned.
*
* The function `tr7_make_string` creates a string of
* `size` using `tr7_make_string_static` when `copy` is zero
* or usign `tr7_make_string_copy` otherwise.
* If the string is not a valid UTF8 encoded UNICODE string, TR7_NIL is returned.
*
* All these functions are returning TR7_NIL when allocation fails or
* if the given string is not a valid UTF8 encoded UNICODE string.
*
* !!! CAUTION
*    All these functions can trigger garbage collection
*
* ### functions for manipulating strings
*/
TR7_EXPORT int         tr7_is_string(tr7_t item);
TR7_EXPORT size_t      tr7_string_size(tr7_t string);
TR7_EXPORT const char *tr7_string_buffer(tr7_t string);
TR7_EXPORT size_t      tr7_string_length(tr7_t string);
TR7_EXPORT tr7_char_t  tr7_string_ref(tr7_t string, size_t pos);
TR7_EXPORT int         tr7_string_set(tr7_engine_t tsc, tr7_t string, size_t pos, tr7_char_t car);
/*
* The function `tr7_is_string` test if `item` is a string and returns
* 1 if it is the case and 0 otherwise.
*
* The function `tr7_string_size` returns the count of bytes of the buffer
* holding the UTF8 string. The argument `string` MUST be a string.
*
* The function `tr7_string_buffer` returns the UTF8 string buffer.
* The argument `string` MUST be a string.
*
* The function `tr7_string_length` returns the count of characters
* of the `string`. The argument `string` MUST be a string.
*
* The function `tr7_string_ref` returns the character of `string` at position
* `pos`. The argument `string` MUST be a string. The argument `pos`
* MUST be a valid position: greater or equal to 0 and less than the length.
* If pos is invalid, the value -1 is returned.
*
* The function `tr7_string_set` set the character of `string` at position
* `pos` to the character `car`. The argument `string` MUST be a string.
* The argument `pos` MUST be a valid position: greater or equal to 0 and
* less than the length. It returns 1 is all went good or 0 if allocation
* failed or `pos` is erroneous
*
**************************************************************************
* Defining symbols
* ----------------
*
* Symbols are implemented using buffers
*
* Macros for manipulating symbols from tr7_buffer_t values
*
* - TR7_SYMBOL_CONTENT(s): Returns the content of the tr7_buffer_t s
* - TR7_SYMBOL_SIZE(s): Returns the unsigned length of the tr7_buffer_t s
*/
#define TR7_SYMBOL_SIZE(s)        TR7_BUFFER_LENGTH(s)
#define TR7_SYMBOL_CONTENT(s)     TR7_BUFFER_CONTENT(s)
/*
* Macros for manipulating symbols from tr7_cell_t values
*
* - TR7_CELL_IS_SYMBOL(c): Returns true if the tr7_cell_t c is a symbol
* - TR7_CELL_TO_SYMBOL(c): Returns the symbol value of the tr7_cell_t c
* - TR7_CELL_CONTENT_SYMBOL(c): Returns the symbol content of the tr7_cell_t c
* - TR7_CELL_SIZE_SYMBOL(c): Returns the symbol unsigned length of the tr7_cell_t c
*/
#define TR7_CELL_IS_SYMBOL(c)      TR7_CELL_IS_KIND(c, Tr7_Head_Kind_Symbol)
#define TR7_CELL_TO_SYMBOL(c)      TR7_CELL_TO_BUFFER(c)
#define TR7_CELL_CONTENT_SYMBOL(c) TR7_SYMBOL_CONTENT(TR7_CELL_TO_SYMBOL(c))
#define TR7_CELL_SIZE_SYMBOL(c)    TR7_SYMBOL_SIZE(TR7_CELL_TO_SYMBOL(c))
/*
* Macros for manipulating symbols from tr7_t values
*
* - TR7_IS_SYMBOL(t): Returns true if the tr7_t t is a symbol
* - TR7_TO_SYMBOL(t): Returns the symbol value of the tr7_t t
* - TR7_AS_SYMBOL(t): Returns the symbol value of the tr7_t t
*                   or return NULL when t is not a symbol
* - TR7_FROM_SYMBOL(s): Returns the tr7_t value of the tr7_buffer_t s
* - TR7_CONTENT_SYMBOL(t): Returns the symbol content of the tr7_t t
* - TR7_SIZE_SYMBOL(t): Returns the symbol unsigned length of the tr7_t t
*/
#define TR7_IS_SYMBOL(t)        TR7_IS_CELL_KIND((t), Tr7_Head_Kind_Symbol)
#define TR7_TO_SYMBOL(t)        TR7_TO_BUFFER(t)
#define TR7_AS_SYMBOL(t)        (TR7_IS_SYMBOL(t) ? TR7_TO_SYMBOL(t) : NULL)
#define TR7_FROM_SYMBOL(s)      TR7_FROM_BUFFER(s)
#define TR7_CONTENT_SYMBOL(t)   TR7_SYMBOL_CONTENT(TR7_TO_SYMBOL(t))
#define TR7_SIZE_SYMBOL(t)      TR7_SYMBOL_SIZE(TR7_TO_SYMBOL(t))
/*
* ### functions for manipulating symbols
*/
TR7_EXPORT int         tr7_is_symbol(tr7_t item);
TR7_EXPORT const char *tr7_symbol_string(tr7_t symbol);
TR7_EXPORT size_t      tr7_symbol_size(tr7_t symbol);
TR7_EXPORT size_t      tr7_symbol_length(tr7_t symbol);
TR7_EXPORT tr7_char_t  tr7_symbol_ref(tr7_t symbol, size_t pos);
/*
* The function `tr7_is_symbol` test if `item` is a symbol and returns
* 1 if it is the case and 0 otherwise.
*
* The function `tr7_symbol_size` returns the count of bytes of the buffer
* holding the UTF8 symbol. The argument `symbol` MUST be a symbol.
*
* The function `tr7_symbol_buffer` returns the UTF8 symbol buffer.
* The argument `symbol` MUST be a symbol.
*
* The function `tr7_symbol_length` returns the count of characters
* of the `symbol`. The argument `symbol` MUST be a symbol.
*
* The function `tr7_symbol_ref` returns the character of `symbol` at position
* `pos`. The argument `symbol` MUST be a symbol. The argument `pos`
* MUST be a valid position: greater or equal to 0 and less than the length.
* If pos is invalid, the value -1 is returned.
*
* ### functions for getting symbols from the dictionary
*/
TR7_EXPORT tr7_t tr7_symbol_lookup(tr7_engine_t tsc, const char *name, size_t length, int copy, int create);
TR7_EXPORT tr7_t tr7_get_symbol_length(tr7_engine_t tsc, const char *name, size_t size, int copy);
TR7_EXPORT tr7_t tr7_get_symbol(tr7_engine_t tsc, const char *name, int copy);
/*
* The function `tr7_symbol_lookup` returns the symbol of the
* dictionary having the UTF8 `name` of `size`. If the symbol is not
* already in the dictionary, and `create` is not zero, creates the symbol
* either by copying if `copy` is not zero or by referencing `name` directly.
* Otherwise, if the symbol is not in the dictionary and `create` is zero
* returns TR7_FALSE.
*
* The function `tr7_get_symbol_length` calls `tr7_symbol_lookup` with
* `create`=1, meaning it always return a symbol.
*
* The function `tr7_get_symbol` computes the `size` of `name` and
* calls `tr7_get_symbol_length`.
*
* !!! CAUTION
*    All these functions can trigger garbage collection (except if `create` == 0)
*
**************************************************************************
* Defining vectors
* ----------------
*
* Vectors are used to store tr7_t values contiguously in memory
*/
typedef struct tr7_vector *tr7_vector_t;
/*
* They are a structure made of:
*/
struct tr7_vector
{
   tr7_head_t head;     /* content length, immutability and kind */
   tr7_t      items[];  /* the items */
};
/*
* Macros for manipulating vectors from tr7_vector_t values
*
* - TR7_VECTOR_LENGTH(v): Returns the unsigned length of the tr7_vector_t v
* - TR7_VECTOR_ITEMS(v): Returns the array of items of the tr7_vector_t v
* - TR7_VECTOR_ITEM(v,i): Returns the item of index i of the tr7_vector_t v
*                         Can be used for getting and setting
*/
#define TR7_VECTOR_LENGTH(v)        TR7_HEAD_UVALUE((v)->head)
#define TR7_VECTOR_ITEMS(v)         ((v)->items)
#define TR7_VECTOR_ITEM(v,i)        (TR7_VECTOR_ITEMS(v)[i])
/*
* Macros for manipulating vectors from tr7_cell_t values
*
* - TR7_CELL_IS_VECTOR(c): Returns true if the tr7_cell_t c is a vector
* - TR7_CELL_TO_VECTOR(c): Returns the vector value of the tr7_cell_t c
* - TR7_CELL_VECTOR_LENGTH(c): Returns the vector unsigned length of the tr7_cell_t c
* - TR7_CELL_VECTOR_ITEMS(c): Returns the vector's array of items of the tr7_cell_t c
* - TR7_CELL_VECTOR_ITEM(c,i): Returns the vector's item of index i of the tr7_cell_t c
*/
#define TR7_CELL_IS_VECTOR(c)       TR7_CELL_IS_KIND((c), Tr7_Head_Kind_Vector)
#define TR7_CELL_TO_VECTOR(c)       ((tr7_vector_t)(c))
#define TR7_CELL_VECTOR_LENGTH(c)   TR7_VECTOR_LENGTH(TR7_CELL_TO_VECTOR(c))
#define TR7_CELL_VECTOR_ITEMS(c)    TR7_VECTOR_ITEMS(TR7_CELL_TO_VECTOR(c))
#define TR7_CELL_VECTOR_ITEM(c,i)   TR7_VECTOR_ITEM(TR7_CELL_TO_VECTOR(c),i)
/*
* Macros for manipulating vectors from tr7_t values
*
* - TR7_IS_VECTOR(t): Returns true if the tr7_t t is a vector
* - TR7_TO_VECTOR(t): Returns the vector value of the tr7_t t
* - TR7_AS_VECTOR(t): Returns the vector value of the tr7_t t
*                   or return NULL when t is not a vector
* - TR7_FROM_VECTOR(v): Returns the tr7_t value of the tr7_vector_t v
* - TR7_LENGTH_VECTOR(t): Returns the vector unsigned length of the tr7_t t
* - TR7_ITEMS_VECTOR(t): Returns the vector's array of items of the tr7_t t
* - TR7_ITEM_VECTOR(t,i): Returns the vector's item of index i of the tr7_t t
*/
#define TR7_IS_VECTOR(t)            TR7_IS_CELL_KIND((t), Tr7_Head_Kind_Vector)
#define TR7_TO_VECTOR(t)            ((tr7_vector_t)TR7_TO_CELL(t))
#define TR7_AS_VECTOR(t)            (TR7_IS_VECTOR(t) ? TR7_TO_VECTOR(t) : NULL)
#define TR7_FROM_VECTOR(v)          TR7_FROM_CELL(v)
#define TR7_LENGTH_VECTOR(t)        TR7_VECTOR_LENGTH(TR7_TO_VECTOR(t))
#define TR7_ITEMS_VECTOR(t)         TR7_VECTOR_ITEMS(TR7_TO_VECTOR(t))
#define TR7_ITEM_VECTOR(t,i)        TR7_VECTOR_ITEM(TR7_TO_VECTOR(t),i)
/*
* ### functions `tr7_make_vector...` for creating vectors
*/
TR7_EXPORT tr7_t tr7_make_vector_stride(tr7_engine_t tsc, size_t len, tr7_t *items, size_t stride);
TR7_EXPORT tr7_t tr7_make_vector_copy(tr7_engine_t tsc, size_t len, tr7_t *items);
TR7_EXPORT tr7_t tr7_make_vector_fill(tr7_engine_t tsc, size_t len, tr7_t value);
/*
* The function `tr7_make_vector_stride` creates a vector of `len` elements,
* the vector is initialized by copying values pointed by `items` separated
* by `stride`: i.e.: result[i] = items[i*stride]
* `items` must be an array of at least `1 + (len - 1) * stride` values.
*
* The function `tr7_make_vector_copy` creates a vector of `len` elements,
* the vector is initialized by copying the values pointed by `items` that
* must be an array of at least `len` values.
*
* The function `tr7_make_vector_fill` creates a vector of `len` elements,
* each element being `value`.
*
* All these functions are returning TR7_NIL when allocation fails.
*
* !!! CAUTION
*    All these functions can trigger garbage collection
*
* ### functions for manipulating vectors
*/
TR7_EXPORT size_t tr7_vector_length(tr7_t vec);
TR7_EXPORT tr7_t  tr7_vector_ref(tr7_t vec, size_t ielem);
TR7_EXPORT tr7_t  tr7_vector_set(tr7_t vec, size_t ielem, tr7_t value);
/*
* The function `tr7_vector_length` returns the count of elements of
* `vec`.
*
* The function `tr7_vector_ref` returns the element of index `ielem`
* of `vec`.
*
* The function `tr7_vector_set` set the element of index `ielem`
* of `vec` to the value `value`.
*
* !!! CAUTION
*    For aa these functions `vec` MUST be a vector and `ielem` MUST be
*    a valid index: `0 <= ielem < tr7_vector_length(vec)`
*
**************************************************************************
* Defining foreign pointers
* -------------------------
*
* It is safe to pass any pointer as tr7_t value. Nevertheless, it can
* have hard to predict side effects. For this reason, it is safer to
* use the facility offered by TR7 to manage the pointer issued by C.
* Moreover, it allows to track life cycle of the pointer.
*
* Management of foreign pointers introduces 2 hooks in the life cycle
* of pointers: one when the pointer is marked alive and one when the
* pointer is dead.
*
* The hook called for marking the pointer as alive, `marker` can be used
* to mark TR7 values hold by the pointer context using the function
* `tr7_mark`. This is a bit lighter than using `tr7_hold`/`tr7_unhold`
* when the related TR7's values are linked to the pointer.
*
* The hook called when the pointer is dead for TR7, `disposer` can be
* used to release resources linked to the pointer.
*
* ### foreign pointer hooks
*
* Both hooks are defined as below:
*/
typedef void (*tr7_cptr_cb_t)(tr7_engine_t, void*);
/*
* The first received argument is the engine where the pointer
* is/was recorded.
*
* The second received argument is the recorded pointer (NOTE WELL,
* not the TR7 value holding the pointer, the recorded pointer).
*
* The hooks are grouped together in a vtable. The vtable pointer
* itself can be used as a 'tag' or ID for the foreign pointer in
* order to distinguish different kinds of foreign pointers.
* In that way, it can also be used in conjunction with 'offsetof'
* 'container_of' to add typing data.
*/
typedef const struct tr7_cptr_vtable tr7_cptr_vtable_t;
struct tr7_cptr_vtable
{
   tr7_cptr_cb_t     marker;   /* marker callback */
   tr7_cptr_cb_t     disposer; /* disposer callback */
};
/*
* Also in order to avoid 2 calls when one can be enough, the
* structure tr7_foreign groups the foreign pointer and its vtable.
*/
typedef struct tr7_foreign tr7_foreign_t;
struct tr7_foreign
{
   void *pointer;
   tr7_cptr_vtable_t *vtable;
};
/*
* ### Functions for holding foreign pointers
*/
TR7_EXPORT tr7_t tr7_make_foreign_pointer(tr7_engine_t tsc, void *pointer, tr7_cptr_vtable_t *vtable);
TR7_EXPORT void *tr7_get_foreign_pointer(tr7_t value);
TR7_EXPORT tr7_cptr_vtable_t *tr7_get_foreign_pointer_vtable(tr7_t value);
TR7_EXPORT tr7_foreign_t tr7_get_foreign(tr7_t value);
/*
* The function `tr7_make_foreign_pointer` creates a value for holding the
* `pointer`. The usage of the vtable is described above. Any of the hook can be NULL.
*
* The function `tr7_get_foreign_pointer` returns the pointer stored in `value`
* or NULL if `value` is not a foreign pointer value.
*
* The function `tr7_get_foreign_pointer_vtable` returns the vtable stored in `value`
* or NULL if `value` is not a foreign pointer value.
*
* The function `tr7_get_foreign` returns the foreign pointer and the vtable
* stored in `value or both NULL if `value` is not a foreign pointer value.
*
**************************************************************************
* Defining foreign functions
* --------------------------
*
* Foreign functions are allowing SCHEME to call (transfer execution control)
* to C function (or other languages though C).
*
* The invoked foreign functions receives argument and returns value(s)
* on normal flow or raise a non-continuable error on exceptional conditions.
*
* Before returning (if it returns) a foreign function is allowed to create
* TR7 values and to call SCHEME procedures.
*
* ### return status of foreign functions
*
* The type `tr7_C_return_t` defined below defines values that can return
* a foreign function.
*/
typedef enum
{
   Tr7_C_Return_Raise = 0, /* raise a not continuable error */
   Tr7_C_Return_Ok    = 1  /* normal termination */
}
   tr7_C_return_t;
/*
* It has only two defined values that are `Tr7_C_Return_Raise` and
* `Tr7_C_Return_Ok`. Internally, it is treated as zero or not zero.
*
* The functions of the family `tr7_C_return...` or `tr7_C_raise...`
* return the correct corresponding value and are generally used
* for returning the status.
*
* ### returning values from a foreign function
*
* When foreign functions are invoked, the engine context is configured
* for them to return nothing. So in order to return value(s), to SCHEME
* the foreign function must call one of the function below:
*/
TR7_EXPORT tr7_C_return_t tr7_C_return_single(tr7_engine_t tsc, tr7_t value);
TR7_EXPORT tr7_C_return_t tr7_C_return_values(tr7_engine_t tsc, unsigned count, tr7_t array[]);
/*
* The function `tr7_C_return_single` returns the given `value`.
*
* The function `tr7_C_return_values` returns the `count` values pointed
* by `array`. Values are copied so `array` can be in stack.
*
* All these functions are returning `Tr7_C_Return_Ok` so it can be used for
* returning from foreign function at its end as below:
*
* ```C
* tr7_C_return_t five(tr7_engine_t tsc, int nval, tr7_t *values, void *closure)
* {
*    return tr7_C_return_single(tsc, TR7_FROM_INT(5));
* }
* ```
*
* However, returning directly after setting the value is not mandatory.
* So it is possible to set the returned value in sub calls, for example.
*
* Also, calling these functions, and the functions of family
* `tr7_C_raise...` (see below), more than one time is possible.
* The last call take precedence over the previous ones.
*
* And it is always possible to return nothing by calling
* `tr7_C_return_single(tsc, TR7_VOID)`.
*
* ### returning exceptions from a foreign function
*/
TR7_EXPORT tr7_C_return_t tr7_C_raise_single(tr7_engine_t tsc, tr7_t value);
TR7_EXPORT tr7_C_return_t tr7_C_raise_error(tr7_engine_t tsc, const char *utf8msg, tr7_t irritants, int copy);
/*
* The function `tr7_C_raise_single` can be used to raise an exception
* with the given `value`. It is equivalent to SCHEME `(raise value)`.
*
* The function `tr7_C_raise_error` can be used to raise an error
* object whose text is the UTF8 null terminated C string `utf8msg`
* and whose irritants are members of the list `irritants`. It is
* equivalent to the SCHEME `(apply error (cons "utf8msg" irritants))`.
*
* When `copy` is not null, the string is copied and can then be in stack.
* Otherwise, the string is taken as is and never freed.
*
* The code below:
*
* ```C
* tr7_C_return_t raise_x(tr7_engine_t tsc, int nval, tr7_t *values, void *closure)
* {
*    return tr7_C_raise_single(tsc, x);
* }
* ```
*
* is equivalent to:
*
* ```C
* tr7_C_return_t raise_x(tr7_engine_t tsc, int nval, tr7_t *values, void *closure)
* {
*    tr7_C_return_single(tsc, x);
*    return Tr7_C_Return_Raise;
* }
* ```
*
* ### the foreign function callback
*
* C callbacks implementing foreign functions have the the type `tr7_C_func_t`
* defined as:
*/
typedef tr7_C_return_t (*tr7_C_func_t)(tr7_engine_t tsc, int nvalues, const tr7_t *values, void *closure);
/*
* These callbacks are receiving four arguments:
*
* - `tsc`: the TR7 engine
* - `nvalues`: the count of values passed as argument of the invocation
* - `values`: array of the passed values, valid items are `values[0..nvalues-1]`
* - `closure`: a pointer given when the function has been declared
*
* The callback must return either `Tr7_C_Return_Ok` or `Tr7_C_Return_Raise`
* as explained above.
*
* ### creating a callable foreign functions
*
* For creation of foreign function a structure of type `tr7_C_func_def_t`,
* defined here, is required:
*/
typedef struct
{
   const char   *name;     /* name for registering the foreign function */
   tr7_C_func_t  func;     /* callback implementing the foreign function */
   void         *closure;  /* a user data pointer related to the function */
   const char   *typargs;  /* description of expected types */
   int8_t        min_args; /* minimal count of arguments */
   int8_t        max_args; /* maximal count of arguments */
}
   tr7_C_func_def_t;
/*
* The functions for creating one foreign function is:
*/
TR7_EXPORT tr7_t tr7_make_C_func(tr7_engine_t tsc, const tr7_C_func_def_t *funcdef);
/*
* The function `tr7_make_C_func` creates an anonymous TR7 procedure for the
* foreign function described by `funcdef`. It return TR7_FALSE if allocation
* failed. The descriptor `funcdef` must remain accessible during all the
* life of the returned value.
* The field `funcdef->name` is here for functions registering created foreign
* function in an environment but it is not used by `tr7_make_C_func` and, so,
* can be NULL.
*
* The description of foreign functions by instances of `tr7_C_func_def_t`
* has rules. Here they are:
*
* 1. the `name` must be set except for `tr7_make_C_func`.
* 2. the callback `func` must not be NULL.
* 3. `typargs` can be NULL and in that case, any type of argument is accepted.
*    But if `typargs` is not NULL, it must be a string describing the expected
*    types as explained below in chapter "describing type of expected values".
* 4. `min_args` must be greater or equal to zero.
* 5. `max_args` can be positive, null or negative. Its absolute value must
*    be greater or equal to `min_args`.
*
* When `0 <= min_args <= max_args`, the foreign function is called with a count
* of values `nvalues` such that  `min_args <= nvalues <= max_args`.
*
* When `0 <= min_args` and `max_args < -min_args`, the foreign function is
* called with a count of values `nvalues` such that
* `min_args <= nvalues <= -max_args` and if `nvalues == -max_args`,
* the last value is the list of the arguments of the call starting
* at index '-max_args'.
*
* Example: declaring a foreign function of `name` "foo" with `min_args = 2` and
* `max_args = -3` is equivalent to declaring in SCHEME
* `(define (foo a b . c) ...`.
* Then calling `(foo 1 2 3 4)` makes the foreign function receiving 3 values:
* `1`, `2` and `(3 4)`.
*
* ### registering foreign functions
*
* The mainstream use is to register a bloc of C function in an indexable environment.
* To achieve the family of functions that register foreign C functions includes
* the ability to pass array of instances of the type `tr7_C_func_def_t` seen above.
* For all of these functions, the field `name` is required.
*/
TR7_EXPORT tr7_t tr7_lib_register_C_func(tr7_engine_t tsc, const char *libname,
                                         const tr7_C_func_def_t *funcdef);
TR7_EXPORT void  tr7_lib_register_C_func_list(tr7_engine_t tsc, const char *libname,
                                         const tr7_C_func_def_t *fundefs);
TR7_EXPORT void  tr7_lib_register_C_functions(tr7_engine_t tsc, const char *libname,
                                         const tr7_C_func_def_t *fundefs, unsigned count);
TR7_EXPORT tr7_t tr7_register_C_func(tr7_engine_t tsc, const tr7_C_func_def_t *funcdef);
TR7_EXPORT void  tr7_register_C_func_list(tr7_engine_t tsc, const tr7_C_func_def_t *fundefs);
TR7_EXPORT void  tr7_register_C_functions(tr7_engine_t tsc, const tr7_C_func_def_t *fundefs, unsigned count);
/*
* The function `tr7_lib_register_C_func` creates a TR7 procedure for the foreign
* function described by `funcdef` and registers it in the environment of the
* library whose name is given by `libname` in the given TR7 engine.
* It returns TR7_FALSE if allocation failed.
* For this function, the field `funcdef->name` is used and must not be null.
* The descriptor `funcdef` must remain accessible during all the life of the
* returned value.
*
* The libname must follow the internal naming of libraries: the names are
* concatenated using slash separator. For example, for the scheme library
* `(mylib more details)`, the internal name is the string `mylib/more/details`.
*
* CAUTION: the builtins libraries CAN NOT be extended using this behaviour.
* It is an error to try to register a C function in any builtin library:
* TR7_VOID is returned in that case.
*
* The libname can be NULL. In that case, the library that records the C function
* is the library `(tr7 foreigns)` named `tr7/foreigns` as defined by the macro
* `TR7_FOREIGNS_LIBNAME`:
*/
#define TR7_FOREIGNS_LIBNAME "tr7/foreigns"
/*
* The functions `tr7_lib_register_C_func_list` and `tr7_lib_register_C_functions`
* are creating TR7 procedure for the items of the array `fundefs` and registering
* them in the current environment with their given names. The difference is
* that `tr7_lib_register_C_functions` declares `count` functions from the array
* when `tr7_lib_register_C_func_list` declares the functions until it finds
* a description whose field `name` is NULL. Here again, the array must
* remain valid while any of the created value is alive.
*
* The functions `tr7_register_C_func`, `tr7_register_C_func_list` and
* `tr7_register_C_functions` are legacy wrappers for calling the corresponding
* functions `tr7_lib_register_C_func`, `tr7_lib_register_C_func_list` and
* `tr7_lib_register_C_functions` with NULL for `libname`.
*
* ### describing type of expected values
*
* The field `typargs` of the descriptor is used for requesting TR7 to check
* the type of the arguments before transferring control to the foreign function.
* If the check fails, an error is reported and the foreign function is not
* called. Conversely, when the foreign function receives the call, it is
* ensured that the received arguments are of the required type. The intended
* effect is to avoid writing type check in foreign functions.
*
* For example, the foreign function `foo` declared below, expects a string
* and an integer:
*
* ```C
* static tr7_C_func_def_t mydef = {
*    .name = "foo",
*    .func = foo,
*    .closure = NULL,
*    .typargs = TR7ARG_STRING TR7ARG_NATURAL,
*    .min_args = 2,
*    .max_args = 2
* };
*
* tr7_register_C_func(tsc, &mydef);
* ```
*
* The types that can be checked are:
*
* - `TR7ARG_ANY`: Any type
* - `TR7ARG_STRING`: must be a string
* - `TR7ARG_SYMBOL`: must be a symbol
* - `TR7ARG_PORT`: must be a port
* - `TR7ARG_INPORT`: must be an input port
* - `TR7ARG_OUTPORT`: must be an output port
* - `TR7ARG_ENVIRONMENT`: must be an environment
* - `TR7ARG_PAIR`: must be a pair
* - `TR7ARG_ANY_LIST`: must be either a pair or the empty list
* - `TR7ARG_CHAR`: must be a character
* - `TR7ARG_VECTOR`: must be a vector
* - `TR7ARG_NUMBER`: must be a number
* - `TR7ARG_INTEGER`: must be an integer
* - `TR7ARG_NATURAL`: must be a natural integer, i.e. positive or null
* - `TR7ARG_BYTEVEC`: must be a bytevector
* - `TR7ARG_PROC`: must be a procedure
* - `TR7ARG_ERROBJ`: must be an error object
* - `TR7ARG_BYTE`: must be an integer from 0 to 255
* - `TR7ARG_RECORD`: must be a record
* - `TR7ARG_RECORD_DESC`: must be a record descriptor
* - `TR7ARG_PROPER_LIST`: must be a proper list (no cycle and terminated
*                         by the empty list)
*
* When the count of type required  is less than the actual count of
* argument given, the last type is required for the extra arguments.
* By example, for the procedure `+`, giving only `TR7ARG_NUMBER` is enough.
*
* The C mechanism used to implement that behaviour is a bit tricky and then
* requires few words of explanation.
*
* First, it use the C feature that string constants are merged. For example,
* the string `"here I am"` can also be written `"here" " I " "am"`.
*
* So, if `TR7ARG_STRING` was `"s"` and `TR7ARG_NATURAL` was `"n"`, then
* writing `TR7ARG_STRING TR7ARG_NATURAL`, as above, would be understood by the
* C compiler as the string constant `"sn"`.
*
* Having that mechanism is advantageous because:
*
* - it leads to an easy way of specifying the type of arguments
* - the string representation is compact
* - the compiler can optimize memory usage by removing duplication
*
* Second, the single character of the strings for types are given in
* numerical and compact order. To achieves the numeric to string conversion,
* the C preprocessor is used but it enforces to define numeric constants
* using octal notation because 0xxx -> "\0xxx" works as expected.
*
* So here is the definition of types:
* - _TR7ARGNUM_XXX_ is the numerical constants for type XXX (used internally)
* - TR7ARG_XXX is the string constant for type XXX to be used when defining
*   types of foreign function arguments.
*
* Definition of numerical constants for types
*/
#define _TR7ARGNUM_RESERVED_1_  001
#define _TR7ARGNUM_RESERVED_2_  002
#define _TR7ARGNUM_ANY_         003 /* must be first after specials */
#define _TR7ARGNUM_STRING_      004
#define _TR7ARGNUM_SYMBOL_      005
#define _TR7ARGNUM_PORT_        006
#define _TR7ARGNUM_INPORT_      007
#define _TR7ARGNUM_OUTPORT_     010
#define _TR7ARGNUM_ENVIRONMENT_ 011
#define _TR7ARGNUM_PAIR_        012
#define _TR7ARGNUM_ANY_LIST_    013
#define _TR7ARGNUM_CHAR_        014
#define _TR7ARGNUM_VECTOR_      015
#define _TR7ARGNUM_NUMBER_      016
#define _TR7ARGNUM_INTEGER_     017
#define _TR7ARGNUM_NATURAL_     020
#define _TR7ARGNUM_BYTEVEC_     021
#define _TR7ARGNUM_PROC_        022
#define _TR7ARGNUM_ERROBJ_      023
#define _TR7ARGNUM_BYTE_        024
#define _TR7ARGNUM_RECORD_      025
#define _TR7ARGNUM_RECORD_DESC_ 026
#define _TR7ARGNUM_PROPER_LIST_ 027
#define _TR7ARGNUM_BOX_         030
#define _TR7ARGNUM_TXT_INPORT_  031
#define _TR7ARGNUM_TXT_OUTPORT_ 032
#define _TR7ARGNUM_BIN_INPORT_  033
#define _TR7ARGNUM_BIN_OUTPORT_ 034
/*
* conversion of _TR7ARGNUM_XXX_ to string
*/
#define _TR7ARG_AUX_(x) #x
#define _TR7ARG_(x) _TR7ARG_AUX_(\x)
/*
* Definition of string constants for types
*/
#define TR7ARG_ANY             _TR7ARG_(_TR7ARGNUM_ANY_)
#define TR7ARG_STRING          _TR7ARG_(_TR7ARGNUM_STRING_)
#define TR7ARG_SYMBOL          _TR7ARG_(_TR7ARGNUM_SYMBOL_)
#define TR7ARG_PORT            _TR7ARG_(_TR7ARGNUM_PORT_)
#define TR7ARG_INPORT          _TR7ARG_(_TR7ARGNUM_INPORT_)
#define TR7ARG_OUTPORT         _TR7ARG_(_TR7ARGNUM_OUTPORT_)
#define TR7ARG_ENVIRONMENT     _TR7ARG_(_TR7ARGNUM_ENVIRONMENT_)
#define TR7ARG_PAIR            _TR7ARG_(_TR7ARGNUM_PAIR_)
#define TR7ARG_ANY_LIST        _TR7ARG_(_TR7ARGNUM_ANY_LIST_)
#define TR7ARG_CHAR            _TR7ARG_(_TR7ARGNUM_CHAR_)
#define TR7ARG_VECTOR          _TR7ARG_(_TR7ARGNUM_VECTOR_)
#define TR7ARG_NUMBER          _TR7ARG_(_TR7ARGNUM_NUMBER_)
#define TR7ARG_INTEGER         _TR7ARG_(_TR7ARGNUM_INTEGER_)
#define TR7ARG_NATURAL         _TR7ARG_(_TR7ARGNUM_NATURAL_)
#define TR7ARG_BYTEVEC         _TR7ARG_(_TR7ARGNUM_BYTEVEC_)
#define TR7ARG_PROC            _TR7ARG_(_TR7ARGNUM_PROC_)
#define TR7ARG_ERROBJ          _TR7ARG_(_TR7ARGNUM_ERROBJ_)
#define TR7ARG_BYTE            _TR7ARG_(_TR7ARGNUM_BYTE_)
#define TR7ARG_RECORD          _TR7ARG_(_TR7ARGNUM_RECORD_)
#define TR7ARG_RECORD_DESC     _TR7ARG_(_TR7ARGNUM_RECORD_DESC_)
#define TR7ARG_PROPER_LIST     _TR7ARG_(_TR7ARGNUM_PROPER_LIST_)
#define TR7ARG_BOX             _TR7ARG_(_TR7ARGNUM_BOX_)
#define TR7ARG_TXT_INPORT      _TR7ARG_(_TR7ARGNUM_TXT_INPORT_)
#define TR7ARG_TXT_OUTPORT     _TR7ARG_(_TR7ARGNUM_TXT_OUTPORT_)
#define TR7ARG_BIN_INPORT      _TR7ARG_(_TR7ARGNUM_BIN_INPORT_)
#define TR7ARG_BIN_OUTPORT     _TR7ARG_(_TR7ARGNUM_BIN_OUTPORT_)
/*
**************************************************************************
* Utility functions
* -----------------
*
* ### predicates on numbers
*/
TR7_EXPORT int tr7_is_number(tr7_t t);
TR7_EXPORT int tr7_is_real(tr7_t t);
TR7_EXPORT int tr7_is_exact(tr7_t t);
TR7_EXPORT int tr7_is_exact_integer(tr7_t t);
TR7_EXPORT int tr7_is_NaN(tr7_t t);
TR7_EXPORT int tr7_is_finite(tr7_t t);
TR7_EXPORT int tr7_is_infinite(tr7_t t);
/*
* The functions above are the C equivalent functions of the Scheme
* predicates `number?`, `real?`, `exact?`, `exact-integer?`, `nan?`,
* `finite?` and `infinite?`
*
* ### other predicates
*/
TR7_EXPORT int tr7_is_procedure(tr7_t t);
TR7_EXPORT int tr7_is_error(tr7_t item);
TR7_EXPORT int tr7_is_read_error(tr7_t item);
TR7_EXPORT int tr7_is_file_error(tr7_t item);
/*
* The functions above are the C equivalent functions of the Scheme
* predicates `procedure?`, `error-object?`, `read-error?` and `file-error?`.
*
* Most other R7RS predicates are implemented as C macros documented
* above and recalled on that table:
*
*    C macro              | Scheme predicate
*    ----------------------------------------------
*    TR7_IS_BOOLEAN(t)    | (boolean? t)
*    TR7_IS_CHAR(t)       | (char? t)
*    TR7_IS_CHARACTER(t)  | (char? t)
*    TR7_IS_STRING(t)     | (string? t)
*    TR7_IS_SYMBOL(t)     | (symbol? t)
*    TR7_IS_NIL(t)        | (null? t)
*    TR7_IS_PAIR(t)       | (pair? t)
*    TR7_IS_VECTOR(t)     | (vector? t)
*    TR7_IS_BYTEVECTOR(t) | (bytevector? t)
*    TR7_IS_EOF(t)        | (eof-object? t)
*
* Also some other predicates previously explained could be useful in C:
*
*    C macro                  | Utility
*    --------------------------------------------------
*    TR7_IS_VOID(t)           | is TR7_VOID? (used for undefined value)
*    TR7_IS_INT(t)            | is small integer?
*    TR7_IS_DOUBLE(t)         | is IEEE double?
*    TR7_IS_FALSE(t)          | is #false?
*    TR7_IS_TRUE(t)           | is #true?
*
* ### closures
*/
TR7_EXPORT int tr7_is_closure(tr7_t t);
TR7_EXPORT void tr7_closure_set_tag(tr7_t closure, tr7_t value);
TR7_EXPORT tr7_t tr7_closure_get_tag(tr7_t closure);
/*
* Closures are TR7 items handling lambdas and case-lambdas.
* When option USE_TR7_TAGGED_CLOSURES is defined, the closures
* are able to record one tag (a tag is any TR7 value).
* Otherwise, tr7_closure_get_tag always returns TR7_FALSE.
*
* ### convert list to/from vector
*/
TR7_EXPORT tr7_t tr7_list_to_vector(tr7_engine_t tsc, tr7_t list);
TR7_EXPORT tr7_t tr7_vector_to_list(tr7_engine_t tsc, tr7_t vector);










/*
*****************************************************************************
*/
typedef struct tr7_config tr7_config_t;
typedef void *(*tr7_malloc_t)(size_t);
typedef void (*tr7_free_t)(void *);

/*
* - main_dictionary_size count of entries in the main dictionary and environment
* - stack_size_max       max size of stack (0 for unlimited)
* - malloc               malloc compatible allocator
* - free                 free compatible freeer
*/
struct tr7_config
{
   unsigned       main_dictionary_size;
   unsigned       stack_size_max;
   tr7_malloc_t   malloc;
   tr7_free_t     free;
};



/*
* This enumeration is for setting strings used internally.
* This strings are:
*
* - Tr7_StrID_Prompt:          prompt string for default REPL
* - Tr7_StrID_Path:            default path and load path
* - Tr7_StrID_Library_Path:    path for libraries
* - Tr7_StrID_Include_Path:    path for includes
* - Tr7_StrID_Extension_Path:  path for extensions
*
* Except for prompt, all the strings for paths can list many
* directories in the usual way. Search is done in the given
* order as expected.
*/
typedef enum tr7_strid
{
   Tr7_StrID_Prompt,
   Tr7_StrID_Path,
   Tr7_StrID_Library_Path,
   Tr7_StrID_Include_Path,
   Tr7_StrID_Extension_Path,
   __Tr7_StrID_Count__   /* NOT USED EXCEPT FOR COUNTING STRINGS */
}
   tr7_strid_t;


TR7_EXPORT const char  *tr7_get_id(void);
TR7_EXPORT const char  *tr7_get_version(void);

TR7_EXPORT void         tr7_config_init_default(tr7_config_t *config);
TR7_EXPORT tr7_engine_t tr7_engine_create(tr7_config_t *config);
TR7_EXPORT void         tr7_engine_destroy(tr7_engine_t tsc);


TR7_EXPORT void tr7_set_argv(char **argv);
TR7_EXPORT void tr7_set_standard_ports(tr7_engine_t tsc);
TR7_EXPORT void tr7_set_input_port_file(tr7_engine_t tsc, FILE * fin, const char *fname);
TR7_EXPORT void tr7_set_input_port_string(tr7_engine_t tsc, char *start, char *end);
TR7_EXPORT void tr7_set_output_port_file(tr7_engine_t tsc, FILE * fout, const char *fname);
TR7_EXPORT void tr7_set_error_port_file(tr7_engine_t tsc, FILE * ferr, const char *fname);

/*
* This enumeration is for setting options of tr7_play_file
* and tr7_play_string.
*/
typedef enum tr7_play
{
   Tr7_Play_No_Options   = 0,
   Tr7_Play_Show_Prompt  = 1,  /* emit the prompt before reading */
   Tr7_Play_Show_Eval    = 2,  /* show what is to be evaluated */
   Tr7_Play_Show_Result  = 4,  /* show the result of evaluation */
   Tr7_Play_Show_Load    = 8,  /* show what is loaded  */
   Tr7_Play_Fold_Case    = 16, /* should fold the case ? */
   Tr7_Play_Show_Errors  = 32, /* should show errors */
   Tr7_Play_Keep_Playing = 64, /* should continue playing even after
                                  error is trapped */
   Tr7_Play_Interactive  = Tr7_Play_Show_Prompt
                         | Tr7_Play_Show_Result
                         | Tr7_Play_Show_Load
                         | Tr7_Play_Show_Errors
                         | Tr7_Play_Keep_Playing,
   Tr7_Play_Trap_Errors  = Tr7_Play_Show_Errors
                         | Tr7_Play_Keep_Playing,
   Tr7_Play_Show_Compile = 128, /* show the compilation result */
   __Tr7_Play_Count_            /* NOT USED EXCEPT FOR COUNTING */
}
   tr7_play_t;

/*
* play functions are returning 0 on error and 1 on success,
* the behavior of the function, what is shown and how errors are
* treated can be controled by options.
*/
TR7_EXPORT int  tr7_play_file(tr7_engine_t tsc, FILE * fin, const char *filename, tr7_play_t options);
TR7_EXPORT int  tr7_play_string(tr7_engine_t tsc, const char *cmd, tr7_play_t options);

/*
* load functions are returning 0 on error and 1 on success,
* load functions are trapping errors
* when an error is trapped, it is printed and execution continue
* Synonym of play with either Tr7_Play_Interactive if file is stdin
* or Tr7_Play_Trap_Errors otherwise
*/
TR7_EXPORT int  tr7_load_file(tr7_engine_t tsc, FILE * fin, const char *filename);
TR7_EXPORT int  tr7_load_string(tr7_engine_t tsc, const char *cmd);

/*
* run functions are returning 0 on error and 1 on success,
* run functions are not trapping errors
* when an error is raised it returns 0 and the error car be
* retrieved using tr7_get_last_value
* Synonym of play with Tr7_Play_No_Options
*/
TR7_EXPORT int  tr7_run_file(tr7_engine_t tsc, FILE * fin, const char *filename);
TR7_EXPORT int  tr7_run_string(tr7_engine_t tsc, const char *cmd);

/*
* these functions allow to inspect the last returned values.
* tr7_get_values_count returns the count of available values
* tr7_get_value returns the value of index (starting at 0)
* tr7_get_last_value is like tr7_get_value(tsc, 0)
* tr7_get_values allows to get returned values in an array, returns the count of values
* TR7_VOID is returned if some index is invalid
*/
TR7_EXPORT unsigned tr7_get_values_count(tr7_engine_t tsc);
TR7_EXPORT tr7_t    tr7_get_value(tr7_engine_t tsc, unsigned index);
TR7_EXPORT tr7_t    tr7_get_last_value(tr7_engine_t tsc);
TR7_EXPORT unsigned tr7_get_values(tr7_engine_t tsc, unsigned count, tr7_t *values);





/*
* ### Interaction with GC
*/
TR7_EXPORT void tr7_mark(tr7_engine_t tsc, tr7_t value);
TR7_EXPORT void tr7_hold(tr7_engine_t tsc, tr7_t value);
TR7_EXPORT void tr7_unhold(tr7_engine_t tsc, tr7_t value);
/*
* The function `tr7_mark` marks the value. Don't used it at the moment.
*
* The functions `tr7_hold` tells that `value` is hold in C. Consequently,
* the value will not be reclaimed by GC.
*
* The function `tr7_unhold` does the opposite and tells that `value`
* can be reclaimed by GC. It is safe to call this function even if no
* call to `tr7_hold` was made for the value. Also, there is no counting
* on how many time `tr7_hold` was called, the first call to `tr7_unhold`
* unholds the value.
*/


TR7_EXPORT tr7_t tr7_apply0(tr7_engine_t tsc, const char *procname);
TR7_EXPORT tr7_t tr7_call(tr7_engine_t tsc, tr7_t func, tr7_t args);
TR7_EXPORT tr7_t tr7_eval(tr7_engine_t tsc, tr7_t obj);

TR7_EXPORT void tr7_define(tr7_engine_t tsc, tr7_t env, tr7_t symbol, tr7_t value);

/*
* ### reading
*/
TR7_EXPORT tr7_t tr7_read(tr7_engine_t tsc);
TR7_EXPORT tr7_t tr7_from_utf8_length(tr7_engine_t tsc, const char *expr, size_t length);
TR7_EXPORT tr7_t tr7_from_utf8(tr7_engine_t tsc, const char *expr);
/*
* All these functions are returning either the read value or the eof object
* or an error. So it is recommended to process result using below testing:
*
* ```C
* r = tr7_read(tsc);
* if (TR7_IS_EOF(r))
*   ...process end of file
* else if (tr7_is_error(r))
*   ...process read error
* else
*   ...process read value
* ```
*
* The function `tr7_read` read one value from current input port.
*
* The function `tr7_from_utf8_length` read one value from the given string
* `expr` of `length`.
*
* The function `tr7_from_utf8` call `tr7_from_utf8_length` with the length
* of `expr` returned by `strlen`.
*/

/*
* Function to set the 'value' of the string of id 'strid'
*/
TR7_EXPORT void tr7_set_string(tr7_engine_t tsc, tr7_strid_t strid, const char *value);
/*
* printing to output-port
*/
TR7_EXPORT void tr7_write(tr7_engine_t tsc, tr7_t item);
TR7_EXPORT void tr7_write_simple(tr7_engine_t tsc, tr7_t item);
TR7_EXPORT void tr7_write_shared(tr7_engine_t tsc, tr7_t item);
TR7_EXPORT void tr7_display(tr7_engine_t tsc, tr7_t item);
TR7_EXPORT int  tr7_display_string(tr7_engine_t tsc, const char *s);
TR7_EXPORT void tr7_flush(tr7_engine_t tsc);

/*
*/

TR7_EXPORT tr7_t tr7_error_message(tr7_t errobj);
TR7_EXPORT tr7_t tr7_error_irritants(tr7_t errobj);
TR7_EXPORT tr7_t tr7_error_stack(tr7_t errobj);

/*
* Constants for error when getting libraries
* Here defined values are positives
*/
typedef enum {
   Tr7_GetLib_No_Error = 0,         /* no error, must be zero */
   Tr7_GetLib_Error_Invalid_Name,   /* invalid library name */
   Tr7_GetLib_Error_Name_Too_Long,  /* library name too long */
   Tr7_GetLib_Error_Not_Found,      /* no file found for the library */
   Tr7_GetLib_Error_Name_Mismatch,  /* loaded file doesn't declare library */
   Tr7_GetLib_Error_Eval            /* error while loading (eval) the library */
}
   tr7_getlib_error_t;
TR7_EXPORT int tr7_has_lib(tr7_engine_t tsc, const char *name);
TR7_EXPORT int tr7_load_lib(tr7_engine_t tsc, const char *name);
TR7_EXPORT int tr7_import_lib(tr7_engine_t tsc, const char *name);
TR7_EXPORT int tr7_has_lib_length(tr7_engine_t tsc, const char *name, unsigned length);
TR7_EXPORT int tr7_load_lib_length(tr7_engine_t tsc, const char *name, unsigned length);
TR7_EXPORT int tr7_import_lib_length(tr7_engine_t tsc, const char *name, unsigned length);


/*
*****************************************************************************
*****************************************************************************
*****************************************************************************
   candidates for removal from tr7.h below
*****************************************************************************
*****************************************************************************
*****************************************************************************
*/



/*
*****************************************************************************
* Defining comparison results
* ---------------------------
*/
typedef enum tr7_compare
{
   Tr7_Cmp_Unrelated        = 0,
   Tr7_Cmp_Lesser           = 1,
   Tr7_Cmp_Equal            = 2,
   Tr7_Cmp_Lesser_Or_Equal  = 3,
   Tr7_Cmp_Greater          = 4,
   Tr7_Cmp_Different        = 5,
   Tr7_Cmp_Greater_Or_Equal = 6,
   Tr7_Cmp_Any              = 7
}
   tr7_compare_t;

/*
**************************************************************************
*
*/

#ifdef __cplusplus
}
#endif

#endif /* _TR7_H */
/*
Local variables:
c-file-style: "k&r"
End:
vim: noai ts=3 sw=3 expandtab
*/
