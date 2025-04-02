====
kcpp
====

A preprocessor for C++23 writen in Python, implemented as a library.

  :Licence: MIT
  :Language: Python (>= 3.10)
  :Author: Neil Booth


Getting started
===============

Put this in `/tmp/foo.cpp`::

  #define div 1 / 0
  #define g(x) 2 + x
  #if g(div)
  #endif

Then::

  $ pip install kcpp
  $ kcpp /tmp/foo.cpp
  #line 1 "/tmp/foo.cpp"
  #line 1 "<predefines>"
  #line 1 "/tmp/foo.cpp"
  "/tmp/foo.cpp", line 3: error: division by zero
      3 | #if g(div)
        |       ^~~
      "/tmp/foo.cpp", line 1: note: in expansion of macro 'div'
          1 | #define div 1 / 0
            |               ^ ~
      "/tmp/foo.cpp", line 2: note: in expansion of macro 'g'
          2 | #define g(x) 2 + x
            |                  ^
  1 error generated compiling "/tmp/foo.cpp".


Why write a preprocessor in Python?
===================================

Good question.  Essentially because Python makes it very easy to refactor code to find the
cleanest and most efficient implementation of an idea.  It is ideal for a reference
implementation that can be transcoded to C or C++.  I believe the result would be much
better than could be achieved from scratch in a similar timeframe in those languages alone.

I was a co-maintainer of GCC's preprocessor 1999 to 2003.  During this time the
preprocessor was converted from a standalone executable that would write its output to a
pipe, to be an integrated "libary" (libcpp) into the compiler proper.  Compilers are
addictive, and sometime around 2005-2007 I wrote a complete C99 front-end in C (which is
not public), and its implementation of a host- and target-independent IEEE-conforming
floating point emulator using bignum integer arithmetic, which I translated from C to C++,
was incorporated into LLVM, which needed such an emulator for Clang, as APFloat.cpp
in 2007.  My experience made it very clear how hard it is to refactor and restructure C or
C++ code to do things more simply or in better ways.  Another reason refactoring is
avoided is fear of breaking things subtly owing to poor testsuite coverage, or having to
update hundreds or thousands of tests to account for changes in output or diagnostics that
a refactoring tends to cause.

A glance at the expression parsing and evalation, and diagnostic, subsystems of GCC and
Clang, and trying to understand them, shows creeping complexity and loss of clarity.
Clang's original preprocessor was quite clean and efficient; I'm not sure that could ever
have been said of libcpp that I worked on.

In 2012 I learnt Python and have come to love its simplicity and elegance.  In 2016 with
ElectrumX I proved Python can efficiently process challenging workloads.  More recently I
have become interested in learning C++ properly - although able to write basic C++ from
around the mid 1990s, I used to prefer the simplicity of C.

Recently I noticed C++ was "getting its act together" and took a look at C++ standard
drafts.  I became curious and decided "Hmm, let's try something a little insane and write
a C++23 preprocessor in Python."  So kcpp was born in mid-January 2025.

Can a performant and standards-conforming preprocessor be written in Python?


What about the other open source C preprocessors?
=================================================

There are several publicly available preprocessors, usually written in C or C++, and most
claim to be standards conforming, but are only superficially so.  It is indeed quite some
work to be almost conforming, but the last 10-20% is very hard, particularly considering
endless corner cases, or more recent features like processing UTF-8 source with extended
identifiers, raw strings, the addition of __VA_OPT__ feature, and handling UCNs.  None of
the preprocessors I'm aware of (other than those of the much larger projects GCC and
Clang) make an effort at high-quality diagnostics or serious standards compliance, and
their codebases do not appeal as something to build on.

To my surprise two or three Python preprocessors exist as well, but have similar defects
to their C and C++ counterparts and/or have other goals such as visualization (cpip is a
cool example of this).  They don't appear to be actively maintained.

I invite you to compare the code of other preprocessors with that of kcpp.


Goals
=====

This project shall develop a standards-conforming and efficient (to the extent possible in
Python) preprocessor that provides high quality diagnostics that is host and target
independent (in the compiler sense).  The code should be clean and easy to understand.

Perhaps more importantly, it should be a reference implementation that can be easily
transcoded to an efficient C or C++ implementation by a decent programmer of those
languages.  I believe there is no reason such a re-implementation should not be a par or
better than Clang or GCC with respect to performance and quality, and at the same time
significantly smaller and easier to understand.

I have made some design choices (such as treating source files as binary rather than as
Python Unicode strings, and not using Python's built-in Unicode support) are because those
features don't exist in C and C++.  It should be fairly easy to translate this Python code
to C or C++ equivalents.

I intend to do such a transcoding to C++ once the Python code is mostly complete and
cleaned up later in 2025 as part of my goal of learning C++ properly.


Features that are essentially complete
======================================

The following features are bascially complete to the C++23 specifications, modulo silly
bugs and cleanups:

- lexing
- macro expansion, including __VA_OPT__ and whitespace correctness
- predefined and built-in macros
- interpretation of literals
- expression parsing
- expression evaluation
- conversion of Unicode character names to codepoints.  I implemented the Python code
  based on the ideas described by cor3ntin at
  https://cor3ntin.github.io/posts/cp_to_name/, but added some ideas and improvements of
  my own to achieve 20+% tighter compaction - see unicode/cp_name_db.py.
- preprocessed output
- display of the macro expansion stack in diagnostics with precise caret locations and
  range highlights
- the diagnostic framework.  Colourized output to a Unicode terminal is supported,
  as are translations (none provided!).  The framework could be hooked up to an IDE.


Incomplete or Missing
=====================

The following are missing.  The framework is already in place so that adding them is
almost trivial, and only held-up by refactoring and testing:

- #pragma
- _Pragma operator
- multiple-include optimisation
- _has_include
- _has_cpp_attribute

The following are more serious projects:

- C++ modules - I've not fully figured out how these work in C++ or how they interact with
the preprocessor.  So unlikely to be tackled until some kind of real frontend exists.
- precompiled headers - possibly an idea and I suspect largely overlaps with modules.
Again, Python is a good place to experiment before attempting an implementation in C++.


Future
======

Features like ``Makefile`` output are worth considering going forwards.

A logical next step is to become a front-end in Python.

It should be easy to extend the code to provide hooks for analysis or other tools needing
a preprocessor back-end.

Feature requests are welcome.


Documentation
=============

Soon.  The code is well-commented and reasonably clean though - it shouldn't be hard to
figure out.


Tests
=====

My testuite for the code is mostly private.  Test case submissions for the public repo
(using pytest) are welcome.

Bug reports are also welcome.


ChangeLog
=========

0.1  2025-03-16

Initial release.  Quite incomplete but progress from here should be rapid.

0.2  2025-03-23

Object-like macro expansion, and diagnostics with a macro stack, are implemented.

0.3  2025-03-28

Macro expansion imlementation complete.  #line implemented.

0.4 2025-04-02

#include implemented.  Skinning.  Preprocessed output done.

0.4.1 2025-04-02

Change directory layout so I don't have to fight setuptools.
