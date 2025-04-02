====
kcpp
====

A preprocessor for C++23 writen in Python, implemented as a library.

  :Licence: MIT
  :Language: Python (>= 3.10)
  :Author: Neil Booth


Why write a preprocessor in Python?
===================================

Good question.  Essentially because Python makes it very easy to refactor code to find the
cleanest and most efficient implementation of an idea.  It is ideal for a reference
implementation that can be transcoded to C or C++.  I believe the result would be much
better than could be achieved from scratch in a similar timeframe in those languages alone.

I was a co-maintainer of GCC's preprocessor 1999 to 2003.  During this time the
preprocessor was converted from a standalone executable that would write its output to a
pipe, to be an integrated "libary" (libcpp) into the compiler proper.  Furthermore, around
2005-2007 I wrote a complete C99 front-end in C (not public), and I rewrote its
implementation of a generic target floating point emulator using bignum integer arithmetic
from C to C++ and contributed to the Clang project, which needed such an emulator, as
APFloat.cpp in around 2007.  From writing a C front-end, it became very clear how hard it
is to refactor and restructure C or C++ code to do things more simply or in better ways,
which generally means it is not done.  Another reason refactoring is avoided is fear of
breaking things subtly owing to poor testsuite coverage, or alternatively having to
manually update hundreds or thousands of tests to account for changes in output that a
refactoring might cause.

A quick glance at the diagnostic and expression parsing and evalation subsystems of GCC
and Clang today, and trying to understand them, shows creeping complexity and loss of
clarity.  Clang's original preprocessor was fairly clean and efficient - something that
was only partly true of GCC's libcpp at one point.

I learnt Python in 2012, and since that time have come to love its simplicity and
elegance.  In 2016 with ElectrumX that I showed that Python can provide efficient
processing of heavy workloads.  More recently I have become interested in learning C++
properly (I used to be able to write basic C++ from around the mid 1990s, but at the time
I much preferred C, and I have never been good at idiomatic C++ as I've never worked on a
large and decent C++ codebase).  I took a look at drafts of the most recent C++ standard
and decided "Aha! Let's try something insane and attempt a C++23 preprocessor in Python."
I started this crazy project at around the end of January 2025.

Can a performant and standards-conforming preprocessor be written in Python?


What about the other open source C preprocessors?
=================================================

There are several publicly available preprocessors, usually written in C or C++, and most
claim to be standards conforming (but are not).  It is fairly easy to be "almost"
conforming, but the last 10-20% is very hard.  This is particularly true when it comes to
endless corner cases, or more recent features like processing generic UTF-8 source,
extended identifiers, __VA_OPT__, and handling UCNs.  None make an effort at high-quality
diagnostics and their codebases do not appeal as something to build on.

To my surprise two or three Python preprocessors exist as well, but have similar defects
to their C and C++ counterparts and/or have other goals such as visualization (cpip is a
cool example of this).  They don't appear to be actively maintained.

I invite you to compare the code of other preprocessors with that of kcpp.


Goals
=====

This project shall develop a standards-conforming and efficient (to the extent possible in
Python) preprocessor that provides high quality diagnostics that is host and target
independent (in the compiler sense).  The code should be clean and easy to follow.

Perhaps more importantly, it should be a "reference implementation" that can be easily
transcoded to a clean and efficient C or C++ implementation by a decent programmer of
those languages.  There is no reason such an implementation should be on at least on a par
with Clang or GCC for performance and quality, and at the same time a lot smaller and
easier to understand.

Some design choices I have made (such as treating source files as binary rather than as
Python Unicode strings, and not using Python's built-in Unicode support) are because those
features don't exist in C and C++.  I want it to be fairly easy to translate the Python
code directly translate to C or C++ equivalents.

I probably will do such a transcoding to C++ once the Python code is mostly complete and
cleaned up.  This will be later in 2025 as part of my goal of learning C++ properly.


Features that are essentially complete
======================================

The following are bascially done and fully working, modulo cleanups and improvements, to
the C++23 specifications:

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

C++ modules - I've not fully figured out how these work in C++ or how they interact with
the preprocessor.  Unlikely to be tackled until some kind of real frontend exists.

Precompiled headers - possibly an idea and I suspect largely overlaps with modules.
Again, Python is a good place to experiment before attempting an implementation in C++.

Makefile output and other features are possibilities going forwards.


Future
======

It should be easy to extend the code to provide hooks for other code or analysis tools
needing a preprocessor back-end.  A logical next step is to become a front-end in Python
too.

Feature requests are welcome.


Documentation
=============

Soon.  The code is well-commented and reasonably clean though - it shouldn't be hard to
figure out.


Tests
=====

I have a testuite for the code but it is private.  Test case submissions for the public
repo (using pytest) are welcome.

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
