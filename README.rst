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
cleanest and most efficient implementation of an idea.

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
refactoring might cause.  A quick glance at the preprocessor or diagnostic subsystems of
GCC and Clang today, and trying to understand them, shows creeping complexity and loss of
clarity - Clang's original preprocessor (written by Chris Lattner) was fairly clean and
efficient - something that couldn't really be said of GCC's libcpp - but those days are
long gone.

I learnt Python in 2012, and since that time have come to love its simplicity and
elegance.  In 2016 I demonstrated with ElectrumX that, with care, Python can provide
efficient implementations of heavy workloads.  More recently I have become interested in
learning C++ properly (I used to be able to write basic C++ from around the mid 1990s, but
at the time I much preferred C, and I have never been good at idiomatic C++ as I've never
worked on a good C++ codebase).  I took a look at drafts of the most recent C++ standard
and decided "Aha! Let's try something insane and attempt a C++23 preprocessor in Python."
I started this crazy project at around the end of February 2025.

So can a performant and standards-conforming preprocessor be written in Python?  Judge for
yourself.


What about the other open source C preprocessors?
=================================================

There are several publicly available preprocessors, usually written in C or C++, and most
claim to be standards conforming, but in reality are far from it, particularly when it
comes to corner cases or more recent features like processing generic UTF-8 source and
handling the details of UCNs.  To my surprise three or four Python preprocessors exist as
well, but from what I could see don't take efficiency or standards conformance seriously,
and had other goals such as visualization (cpip is a cool example of this).


Goals
=====

I want this project to be a standards-conforming and efficient (to the extent possible in
Python) preprocessor that provides extremely high quality diagnostics and is retargetable
(in the compiler sense).  The code should be clean and easy to understand - good examples
to compare with other projects are kcpp's diagnostics subsystem, and expression parser and
evaluator.

Equally, it should be seen as a "reference implementation" that can be easily transcoded
to a clean and efficient C or C++ implementation by any reasonable programmer of those
languages.  Such an implementation would, IMO, be at least on a par with the code of Clang
or GCC and much easier to understand.

Many of the design choices I have made (such as treating source files as binary rather
than as Python Unicode strings, and not using Python's built-in Unicode support) are
because those things don't exist in C and C++, so a simple transcoding would not be
possible.  The Python code should be fairly easy to directly translate to C or C++
equivalents.

I intend do said transcoding, probably to C++, later in 2025 as part of my goal of
learning C++ properly.


Features that are essentially complete
======================================

The following are bascially done and fully working, modulo small cleanups and
improvements:

- lexing
- interpretation of literals
- expression parsing
- expression evaluation
- conversion of Unicode character names to codepoints (based on the ideas described by
  cor3ntin at https://cor3ntin.github.io/posts/cp_to_name/, but I implemented some ideas
  of my own to achieve even tighter compaction; see unicode/cp_name_db.py)
- the basic diagnostic framework.  Colourized output to a Unicode terminal is supported,
  as are translations (none provided!).  The framework could be hooked up to an IDE.


Incomplete or Missing
=====================

The following are missing, or work in progress, but the framework is already in place so
that adding them is not too much of a stretch:

- macro expansion (my current focus)
- some directives, particularly #line, include, #pragma
- preprocessed output (partially done in a trivial way)
- _Pragma operator
- multiple-include optimisation
- _has_include
- _has_cpp_attribute
- make the driver into a library

C++ modules - I've not yet figured out how these work in C++ or how they interact with the
preprocessor.

Precompiled headers - possibly an idea.  Again, Python is a good place to experiment
before attempting an implementation in C++.

Makefile output, preprocessor actions or hooks, and other features, are possibilities
going forwards.


Future
======

It should be easy to extend the code to provide hooks for other code or analysis tools
needing a preprocessor back-end.

Indeed a logical future continuation is to write a front-end in Python too.

Feature requests are welcome.


Documentation
=============

One day.  The code is well-commented and reasonably clean though - it shouldn't be hard to
figure out.


Tests
=====

I have fairly comprehensive tests for the code, but for various reasons I am keeping the
testsuite private.

Bug reports (for those areas in the "Features that are essentially complete" section
above) are most welcome.


ChangeLog
=========

0.0.1  2025-03-16

Initial release.  Quite incomplete but progress from here should be rapid.
