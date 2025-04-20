====
kcpp
====

A preprocessor for C23 and C++23 writen in Python, implemented as a library.

  :Licence: MIT
  :Language: Python (>= 3.10)
  :Author: Neil Booth


Getting started
===============

Put this in ``/tmp/foo.cpp``::

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


Goals
=====

This project is a standards-conforming and efficient (to the extent possible in Python)
preprocessor that provides high quality diagnostics.  It is host and target independent
(in the compiler sense).  The code is intended to be clean and easy to understand.

Perhaps more importantly, it should be a reference implementation that can be easily
transcoded to an quivalent but more efficient C or C++ implementation by a decent
programmer of those languages.  There is no reason such a re-implementation should not be
a par or even exceed Clang or GCC with respect to performance and quality, and at the same
time I would expect it to be significantly smaller and easier to understand and maintain.

Some design choices (such as treating source files as binary rather than as Python Unicode
strings, and not using Python's built-in Unicode support) were made because those features
don't exist in C and C++.  I want it to be easy to translate this Python implementation to
a C or C++ equivalent.

I intend to do such a transcoding to C++ once the Python code is mostly complete and
cleaned up later in 2025 as part of my goal of learning C++ properly.


Why write a preprocessor in Python?
===================================

Good question.  Essentially because Python makes it very easy to refactor code to find the
cleanest and most efficient implementation of an idea.  It is ideal for a reference
implementation that can be transcoded to C or C++.  I believe the result would be much
better than could be achieved from scratch in a similar timeframe in those languages alone.

I was a co-maintainer of GCC's preprocessor from 1999 to 2003.  During this time we
converted it from a standalone executable that would output to a pipe, to an integrated
(kind-of) libary ``libcpp`` in the compiler proper.  Compilers are addictive, and between
2005 and 2007 I wrote a C99 front-end in C (which is not public).  LLVM was lacking an
implementation of compile-time host- and target-independent IEEE-conforming floating point
arithmetic, so I contributed the one from my front-end (after translating it from C to
C++).  Chris Lattner incorporated it into Clang/LLVM as APFloat.cpp in 2007.

My experience writing a front-end made clear the difficulty of refactoring and
restructuring C or C++ code to make improvements.  Another reason it is avoided is fear of
breaking things subtly owing to poor testsuite coverage, or having to update hundreds or
thousands of tests to account for changes in output or diagnostics that a refactoring
tends to cause.  Can compiler testing be improved?

A glance at, e.g., the expression parsing and evalation code of GCC and Clang, or their
diagnostic subsystems, and trying to comprehend them reveals creeping complexity and loss
of clarity.  I remember Clang's original preprocessor from 2007 as being quite clean and
efficient; I'm not sure that could ever have been said of libcpp that I worked on.

In 2012 I learnt Python and have come to love its simplicity and elegance.  In 2016 with
ElectrumX I proved Python can efficiently process challenging workloads.  More recently I
have become interested in learning C++ properly - although able to write basic C++ from
around the mid 1990s, I used to prefer the simplicity of C.

Recently I noticed C++ was "getting its act together" and took a look at C++ standard
drafts.  I became curious and decided "Hmm, let's try something a little insane and write
a C++23 preprocessor in Python."  So kcpp was born in mid-January 2025.

Can a performant and standards-conforming preprocessor be written in Python?


What about the other open source preprocessors?
===============================================

There are several publicly available preprocessors typically written in C or C++.  Usually
they claim to be standards conforming but are usually far from it.  It is significant work
to be 90% conforming, but the last 10% is really quite hard.  There are endless corner
cases, and more recent preprocessor features like extended identifiers, raw strings,
variable-argument macros (particularly the addition of ``__VA_OPT__``), and handling UCNs
are each significant work.  None of the preprocessors I'm aware of (other than those of
the much larger projects GCC and Clang) make an effort at high-quality diagnostics or
serious standards compliance.

To my surprise two or three Python preprocessors exist as well, but have similar defects
and/or have other goals such as visualization (``cpip`` is a cool example of this).  None
appear to be actively maintained.

It is worthwhile comparing the code of other preprocessors with that of ``kcpp`` and
testing them with tricky preprocessing cases.


Features
========

As a **genuinely** conforming C++23 preprocessor ``kcpp`` is essentially complete.
Specifically, the following are implemented:

- lexing (including UCNs, extended identifiers, and raw string literals)
- macro expansion, including variable arguments, ``__VA_OPT__``, and whitespace-correctness
- all standard directives
- ``_Pragma`` operator
- predefined and built-in macros, presently limited to those defined in the standard
- interpretation of character, string and numeric literals
- expression parsing with proper error recovery
- expression evaluation
- ``__has_include``, ``__has_cpp_attribute`` preprocessor conditional operators
- conversion of Unicode character names (those in ``\N{}`` escapes) to codepoints.  I
  implemented it based on the ideas described by **cor3ntin** at
  https://cor3ntin.github.io/posts/cp_to_name/.  I added some ideas and improvements of my
  own to achieve another 20% compaction - see
  https://github.com/kyuupichan/kcpp/blob/master/src/kcpp/unicode/cp_name_db.py.
- module-related directives with import-keyword, export-keyword, module-keyword

In addition the following are complete:

- preprocessed output
- a full diagnostic framework.  This includes changing diagnostic severities from the
  command line, Colourized output to a Unicode terminal, and translations (none
  provided!).  The framework could be hooked up to an IDE.
- diagnostics can display the macro expansion stack with precise caret locations and range
  highlights, with proper handling of multibyte characters, tabstops and CJK terminal
  character widths

For C23 preprocessing, C++-specific features are disabled (alternative operators, raw
string literals, ``<=>``, ``.*`` and ``->*`` tokens, ``__has_cpp_attribute`` and
user-defined suffixes).  The following C23 features are missing simply because I
implemented C++23 first.  They are all easy, apart from ``#embed`` which is quite a new
concept (and in C++26) which will need some thought on its implementation:

  - bit-precise integer suffixes
  - decimal floating-point suffixes
  - slightly different charcter literal semantics
  - different predefined macros
  - ``__has_c_attribute``
  - ``#embed``, ``__has_embed``, etc.

Future
======

- the multiple-include optimization is not yet implemented
- some GCC and Clang extensions should be supported
- features like ``Makefile`` output are worth considering going forwards.
- pprecompiled headers are possibly an idea.  An implementation would probably share a lot
  with modules.  Python is a good place to experiment before attempting an implementation
  in C++, but there is little point doing this until a compiler frontend exists
- add a C and C++ front-end in Python, perhaps as a single parser and codebase?

It should be easy to extend the code to provide hooks for analysis or other tools needing
a preprocessor to do grunt work.


Documentation
=============

I will write some at some point.  The code is well-commented and reasonably clean though -
it shouldn't be hard to figure out.


Tests
=====

I have a testuite but it is mostly private.  Test case submissions for the public repo
(using pytest) are welcome, as are bug reports.
