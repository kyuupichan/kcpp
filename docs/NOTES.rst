Diagnostic Control
------------------

All diagnostics that are not errors belong to a group.  The severity of those diagnostics
can be modified by ``--diag-suppress``, ``--diag-remark``, ``--diag-warning``,
``--diag-error`` and ``--diag-once``.

Warnings and remarks in general can be controlled by ``--remarks``, ``--no-remarks``,
``--warnings`` and ``--no-warnings``.


Strict modes
------------

What should ``--strict-warnings`` and ``--strict`` mean, including in the context of
non-standard features being enabled with ``--feature``.  For example, if compiling in C89
mode but enabling line comments with ``--feature=line-comments``, it would be annoying to
have line comments rejected or diagnosed.

So this is the approach taken by ``kcpp``.  In either strict mode:

  - all non-standard features are disabled.  This happens before ``--feature`` commands
    are processed, so the user can still enable non-standard features.  Features that are
    explicitly enabled in this way do not generate diagnostics.
  - for C++ all diagnosable rules [4.1.1 intro.compliance.general], and all conditionally
    supported constructs that are used but not supported, are diagnosed.
  - for C, all constraint violations are diagnosed.
  - for both C and C++, where reasonably detectable, all undefined and unspecified
    behaviour is diagnosed.
  - "diagnosed" above means an error with ``--strict``, or a warning with
    ``--strict-warnings``.  These switches only upgrade the severity of diagnostics, the
    severity is never lowered.


Language features
-----------------

C++23 introduced, amongst other things, ``#elifdef``, ``#elifndef`` and ``#warning``
directives.  What if these directives are met when compiling for an earlier standard?
Their acceptance cannot change the meaning of an otherwise conforming program, so it seems
least annoying to accept and process them with their C++23 meaning without even a warning.
However, users who want to know if constructs have crept into their codebase that may not
be accepted by all compilers compiling to a given standard can use ``--strict`` or
``--strict-warnings``, and in either case such directives will be rejected with an error
as the additional directives are not then part of the default feature set.

C++23 also introduced the ``z`` suffix for integer literals indicated the signed version
of the ``size_t`` type.  This case is different, as ``0z`` or ``0uz`` could be a
user-defined suffix from C++11.  C++11 and C++14 required such suffixes to begin with an
``_``, no diagnostic required, but in C++17 that requirement was dropped.  In either case,
accepting the ``z`` suffix unconditionally for versions of C++ prior to C++23 could change
the meaning of a conforming program, so it should not be accepted without an explicit
``--feature=suffix-z`` command-line request.


Parser recovery from syntax errors
----------------------------------

In a parser (e.g. expressions.py), every time a syntax error is encountered and an
"expected XYZ" diagnostic of some kind is issued, the parser should be made to recover
with a call to state.recover().  This generally gives the best results - its goals are to
recover as early as possible so that other constructs can be diagnosed, but to recover
reliably so that cascading syntax errors are avoided.
