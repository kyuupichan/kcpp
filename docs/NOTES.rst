Strict modes
------------

It has to be decided what ``--strict-warnings`` and ``--strict`` mean, including in the
context of non-standard features being enabled with ``--feature``.  For example, if
compiling in C89 mode but enabling line comments with ``--feature=line-comments``, it
would be annoying to have line comments rejected or diagnosed.

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


Parser recovery from syntax errors
----------------------------------

In a parser (e.g. expressions.py), every time a syntax error is encountered and an
"expected XYZ" diagnostic of some kind is issued, the parser should be made to recover
with a call to state.recover().  This generally gives the best results - its goals are to
recover as early as possible so that other constructs can be diagnosed, but to recover
reliably so that cascading syntax errors are avoided.
