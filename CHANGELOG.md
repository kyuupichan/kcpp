# 0.6 2025-04-14

- add `#undef` preprocessor action (for `--list-macros`)
- many diagnostic improvements including line-wrapping, source-code windowing
- don't show UTF-8 BOM in diagnostics
- improve diagnostics relating to preprocessor conditional directives
- new command-line options: `--error-limit`, `--diag-suppress`, `--diag-remark`, `--diag-warning`, `--diag-error`, `--diag-once`, `--remarks`, `--warnings`, `--errors`, `--strict`, `--strict-warnings`, `--columns`, `--terminal-width`
- better recovery from some syntax errors and unterminated raw strings
- implement module-related directives

# 0.5 2025-04-07

- `_Pragma`
- `#pragma`
- `__has_include()`
- `__has_cpp_attribute()`
- several bugs fixed.

# 0.4.1 2025-04-02

- change directory layout so I don't have to fight setuptools

# 0.4 2025-04-02

- `#include`
- skinning
- preprocessed output

# 0.3 2025-03-28

- Macro expansion imlementation complete
- `#line`

# 0.2 2025-03-23

- object-like macro expansion
- diagnostics with a macro stack

# 0.1 2025-03-16

- initial release; quite incomplete but progress from here should be rapid
