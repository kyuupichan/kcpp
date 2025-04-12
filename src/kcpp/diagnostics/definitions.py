#
# This file is generated by gen-diags - do not edit!
#

from dataclasses import dataclass
from enum import IntEnum, auto


__all__ = [
    'DID', 'DiagnosticGroup', 'DiagnosticSeverity', 'DiagnosticDefinition',
    'diagnostic_definitions',
]


class DiagnosticSeverity(IntEnum):
    '''The severity of a diagnostic.'''
    none = auto()        # Source file locations, compilation summaries, etc.
    note = auto()        # Notes are emitted nested inside another diagnostic
    ignored = auto()     # Indicates this diagnostic is suppressed and to be ignored
    remark = auto()      # A diagnostic milder than a warning; suppressed by default
    warning = auto()     # Does not increase the error count
    error = auto()       # Increases the error count, halts compilation if limit reached
    fatal = auto()       # A fatal error terminates compilation immediately


class DiagnosticGroup(IntEnum):
    '''All diagnostics whose severity can be controlled by the user must belong to a group.
    Therefore all ignored, warning and remark diagnostics have an associated group.  Errors
    that belong to a group can have their severity downgraded.'''
    none = auto()
    # Strict groups:
    comma_expr = auto()
    defined = auto()
    directive_ws = auto()
    escape_sequence = auto()
    extra_tokens = auto()
    invalid_pp_token = auto()
    line_number = auto()
    macro_redefined = auto()
    macro_ws = auto()
    overflow = auto()
    predefined = auto()
    raw_delimiter = auto()
    shift_count = auto()
    strict_start = comma_expr
    strict_end = shift_count
    # Not strict groups:
    multichar = auto()
    shift_of_negative = auto()
    sign_change = auto()
    undefined = auto()
    unicode = auto()
    warning_directive = auto()


class DID(IntEnum):
    '''The diagnostic identifier.'''
    at_file_and_end_of_line = auto()
    at_file_and_line = auto()
    at_file_end = auto()
    at_file_line_and_column = auto()
    bad_source_date_epoch = auto()
    brief_at_file_and_line = auto()
    brief_at_file_line_and_column = auto()
    builtin_macro_only_if_elif = auto()
    builtin_macro_redefined = auto()
    cannot_be_defined = auto()
    cannot_read_file = auto()
    cannot_write_file = auto()
    character_does_not_exist = auto()
    character_not_single_code_unit = auto()
    codepoint_basic_character_set = auto()
    codepoint_cannot_begin_identifier = auto()
    codepoint_cannot_continue_identifier = auto()
    codepoint_control_character = auto()
    codepoint_invalid = auto()
    codepoint_surrogate = auto()
    comma_in_pp_expression = auto()
    compilation_halted = auto()
    delimeter_invalid_character = auto()
    delimeter_too_long = auto()
    division_by_zero = auto()
    duplicate_macro_parameter = auto()
    else_after_else = auto()
    else_location = auto()
    else_without_if = auto()
    empty_character_literal = auto()
    endif_without_if = auto()
    error_directive = auto()
    error_limit_reached = auto()
    error_summary = auto()
    escape_sequence_value_too_large = auto()
    expected_close_brace = auto()
    expected_close_paren = auto()
    expected_colon = auto()
    expected_comma_in_parameter_list = auto()
    expected_expression = auto()
    expected_header_name = auto()
    expected_identifier = auto()
    expected_macro_name = auto()
    expected_macro_parameter = auto()
    expected_open_brace = auto()
    expected_open_paren = auto()
    expected_string_literal = auto()
    extra_directive_tokens = auto()
    fatal_error_and_error_summary = auto()
    fatal_error_summary = auto()
    filename_should_be_string = auto()
    floating_point_in_pp_expr = auto()
    from_Pragma = auto()
    from_formation_of_header_name = auto()
    function_like_macro_not_defined = auto()
    hash_requires_macro_parameter = auto()
    header_file_not_found = auto()
    hexadecimal_exponent_required = auto()
    identifier_not_NFC = auto()
    in_argument_stringizing = auto()
    in_expansion_of_builtin = auto()
    in_expansion_of_macro = auto()
    in_token_concatenation = auto()
    incomplete_UCN_as_tokens = auto()
    integer_overflow = auto()
    integer_too_large = auto()
    invalid_charset = auto()
    invalid_digit = auto()
    invalid_directive = auto()
    invalid_in_filename = auto()
    invalid_numeric_suffix = auto()
    invalid_op_in_pp_expression = auto()
    invalid_variadic_identifier_use = auto()
    left_shift_overflows = auto()
    line_number_must_be_digit_sequence = auto()
    line_number_out_of_range = auto()
    macro_defined_here = auto()
    macro_definition_ends_with_concat = auto()
    macro_definition_starts_with_concat = auto()
    macro_name_not_identifier = auto()
    macro_name_whitespace = auto()
    macro_produced_defined = auto()
    macro_redefined = auto()
    max_include_depth_reached = auto()
    missing_digit_sequence = auto()
    multicharacter_literal = auto()
    multicharacter_literal_truncated = auto()
    multicharacter_literal_with_prefix = auto()
    nested_va_opt = auto()
    predefined_macro_redefined = auto()
    prior_macro_definition = auto()
    prior_match = auto()
    severity_error = auto()
    severity_fatal = auto()
    severity_note = auto()
    severity_remark = auto()
    severity_warning = auto()
    shift_count_negative = auto()
    shift_count_too_large = auto()
    shift_of_negative_value = auto()
    starting_compilation = auto()
    string_concatenation_conflict = auto()
    string_concatenation_prior = auto()
    string_invalid_in_pp_expression = auto()
    stringize_failed = auto()
    token_concatenation_failed = auto()
    too_few_macro_arguments = auto()
    too_many_macro_arguments = auto()
    unclosed_if_block = auto()
    undefined_identifier = auto()
    unknown_charset = auto()
    unknown_diagnostic_group = auto()
    unknown_target = auto()
    unrecognized_escape_sequence = auto()
    unrecognized_universal_character_name = auto()
    unterminated_argument_list = auto()
    unterminated_block_comment = auto()
    unterminated_literal = auto()
    user_defined_suffix_in_pp_expr = auto()
    utf8_invalid = auto()
    utf8_overlong = auto()
    utf8_surrogate = auto()
    va_opt_ends_with_concat = auto()
    va_opt_starts_with_concat = auto()
    value_changes_sign = auto()
    vertical_whitespace_in_directive = auto()
    warning_directive = auto()

    def __repr__(self):
        return f'DID.{self.name}'


@dataclass
class DiagnosticDefinition:
    '''Defines a diagnostic; sourced from a .yaml file.'''
    did: DID
    severity: DiagnosticSeverity
    group: DiagnosticGroup
    text: str


diagnostic_definitions = {
    DID.at_file_and_end_of_line: DiagnosticDefinition(
        DID.at_file_and_end_of_line,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%0, at end of line %1',
    ),
    DID.at_file_and_line: DiagnosticDefinition(
        DID.at_file_and_line,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%0, line %1',
    ),
    DID.at_file_end: DiagnosticDefinition(
        DID.at_file_end,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%0, at end of source',
    ),
    DID.at_file_line_and_column: DiagnosticDefinition(
        DID.at_file_line_and_column,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%0, line %1, column %2',
    ),
    DID.bad_source_date_epoch: DiagnosticDefinition(
        DID.bad_source_date_epoch,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'source date epoch must be an integer between 0 and %0',
    ),
    DID.brief_at_file_and_line: DiagnosticDefinition(
        DID.brief_at_file_and_line,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%0:%1',
    ),
    DID.brief_at_file_line_and_column: DiagnosticDefinition(
        DID.brief_at_file_line_and_column,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%0:%1:%2',
    ),
    DID.builtin_macro_only_if_elif: DiagnosticDefinition(
        DID.builtin_macro_only_if_elif,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'use of %q0 is restricted to #if and #elif directives',
    ),
    DID.builtin_macro_redefined: DiagnosticDefinition(
        DID.builtin_macro_redefined,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q0 is a built-in macro and cannot be %select{redefined|undefined}1',
    ),
    DID.cannot_be_defined: DiagnosticDefinition(
        DID.cannot_be_defined,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q0 cannot be %select{defined|undefined}1',
    ),
    DID.cannot_read_file: DiagnosticDefinition(
        DID.cannot_read_file,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'cannot read file %0: %1',
    ),
    DID.cannot_write_file: DiagnosticDefinition(
        DID.cannot_write_file,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'cannot write to file %0: %1',
    ),
    DID.character_does_not_exist: DiagnosticDefinition(
        DID.character_does_not_exist,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'character %q0 does not exist in character set %q1',
    ),
    DID.character_not_single_code_unit: DiagnosticDefinition(
        DID.character_not_single_code_unit,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'character %q0 cannot be encoded as a single %q1 in character set %q2',
    ),
    DID.codepoint_basic_character_set: DiagnosticDefinition(
        DID.codepoint_basic_character_set,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q0 is in the basic character set',
    ),
    DID.codepoint_cannot_begin_identifier: DiagnosticDefinition(
        DID.codepoint_cannot_begin_identifier,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q0 cannot begin an identifier',
    ),
    DID.codepoint_cannot_continue_identifier: DiagnosticDefinition(
        DID.codepoint_cannot_continue_identifier,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q0 is not permitted in identifiers or numbers',
    ),
    DID.codepoint_control_character: DiagnosticDefinition(
        DID.codepoint_control_character,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q0 is a control character',
    ),
    DID.codepoint_invalid: DiagnosticDefinition(
        DID.codepoint_invalid,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q0 is not a valid codepoint',
    ),
    DID.codepoint_surrogate: DiagnosticDefinition(
        DID.codepoint_surrogate,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q0 is a surrogate codepoint',
    ),
    DID.comma_in_pp_expression: DiagnosticDefinition(
        DID.comma_in_pp_expression,
        DiagnosticSeverity.warning,
        DiagnosticGroup.comma_expr,
        'comma operator should not be used in a preprocessor expression',
    ),
    DID.compilation_halted: DiagnosticDefinition(
        DID.compilation_halted,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        'Compilation halted.',
    ),
    DID.delimeter_invalid_character: DiagnosticDefinition(
        DID.delimeter_invalid_character,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'invalid character %q0 in raw string literal delimeter',
    ),
    DID.delimeter_too_long: DiagnosticDefinition(
        DID.delimeter_too_long,
        DiagnosticSeverity.warning,
        DiagnosticGroup.raw_delimiter,
        'raw string literal delimeter is too long',
    ),
    DID.division_by_zero: DiagnosticDefinition(
        DID.division_by_zero,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%select{division|remainder}0 by zero',
    ),
    DID.duplicate_macro_parameter: DiagnosticDefinition(
        DID.duplicate_macro_parameter,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'duplicate macro parameter %q0',
    ),
    DID.else_after_else: DiagnosticDefinition(
        DID.else_after_else,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '#%0 after #else',
    ),
    DID.else_location: DiagnosticDefinition(
        DID.else_location,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        '#else was here',
    ),
    DID.else_without_if: DiagnosticDefinition(
        DID.else_without_if,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '#%0 without #if',
    ),
    DID.empty_character_literal: DiagnosticDefinition(
        DID.empty_character_literal,
        DiagnosticSeverity.error,
        DiagnosticGroup.invalid_pp_token,
        'empty character literal',
    ),
    DID.endif_without_if: DiagnosticDefinition(
        DID.endif_without_if,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '#endif without #if',
    ),
    DID.error_directive: DiagnosticDefinition(
        DID.error_directive,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        '%0',
    ),
    DID.error_limit_reached: DiagnosticDefinition(
        DID.error_limit_reached,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        'Error limit reached.',
    ),
    DID.error_summary: DiagnosticDefinition(
        DID.error_summary,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%plural{1:error|:errors}0 generated compiling %1.',
    ),
    DID.escape_sequence_value_too_large: DiagnosticDefinition(
        DID.escape_sequence_value_too_large,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%select{octal|hexadecimal}0 escape sequence value %1 is too large for type %q2',
    ),
    DID.expected_close_brace: DiagnosticDefinition(
        DID.expected_close_brace,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected a %q{}}',
    ),
    DID.expected_close_paren: DiagnosticDefinition(
        DID.expected_close_paren,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected a %q{)}',
    ),
    DID.expected_colon: DiagnosticDefinition(
        DID.expected_colon,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected a %q{:}',
    ),
    DID.expected_comma_in_parameter_list: DiagnosticDefinition(
        DID.expected_comma_in_parameter_list,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected %q{,} in macro parameter list',
    ),
    DID.expected_expression: DiagnosticDefinition(
        DID.expected_expression,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected an expression',
    ),
    DID.expected_header_name: DiagnosticDefinition(
        DID.expected_header_name,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'expected a header name of the form "FILENAME" or <FILENAME>',
    ),
    DID.expected_identifier: DiagnosticDefinition(
        DID.expected_identifier,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected an identifier',
    ),
    DID.expected_macro_name: DiagnosticDefinition(
        DID.expected_macro_name,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected a macro name',
    ),
    DID.expected_macro_parameter: DiagnosticDefinition(
        DID.expected_macro_parameter,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected a macro parameter name',
    ),
    DID.expected_open_brace: DiagnosticDefinition(
        DID.expected_open_brace,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected a %q{{}',
    ),
    DID.expected_open_paren: DiagnosticDefinition(
        DID.expected_open_paren,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected a %q{(}',
    ),
    DID.expected_string_literal: DiagnosticDefinition(
        DID.expected_string_literal,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'expected a string literal',
    ),
    DID.extra_directive_tokens: DiagnosticDefinition(
        DID.extra_directive_tokens,
        DiagnosticSeverity.warning,
        DiagnosticGroup.extra_tokens,
        'extra tokens ignored at end of #%0 directive',
    ),
    DID.fatal_error_and_error_summary: DiagnosticDefinition(
        DID.fatal_error_and_error_summary,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%plural{1:fatal error|:fatal errors}0 and %plural{1:error|:errors}1 generated compil'
        'ing %2.',
    ),
    DID.fatal_error_summary: DiagnosticDefinition(
        DID.fatal_error_summary,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%plural{1:fatal error|:fatal errors}0 generated compiling %1.',
    ),
    DID.filename_should_be_string: DiagnosticDefinition(
        DID.filename_should_be_string,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'filename should be a string literal',
    ),
    DID.floating_point_in_pp_expr: DiagnosticDefinition(
        DID.floating_point_in_pp_expr,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'floating point numbers are not permitted in preprocessor expressions',
    ),
    DID.from_Pragma: DiagnosticDefinition(
        DID.from_Pragma,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'from _Pragma operator',
    ),
    DID.from_formation_of_header_name: DiagnosticDefinition(
        DID.from_formation_of_header_name,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'from formation of header name',
    ),
    DID.function_like_macro_not_defined: DiagnosticDefinition(
        DID.function_like_macro_not_defined,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'function-like macro %q0 is not defined',
    ),
    DID.hash_requires_macro_parameter: DiagnosticDefinition(
        DID.hash_requires_macro_parameter,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        "'#' must be followed by a macro parameter",
    ),
    DID.header_file_not_found: DiagnosticDefinition(
        DID.header_file_not_found,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'header file %q0 not found',
    ),
    DID.hexadecimal_exponent_required: DiagnosticDefinition(
        DID.hexadecimal_exponent_required,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'hexadecimal floating point numbers require an exponent',
    ),
    DID.identifier_not_NFC: DiagnosticDefinition(
        DID.identifier_not_NFC,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'identifier %q0 is not in Normalization Form C',
    ),
    DID.in_argument_stringizing: DiagnosticDefinition(
        DID.in_argument_stringizing,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'in stringizing of macro argument',
    ),
    DID.in_expansion_of_builtin: DiagnosticDefinition(
        DID.in_expansion_of_builtin,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'in expansion of builtin macro %q0',
    ),
    DID.in_expansion_of_macro: DiagnosticDefinition(
        DID.in_expansion_of_macro,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'in expansion of macro %q0',
    ),
    DID.in_token_concatenation: DiagnosticDefinition(
        DID.in_token_concatenation,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'in result of token concatenation',
    ),
    DID.incomplete_UCN_as_tokens: DiagnosticDefinition(
        DID.incomplete_UCN_as_tokens,
        DiagnosticSeverity.warning,
        DiagnosticGroup.unicode,
        'incomplete universal character name; treating as separate tokens',
    ),
    DID.integer_overflow: DiagnosticDefinition(
        DID.integer_overflow,
        DiagnosticSeverity.warning,
        DiagnosticGroup.overflow,
        'integer overflow',
    ),
    DID.integer_too_large: DiagnosticDefinition(
        DID.integer_too_large,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'integer value is too large for any integer type',
    ),
    DID.invalid_charset: DiagnosticDefinition(
        DID.invalid_charset,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        '%q0 encoding cannot be used for type %q1 with width %2 bits',
    ),
    DID.invalid_digit: DiagnosticDefinition(
        DID.invalid_digit,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'invalid %select{binary|octal|decimal|hexadecimal}0 digit',
    ),
    DID.invalid_directive: DiagnosticDefinition(
        DID.invalid_directive,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'invalid directive %q0',
    ),
    DID.invalid_in_filename: DiagnosticDefinition(
        DID.invalid_in_filename,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'filename cannot have %select{an encoding prefix|a user-defined suffix}0',
    ),
    DID.invalid_numeric_suffix: DiagnosticDefinition(
        DID.invalid_numeric_suffix,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'invalid suffix on numeric literal',
    ),
    DID.invalid_op_in_pp_expression: DiagnosticDefinition(
        DID.invalid_op_in_pp_expression,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'operator %q0 cannot appear in a preprocessor expression',
    ),
    DID.invalid_variadic_identifier_use: DiagnosticDefinition(
        DID.invalid_variadic_identifier_use,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'use of %q0 is invalid here',
    ),
    DID.left_shift_overflows: DiagnosticDefinition(
        DID.left_shift_overflows,
        DiagnosticSeverity.warning,
        DiagnosticGroup.overflow,
        'left shift overflows',
    ),
    DID.line_number_must_be_digit_sequence: DiagnosticDefinition(
        DID.line_number_must_be_digit_sequence,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'line number must be a digit sequence',
    ),
    DID.line_number_out_of_range: DiagnosticDefinition(
        DID.line_number_out_of_range,
        DiagnosticSeverity.warning,
        DiagnosticGroup.line_number,
        'the standard requires a line number between 1 and %0',
    ),
    DID.macro_defined_here: DiagnosticDefinition(
        DID.macro_defined_here,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'macro %q0 was defined here',
    ),
    DID.macro_definition_ends_with_concat: DiagnosticDefinition(
        DID.macro_definition_ends_with_concat,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'macro definition cannot end with %q{##}',
    ),
    DID.macro_definition_starts_with_concat: DiagnosticDefinition(
        DID.macro_definition_starts_with_concat,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'macro definition cannot begin with %q{##}',
    ),
    DID.macro_name_not_identifier: DiagnosticDefinition(
        DID.macro_name_not_identifier,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'macro name must be an identifier',
    ),
    DID.macro_name_whitespace: DiagnosticDefinition(
        DID.macro_name_whitespace,
        DiagnosticSeverity.warning,
        DiagnosticGroup.macro_ws,
        'macro name should be followed by whitespace',
    ),
    DID.macro_produced_defined: DiagnosticDefinition(
        DID.macro_produced_defined,
        DiagnosticSeverity.error,
        DiagnosticGroup.defined,
        'macro expansion producing operator %q{defined} is ill-formed',
    ),
    DID.macro_redefined: DiagnosticDefinition(
        DID.macro_redefined,
        DiagnosticSeverity.warning,
        DiagnosticGroup.macro_redefined,
        'redefinition of macro %q0',
    ),
    DID.max_include_depth_reached: DiagnosticDefinition(
        DID.max_include_depth_reached,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'maximum include file depth of %0 reached',
    ),
    DID.missing_digit_sequence: DiagnosticDefinition(
        DID.missing_digit_sequence,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'missing %select{octal|hexadecimal}0 digit sequence',
    ),
    DID.multicharacter_literal: DiagnosticDefinition(
        DID.multicharacter_literal,
        DiagnosticSeverity.warning,
        DiagnosticGroup.multichar,
        'multicharacter literal',
    ),
    DID.multicharacter_literal_truncated: DiagnosticDefinition(
        DID.multicharacter_literal_truncated,
        DiagnosticSeverity.warning,
        DiagnosticGroup.multichar,
        'value of multicharacter literal truncated to fit in type %q{int}',
    ),
    DID.multicharacter_literal_with_prefix: DiagnosticDefinition(
        DID.multicharacter_literal_with_prefix,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'multicharacter literals cannot have an encoding prefix',
    ),
    DID.nested_va_opt: DiagnosticDefinition(
        DID.nested_va_opt,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q{__VA_OPT__} cannot appear within its own replacement tokens',
    ),
    DID.predefined_macro_redefined: DiagnosticDefinition(
        DID.predefined_macro_redefined,
        DiagnosticSeverity.warning,
        DiagnosticGroup.predefined,
        '%select{redefinition|undefinition}1 of predefined macro %q0',
    ),
    DID.prior_macro_definition: DiagnosticDefinition(
        DID.prior_macro_definition,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'previous definition was here',
    ),
    DID.prior_match: DiagnosticDefinition(
        DID.prior_match,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'to match this %q0',
    ),
    DID.severity_error: DiagnosticDefinition(
        DID.severity_error,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        'error',
    ),
    DID.severity_fatal: DiagnosticDefinition(
        DID.severity_fatal,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        'fatal error',
    ),
    DID.severity_note: DiagnosticDefinition(
        DID.severity_note,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        'note',
    ),
    DID.severity_remark: DiagnosticDefinition(
        DID.severity_remark,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        'remark',
    ),
    DID.severity_warning: DiagnosticDefinition(
        DID.severity_warning,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        'warning',
    ),
    DID.shift_count_negative: DiagnosticDefinition(
        DID.shift_count_negative,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'shift count is negative',
    ),
    DID.shift_count_too_large: DiagnosticDefinition(
        DID.shift_count_too_large,
        DiagnosticSeverity.error,
        DiagnosticGroup.shift_count,
        'shift count must be less than the integer width',
    ),
    DID.shift_of_negative_value: DiagnosticDefinition(
        DID.shift_of_negative_value,
        DiagnosticSeverity.warning,
        DiagnosticGroup.shift_of_negative,
        '%select{left|right}0 shift of negative value',
    ),
    DID.starting_compilation: DiagnosticDefinition(
        DID.starting_compilation,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        'Starting compilation of %0.',
    ),
    DID.string_concatenation_conflict: DiagnosticDefinition(
        DID.string_concatenation_conflict,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'string concatenation with different %select{encoding prefixes|user-defined suffixes}'
        '0',
    ),
    DID.string_concatenation_prior: DiagnosticDefinition(
        DID.string_concatenation_prior,
        DiagnosticSeverity.note,
        DiagnosticGroup.none,
        'prior conflicting %select{encoding prefix|user-defined suffix}0 was here',
    ),
    DID.string_invalid_in_pp_expression: DiagnosticDefinition(
        DID.string_invalid_in_pp_expression,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'a string cannot appear in a preprocessor expression',
    ),
    DID.stringize_failed: DiagnosticDefinition(
        DID.stringize_failed,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'stringizing did not produce a valid string literal',
    ),
    DID.token_concatenation_failed: DiagnosticDefinition(
        DID.token_concatenation_failed,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'token concatenation produces %q0 which is not a valid token',
    ),
    DID.too_few_macro_arguments: DiagnosticDefinition(
        DID.too_few_macro_arguments,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'too few arguments passed to macro %q0',
    ),
    DID.too_many_macro_arguments: DiagnosticDefinition(
        DID.too_many_macro_arguments,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'too many arguments passed to macro %q0',
    ),
    DID.unclosed_if_block: DiagnosticDefinition(
        DID.unclosed_if_block,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'unclosed #%0 directive block',
    ),
    DID.undefined_identifier: DiagnosticDefinition(
        DID.undefined_identifier,
        DiagnosticSeverity.remark,
        DiagnosticGroup.undefined,
        '%q0 is not defined; replacing with %q{0}',
    ),
    DID.unknown_charset: DiagnosticDefinition(
        DID.unknown_charset,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'unknown character set %q0',
    ),
    DID.unknown_diagnostic_group: DiagnosticDefinition(
        DID.unknown_diagnostic_group,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'unknown diagnostic group %q0',
    ),
    DID.unknown_target: DiagnosticDefinition(
        DID.unknown_target,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'unknown target %q0',
    ),
    DID.unrecognized_escape_sequence: DiagnosticDefinition(
        DID.unrecognized_escape_sequence,
        DiagnosticSeverity.warning,
        DiagnosticGroup.escape_sequence,
        'invalid escape sequence %q0',
    ),
    DID.unrecognized_universal_character_name: DiagnosticDefinition(
        DID.unrecognized_universal_character_name,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%q0 is not the name of a universal character',
    ),
    DID.unterminated_argument_list: DiagnosticDefinition(
        DID.unterminated_argument_list,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'unerminated argument list invoking macro %q0',
    ),
    DID.unterminated_block_comment: DiagnosticDefinition(
        DID.unterminated_block_comment,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'unterminated block comment',
    ),
    DID.unterminated_literal: DiagnosticDefinition(
        DID.unterminated_literal,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'unterminated %select{character|string|raw string}0 literal',
    ),
    DID.user_defined_suffix_in_pp_expr: DiagnosticDefinition(
        DID.user_defined_suffix_in_pp_expr,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'a user-defined suffix cannot be used in a preprocessor expression',
    ),
    DID.utf8_invalid: DiagnosticDefinition(
        DID.utf8_invalid,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'invalid UTF-8 encoding',
    ),
    DID.utf8_overlong: DiagnosticDefinition(
        DID.utf8_overlong,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'over-long UTF-8 encoding',
    ),
    DID.utf8_surrogate: DiagnosticDefinition(
        DID.utf8_surrogate,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'surrogate in UTF-8 encoding',
    ),
    DID.va_opt_ends_with_concat: DiagnosticDefinition(
        DID.va_opt_ends_with_concat,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '__VA_OPT__ replacement tokens cannot end with %q{##}',
    ),
    DID.va_opt_starts_with_concat: DiagnosticDefinition(
        DID.va_opt_starts_with_concat,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '__VA_OPT__ replacement tokens cannot begin with %q{##}',
    ),
    DID.value_changes_sign: DiagnosticDefinition(
        DID.value_changes_sign,
        DiagnosticSeverity.warning,
        DiagnosticGroup.sign_change,
        'integer promotion causes value to change sign from %0 to %1',
    ),
    DID.vertical_whitespace_in_directive: DiagnosticDefinition(
        DID.vertical_whitespace_in_directive,
        DiagnosticSeverity.ignored,
        DiagnosticGroup.directive_ws,
        '%select{vertical tab|form feed}0 in preprocessing directive',
    ),
    DID.warning_directive: DiagnosticDefinition(
        DID.warning_directive,
        DiagnosticSeverity.warning,
        DiagnosticGroup.warning_directive,
        '%0',
    ),
}
