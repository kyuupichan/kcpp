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
    none = 0
    remark = 1
    note = 2
    warning = 3
    error = 4
    fatal = 5
    ice = 6


class DiagnosticGroup(IntEnum):
    none = 0
    invalid_pp_token = 1


class DID(IntEnum):
    at_file_and_end_of_line = auto()
    at_file_and_line = auto()
    at_file_end = auto()
    at_file_line_and_column = auto()
    brief_at_file_and_line = auto()
    brief_at_file_line_and_column = auto()
    builtin_macro_redefined = auto()
    cannot_be_defined = auto()
    cannot_open_file = auto()
    character_does_not_exist = auto()
    character_not_single_code_unit = auto()
    codepoint_basic_character_set = auto()
    codepoint_cannot_begin_identifier = auto()
    codepoint_cannot_continue_identifier = auto()
    codepoint_control_character = auto()
    codepoint_invalid = auto()
    codepoint_surrogate = auto()
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
    errors_generated = auto()
    escape_sequence_value_too_large = auto()
    expected_close_brace = auto()
    expected_close_paren = auto()
    expected_colon = auto()
    expected_comma_in_parameter_list = auto()
    expected_expression = auto()
    expected_macro_name = auto()
    expected_macro_parameter = auto()
    expected_open_brace = auto()
    expected_open_paren = auto()
    extra_directive_tokens = auto()
    filename_should_be_string = auto()
    floating_point_in_pp_expr = auto()
    hash_requires_macro_parameter = auto()
    hexadecimal_exponent_required = auto()
    identifier_in_pp_expr = auto()
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
    left_shift_of_negative_value = auto()
    left_shift_overflows = auto()
    line_number_must_be_digit_sequence = auto()
    line_number_out_of_range = auto()
    macro_defined_here = auto()
    macro_definition_ends_with_concat = auto()
    macro_definition_starts_with_concat = auto()
    macro_name_not_identifier = auto()
    macro_name_whitespace = auto()
    macro_redefined = auto()
    missing_digit_sequence = auto()
    multicharacter_literal = auto()
    multicharacter_literal_truncated = auto()
    multicharacter_literal_with_prefix = auto()
    nested_va_opt = auto()
    predefined_macro_redefined = auto()
    prior_macro_definition = auto()
    prior_match = auto()
    right_shift_of_negative_value = auto()
    severity_error = auto()
    severity_fatal = auto()
    severity_note = auto()
    severity_remark = auto()
    severity_warning = auto()
    shift_count_negative = auto()
    shift_count_too_large = auto()
    string_concatenation_conflict = auto()
    string_concatenation_prior = auto()
    string_invalid_in_pp_expression = auto()
    stringize_failed = auto()
    token_concatenation_failed = auto()
    too_few_macro_arguments = auto()
    too_many_macro_arguments = auto()
    unknown_charset = auto()
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
    did: DID
    severity: DiagnosticSeverity
    group: DiagnosticGroup
    text: str


diagnostic_definitions = {
    DID.at_file_and_end_of_line: DiagnosticDefinition(
        DID.at_file_and_end_of_line,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '"%0", at end of line %1',
    ),
    DID.at_file_and_line: DiagnosticDefinition(
        DID.at_file_and_line,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '"%0", line %1',
    ),
    DID.at_file_end: DiagnosticDefinition(
        DID.at_file_end,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '"%0", at end of source',
    ),
    DID.at_file_line_and_column: DiagnosticDefinition(
        DID.at_file_line_and_column,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '"%0", line %1, column %2',
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
    DID.cannot_open_file: DiagnosticDefinition(
        DID.cannot_open_file,
        DiagnosticSeverity.fatal,
        DiagnosticGroup.none,
        'cannot open file %q0: %1',
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
    DID.delimeter_invalid_character: DiagnosticDefinition(
        DID.delimeter_invalid_character,
        DiagnosticSeverity.error,
        DiagnosticGroup.invalid_pp_token,
        'invalid character %q0 in raw string literal delimeter',
    ),
    DID.delimeter_too_long: DiagnosticDefinition(
        DID.delimeter_too_long,
        DiagnosticSeverity.error,
        DiagnosticGroup.invalid_pp_token,
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
    DID.errors_generated: DiagnosticDefinition(
        DID.errors_generated,
        DiagnosticSeverity.none,
        DiagnosticGroup.none,
        '%plural{1:error|:errors}0 generated.',
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
    DID.extra_directive_tokens: DiagnosticDefinition(
        DID.extra_directive_tokens,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'extra tokens at end of #%0 directive',
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
    DID.hash_requires_macro_parameter: DiagnosticDefinition(
        DID.hash_requires_macro_parameter,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        "'#' must be followed by a macro parameter",
    ),
    DID.hexadecimal_exponent_required: DiagnosticDefinition(
        DID.hexadecimal_exponent_required,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'hexadecimal floating point numbers require an exponent',
    ),
    DID.identifier_in_pp_expr: DiagnosticDefinition(
        DID.identifier_in_pp_expr,
        DiagnosticSeverity.remark,
        DiagnosticGroup.none,
        '%q0 is not defined; replacing with %q{0}',
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
        DiagnosticGroup.none,
        'incomplete universal character name; treating as separate tokens',
    ),
    DID.integer_overflow: DiagnosticDefinition(
        DID.integer_overflow,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
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
        DiagnosticSeverity.error,
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
    DID.left_shift_of_negative_value: DiagnosticDefinition(
        DID.left_shift_of_negative_value,
        DiagnosticSeverity.warning,
        DiagnosticGroup.none,
        'left shift of negative value',
    ),
    DID.left_shift_overflows: DiagnosticDefinition(
        DID.left_shift_overflows,
        DiagnosticSeverity.warning,
        DiagnosticGroup.none,
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
        DiagnosticGroup.none,
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
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'macro name should be followed by whitespace',
    ),
    DID.macro_redefined: DiagnosticDefinition(
        DID.macro_redefined,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'redefinition of macro %q0',
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
        DiagnosticGroup.none,
        'multicharacter literal',
    ),
    DID.multicharacter_literal_truncated: DiagnosticDefinition(
        DID.multicharacter_literal_truncated,
        DiagnosticSeverity.warning,
        DiagnosticGroup.none,
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
        DiagnosticGroup.none,
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
    DID.right_shift_of_negative_value: DiagnosticDefinition(
        DID.right_shift_of_negative_value,
        DiagnosticSeverity.warning,
        DiagnosticGroup.none,
        'right shift of negative value',
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
        DiagnosticGroup.none,
        'shift count must be less than the integer width',
    ),
    DID.string_concatenation_conflict: DiagnosticDefinition(
        DID.string_concatenation_conflict,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'string concatenation with different %select{encoding prefixes|user-defined suffixes}0',
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
    DID.unknown_charset: DiagnosticDefinition(
        DID.unknown_charset,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        'unknown character set %q0',
    ),
    DID.unrecognized_escape_sequence: DiagnosticDefinition(
        DID.unrecognized_escape_sequence,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
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
        DiagnosticGroup.invalid_pp_token,
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
        DiagnosticGroup.none,
        'integer promotion causes value to change sign from %0 to %1',
    ),
    DID.vertical_whitespace_in_directive: DiagnosticDefinition(
        DID.vertical_whitespace_in_directive,
        DiagnosticSeverity.error,
        DiagnosticGroup.none,
        '%select{vertical tab|form feed}0 in preprocessing directive',
    ),
    DID.warning_directive: DiagnosticDefinition(
        DID.warning_directive,
        DiagnosticSeverity.warning,
        DiagnosticGroup.none,
        '%0',
    ),
}
