import pytest


from kcpp.cpp import Preprocessor
from kcpp.diagnostics import DiagnosticManager


class TestLexer:

    @pytest.mark.parametrize('raw,answer', (
        # Empty buffers, backslashes
        (b'', (ord('\0'), 1)),
        (b'a', (ord('a'), 1)),
        (b'\\', (ord('\\'), 1)),
        (b'\\a', (ord('\\'), 1)),
        (b'\\\n', (ord('\0'), 3)),
        # Test \n. \r, \r\n are skipped but \n\r is a newline followed by \r.
        (b'\\\na', (ord('a'), 3)),
        (b'\\\ra', (ord('a'), 3)),
        (b'\\\r\na', (ord('a'), 4)),
        (b'\\\n\ra', (ord('\r'), 3)),
        # Test \\ followed by whitespace followed by newline
        (b'\\ \na', (ord('a'), 4)),
        (b'\\  \na', (ord('a'), 5)),
        (b'\\\t\na', (ord('a'), 4)),
        (b'\\\f\v \na', (ord('a'), 6)),
        (b'\\ \f \na', (ord('a'), 6)),
        # Test more than one spliace
        (b'\\ \f \n\\\na', (ord('a'), 8)),
        (b'\\ \t\n\\  \na', (ord('a'), 9)),
    ))
    def test_read_logical_byte(self, raw, answer):
        lexer = create_lexer(raw)
        assert lexer.read_logical_byte(0) == answer


def create_lexer(raw):
    pp = Preprocessor(DiagnosticManager())
    return pp.push_virtual_buffer('<test>', raw)
