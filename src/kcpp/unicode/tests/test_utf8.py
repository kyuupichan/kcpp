from random import randrange, choice

import pytest


from kcpp.unicode import utf8_cp


def random_range_values(start, end, avg_step_size):
    range_end = avg_step_size * 2
    # Always yield start and end - 1
    while start < end:
        yield start
        start += randrange(1, range_end)
    yield end - 1


def encode_2(cp):
    assert 0 <= cp < 0x800

    return bytes([0xc0 + (cp >> 6), 0x80 + (cp & 0x3f)])


def encode_3(cp):
    assert 0 <= cp < 0x10000

    return bytes([0xe0 + (cp >> 12), 0x80 + ((cp >> 6) & 0x3f), 0x80 + (cp & 0x3f)])


def encode_4(cp):
    assert 0 <= cp < 0x200000
    return bytes([0xf0 + (cp >> 18), 0x80 + ((cp >> 12) & 0x3f),
                  0x80 + ((cp >> 6) & 0x3f), 0x80 + (cp & 0x3f)])


def all_cases():
    bad_high = list(range(0xc0, 0x100))
    bad_continuations = list(range(0x80)) + bad_high

    def truncate_encoding(encoding, pos):
        result = bytearray(encoding)
        result[pos] = choice(bad_continuations)
        return result, (-1, pos)

    # Immediate EOF
    yield b'', (-1, 0)

    # Single char cases
    for cp in random_range_values(0x0, 0x80, 3):
        yield bytes([cp]), (cp, 1)
        # Overlong encodings
        yield encode_2(cp), (-2, 2)
        yield encode_3(cp), (-2, 3)
        yield encode_4(cp), (-2, 4)

    # Two-byte cases
    for cp in random_range_values(0x80, 0x800, 25):
        encoding = encode_2(cp)
        yield encoding, (cp, 2)
        yield truncate_encoding(encoding, 1)
        # Overlong encodings
        yield encode_3(cp), (-2, 3)
        yield encode_4(cp), (-2, 4)
        # EOF mid-stream
        yield encoding[:-1], (-1, 1)

    # Three-byte cases
    for cp in random_range_values(0x800, 0x10000, 256):
        # Handle surrogates
        ret = (-3, 3) if 0x0d800 <= cp <= 0x0dfff else (cp, 3)
        encoding = encode_3(cp)
        yield encoding, ret
        yield truncate_encoding(encoding, randrange(1, 3))
        # Overlong encoding (also for surrogates)
        yield encode_4(cp), (-2, 4)
        # EOF mid-stream
        size = randrange(1, 3)
        yield encoding[:size], (-1, size)

    # Four-byte cases
    for cp in random_range_values(0x010000, 0x110000, 16000):
        encoding = encode_4(cp)
        yield encoding, (cp, 4)
        yield truncate_encoding(encoding, randrange(1, 4))
        # EOF mid-stream
        size = randrange(1, 4)
        yield encoding[:size], (-1, size)

    # Out of range cps
    for cp in random_range_values(0x110000, 0x200000, 15000):
        yield encode_4(cp), (-1, 4)


@pytest.mark.parametrize("raw, answer", all_cases())
def test_utf8(raw, answer):
    assert utf8_cp(raw, 0) == answer
