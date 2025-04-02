import os
import pytest

from kcpp.unicode.name_to_cp import name_to_cp

cur_dir = os.path.dirname(os.path.realpath(__file__))


def name_to_cp_testcases():
    with open(os.path.join(cur_dir, 'name_to_cp.txt'), 'r') as f:
        for line in f.readlines():
            comment = line.find('#')
            if comment != -1:
                line = line[:comment]
            line = line.strip()
            if not line:
                continue
            cp_hex, name = line.split(':')
            cp = -1 if not cp_hex else int(cp_hex, 16)
            yield name, cp


@pytest.mark.parametrize('name, cp', list(name_to_cp_testcases()))
def test_name_to_cp(name, cp):
    assert name_to_cp(name) == cp
