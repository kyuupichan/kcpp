import os

# import pytest

# from unicode import to_NFD


files_dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'files')


def read_file(filename):
    with open(os.path.join(files_dir, filename), 'rb') as f:
        return f.read()


def nf_testcases(start=0, end=100_000):
    count = 0
    with open(os.path.join(files_dir, 'NormalizationTest.txt'), 'r') as f:
        for line in f.readlines():
            c = line.find('#')
            if c != -1:
                line = line[:c]
            line = line.strip()
            if line and line[0] != '@':
                if start <= count < end:
                    yield tuple(''.join(chr(int(cp, 16)) for cp in field.split())
                                for field in line.split(';')[:5])
                count += 1


# @pytest.mark.parametrize("testcase", nf_testcases(end=1000))
# def test_NFD(testcase):
#     source = testcase[0]
#     source, _, NFD, _, _ = testcase
#     result = to_NFD(source)
#     print(source, len(source), NFD, len(NFD), result, len(result))
#     assert result == NFD
