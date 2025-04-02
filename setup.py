import os.path
import re
import setuptools


def find_version(filename):
    with open(filename) as f:
        text = f.read()
    match = re.search(r"^_version_str = '(.*)'$", text, re.MULTILINE)
    if not match:
        raise RuntimeError('cannot find version')
    return match.group(1)


tld = os.path.abspath(os.path.dirname(__file__))
version = find_version(os.path.join(tld, 'src', 'kcpp', '__init__.py'))


setuptools.setup(
    version=version,
    download_url=('https://github.com/kyuupichan/kcpp/archive/{version}.tar.gz'),
)
