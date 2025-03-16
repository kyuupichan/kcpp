#!/usr/bin/env python
#
# Copyright (c) 2025 Neil Booth.
#
# A C++23 Preprocessor in Python 3.
#

import os

from driver import Driver


def main(argv=None):
    driver = Driver()
    driver.run(argv, os.environ)


if __name__ == '__main__':
    main()
