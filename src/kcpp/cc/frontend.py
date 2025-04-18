# Copyright (c) 2025, Neil Booth.
#
# All rights reserved.
#
'''Compiler frontend.'''


from kcpp.core import TokenKind
from kcpp.driver import FrontEndBase


__all__ = ['Compiler']


class Compiler(FrontEndBase):

    help_group_name = 'compiler'

    def process(self, source, multiple):
        super().process(source, multiple)
        print('Reading tokens...')
        get_token = self.pp.get_token
        while True:
            token = get_token()
            if token.kind == TokenKind.EOF:
                break
        print('Done')
