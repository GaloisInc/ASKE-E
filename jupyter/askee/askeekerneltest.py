from askeekernel import *
import unittest

class ParserTest(unittest.TestCase):
    def test_parseCall(self):
        cmd = parse_command('loadCsv(b,"c",d)')
        call = cmd.expr
        self.assertTrue(isinstance(call, ExprCall))
        self.assertEqual(3, (len(call.args)))

if __name__ == '__main__':
    unittest.main()