#!/usr/bin/env python3

import sys
import pkgutil
import importlib
import json
from typing import List, Dict, Any

class PyInterpreter:
    """

    A PyInterpreter is a service that discovers a set of Python modules, each
    exporting a function, and then uses those functions to interpret and
    evaluate JSON representations of calls to those functions.

    This forms the core of how we extend Exposure with additional functions
    implemented in Python.

    The core loop accepts a line of JSON input of the form:
      { "function" : f, "args" : [...] }

    If a module `f` exporting `f` was discovered, then the interpreter will
    decode the "args" JSON and evaluate `f` on those values. The PyInterpreter
    will then JSONify the result and print a JSON line of the form:

      { "type" : "success" , "value" : ... }

    If there is an error at any stage of this, the PyInterpreter will print
    a JSON line of the form:

      { "type" : "error" , "message" : "..." }
    """
    def __init__(self, roots: List[str]):
        self.ext_table = {}
        self.outh = sys.stdout
        for root in roots:
            sys.path.append(root)
            found = pkgutil.iter_modules(path=[root])
            for _finder,name,_ipkg in found:
                self.ext_table[name] = importlib.import_module(name)

    def fail(self, msg: Dict):
        """
        Send a failure message
        """
        resp = { 'type' : 'failure', 'message' : msg }
        self.send_response(resp)

    def succeed(self, val: Dict):
        """
        Send a success message
        """
        resp = { 'type' : 'success', 'value' : val }
        self.send_response(resp)

    def send_response(self, resp: Dict):
        """
        Send a response to the client
        """
        self.outh.write(json.dumps(resp))
        self.outh.write('\n')
        self.outh.flush()

    def callfn(self, fn: str, args: List[Any]):
        """
        Look up the function and try to call it
        """
        if fn in self.ext_table:
            func = getattr(self.ext_table[fn], fn)
            try:
                self.succeed(func(*args))
            except Exception as e:
                self.fail(str(e))
        else:
            self.fail(f"Unknown function: {fn}")

    def run_interpreter(self):
        """
        Main REPL loop: get input, decode, call function, encode, print
        """
        while True:
            try:
                line = input()
            except EOFError:
                return

            try:
                cmd  = json.loads(line)
            except:
                self.fail(f"Unable to parse command: {line}")
                continue

            try:
                fn   = cmd['function']
                args = cmd['args']
            except KeyError as e:
                self.fail(f"Command missing '{str(e)}'")
                continue

            self.callfn(fn ,args)

def main():
    interp = PyInterpreter(sys.argv[1:])
    interp.run_interpreter()

if __name__ == '__main__':
    main()
