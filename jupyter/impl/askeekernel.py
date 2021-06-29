"""
Implementation of the ASKE-E 'exposure' jupyter kernel.
This kernel is a thin wrapper around the REPL endpoint provided by Donu.
"""
import re
import json
from typing import Dict
import traceback
import requests
from ipykernel.kernelbase import Kernel

#------------------------------------------------------------------------------
# Config
#------------------------------------------------------------------------------
DONU_ADDRESS = "http://localhost:8000"

# -----------------------------------------------------------------------------
# Donu client
# -----------------------------------------------------------------------------

class Donu:
    """
    Represents a Donu session
    """
    # pylint: disable=too-few-public-methods
    def __init__(self, addr:str):
        self.sess = requests.Session()
        self.address = addr

    def request(self, req:Dict) -> Dict:
        """
        POST a request to the connected Donu session.
        req should conform to Donu's API
        """
        response = self.sess.post(self.address, json.dumps(req))
        try:
            return json.loads(response.text)
        except Exception as ex:
            raise Exception("Call to donu failed - response text: " + response.text) from ex
# -----------------------------------------------------------------------------
#  Kernel
# -----------------------------------------------------------------------------
def parse_code(code:str):
    """
    This returns the command that we should send to 'donu'. Normally this is just
    'execute-exposure', but we might also be given a 'magic' to reset the REPL
    state
    """
    cmd = None

    if re.match(r'\%clear', code) is not None:
        cmd = {
            'command' : 'clear-exposure-state',
        }
    else:
        cmd = {
            'command' : 'execute-exposure-code',
            'code': code
        }

    return cmd

class ASKEEKernel(Kernel):
    """
    The kernel class
    """
    # pylint: disable=abstract-method
    implementation = 'ASKE-E'
    implementation_version = '1.0'
    language = 'no-op'
    language_version = '0.1'
    language_info = {
        'name': 'Any text',
        'mimetype': 'text/plain',
        'file_extension': '.txt',
    }
    banner = "ASKE-E Kernel - TODO (come up with a better name)"
    donu   = Donu(DONU_ADDRESS)

    def do_execute(self, code:str, silent:bool, store_history=True, user_expressions=None,
                   allow_stdin=False):
        """
        The main method we need to supply. This gets the user's code from a cell and evaluates it
        by sending a request to Donu.
        """
        try:
            parsed = parse_code(code)
            resp   = self.execute_donu_cmd(parsed)
            output = { 'text/plain': resp }

            if not silent:
                self.send_response(self.iopub_socket, "display_data", {"data": output, "metadata": {}})

            return {
                'status': 'ok',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
            }

        except: # pylint: disable=bare-except
            trace_back = traceback.format_exc()
            self.output_err(trace_back)
            return {
                'status': 'error',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
            }

    def output_err(self, msg: str):
        stream_content = {'name': 'stdout', 'text': msg}
        self.send_response(self.iopub_socket, 'stream', stream_content)

    def execute_donu_cmd(self, cmd):
        if cmd is not None:
            resp = self.donu.request(cmd)
            if resp['status'] == 'success':
                return resp['result']
            if resp['status'] == 'error':
                return ("Error: %s" % resp['error'])
            raise Exception("Unexpected response status %s" % resp['status'])
        return None

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=ASKEEKernel)
