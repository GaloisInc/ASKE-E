"""
Implementation of the ASKE-E 'exposure' jupyter kernel.
This kernel is a thin wrapper around the REPL endpoint provided by Donu.
"""
import re
import json
from typing import Dict
import traceback
import tabulate
import requests
from ipykernel.kernelbase import Kernel

#------------------------------------------------------------------------------
# Config
#------------------------------------------------------------------------------
DONU_ADDRESS = "http://localhost:8000"
vega_lite_schema = "https://vega.github.io/schema/vega-lite/v4.json"

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

def format_data_series(time, values):
    """
    Returns a vega light chart + a textual representation
    """
    timeTab = ["time"] + time
    valTabs = [ [k] + values[k] for k in values ]
    txt = tabulate.tabulate([timeTab] + valTabs)

    vals = []
    for (i, t) in enumerate(time):
        for k in values:
            vals.append({'time': t,
                         'series': k,
                         'y': values[k][i]})

    layers = [ { "mark": "line",
          "encoding": {
              "x": {"field": "time", "type": "quantitative"},
              "y": {"field": "y", "type":"quantitative"},
              "color": {"field": "series", "type": "nominal" }
              }
        } ]


    out = {
        'application/vnd.vegalite.v4+json': {
            '$schema': vega_lite_schema,
            'description':'',
            'data': { "values": vals},
            'mark': 'point',
            'layer': layers
        },

        'text/plain': txt
    }
    return out

def format_histogram(lo, _hi, sz, bins):
    """
    Returns a vega lite bar graph + textual representation as json
    """
    def mk_bin(b):
        count = bins[b]
        b = int(b)
        return {'lo': lo+(b*sz), 'hi':lo+((b+1)*sz), 'count': int(count) }

    values = [ mk_bin(b) for b in bins ]
    out = {
        'application/vnd.vegalite.v4+json': {
            '$schema': vega_lite_schema,
            'data' : {
                'values' : values
            },
            'mark' : 'bar',
            'encoding': {
                'x': { 'field': 'lo', 'bin': { 'binned': 'true', "step": sz } },
                'x2': {'field': 'hi'},
                'y': {'field': 'count', 'type':'quantitative'}
            }
        }
        ,
        'text/plain' : json.dumps(values)
    }

    return out

def format_plot(xlab, ylabs, xs, yss):
    """
    Returns a vega light chart + a textual representation
    """
    vals = []
    for (t, ys) in enumerate(yss):
        for (yi,ylab) in enumerate(ylabs):
            vals.append({ xlab: xs[t], ylab: ys[yi], "series": ylab })

    layers = [ { "mark": "line",
                 "encoding": {
                     "x": {"field": xlab, "type": "quantitative"},
                     "y": {"field": ylab, "type": "quantitative"},
                     "color": {"field": "series", "type": "nominal"}
                 }
                } for ylab in ylabs ]

    out = {
        'application/vnd.vegalite.v4+json': {
            '$schema': vega_lite_schema,
            'description':'',
            'data': { "values": vals},
            'mark': 'point',
            'layer': layers
        },
    }

    #     'text/plain': txt
    # }
    return out


def format_scatter(xlab, ylabs, xs, ysss):
    """
    Returns a vega light chart + a textual representation
    """
    vals = []
    for (si,ys) in enumerate(ysss):
        for (i, x) in enumerate(xs):
            for ysamp in ys[i]:
                vals.append({ xlab: x, ylabs[si]: ysamp, "series": ylabs[si] })

    layers = [ { "mark": "point",
                 "encoding": {
                     "x": {"field": xlab, "type": "quantitative"},
                     "y": {"field": ylab, "type": "quantitative"},
                     "color": {"field": "series", "type": "nominal"}
                 }
                } for ylab in ylabs ]

    out = {
        'application/vnd.vegalite.v4+json': {
            '$schema': vega_lite_schema,
            'description':'',
            'data': { "values": vals},
            'mark': 'point',
            'layer': layers
        },
    }

    #     'text/plain': txt
    # }
    return out


def format_latex(orig_latex):
    """
    Jupyter requires the LaTeX to be:

    (1) Surrounded by $$ ... $$
    (2) Using \\ to indicate newlines

    This function performs surgery on ASKE-E–pretty-printed LaTeX to conform
    to these requirements.
    """
    return "$$ \n" + ''.join(w+" \\\\\n" for w in orig_latex.splitlines()) + "$$"


def format_table(labels, rows):
    """
    TODO RGS: Docs
    """
    def mk_md_row(cells):
        row  = "|"
        row += "|".join(cells)
        return row + "|"

    md = []
    md.append(mk_md_row(labels))
    md.append(mk_md_row(["---" for i in range(len(labels))]))
    for row in rows:
        md.append(mk_md_row(str(e['value']) for e in row))

    out = {
        'text/markdown': "\n".join(md)
    }
    return out


def format_resp_value(v):
    if isinstance(v, dict):
        ty = v['type']
        val = v['value']

        if ty == 'string':
            return { 'text/plain': val }

        if ty == 'double':
            return { 'text/plain': json.dumps(val) }

        if ty == 'plot':
            return format_plot(val['xlabel'], val['ylabels'], val['xs'], val['yss'])

        if ty == 'scatter':
            return format_scatter(val['xlabel'], val['ylabels'], val['xs'], val['yss'])

        if ty == 'data-series':
            return format_data_series(val['time'], val['values'])

        if ty == 'histogram':
            return format_histogram(float(val['min']),
                                    float(val['max']),
                                    float(val['size']),
                                    val['bins'])

        if ty == 'latex':
            return { 'text/latex': format_latex(val) }

        if ty == 'table':
            return format_table(val['labels'], val['rows'])

    return { 'text/plain': json.dumps(v) }


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
            # Resp should be a list of things to display
            # lines = "\n".join([format_resp_value(r) for r in resp])
            lines = [format_resp_value(r) for r in resp]
            # output = { 'text/plain': lines }

            if not silent:
                for output in lines:
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
                return ["Error: %s" % resp['error']]
            raise Exception("Unexpected response status %s" % resp['status'])
        return None

    def run_as_main():
        from ipykernel.kernelapp import IPKernelApp
        IPKernelApp.launch_instance(kernel_class=ASKEEKernel)
