"""
Implementation of the ASKE-E 'exposure' jupyter kernel.
This kernel is a thin wrapper around the REPL endpoint provided by Donu.
"""
import re
import json
from typing import Dict
import traceback
import tabulate
from ipykernel.kernelbase import Kernel
import json
import websocket
import threading
import queue
import base64
import os

# -----------------------------------------------------------------------------
# Flags that control diagnostic/debugging features
# -----------------------------------------------------------------------------
DEBUG_ENV_VAR = "ASKEE_DEBUG"
DEBUG_TIME = False

#------------------------------------------------------------------------------
# Config
#------------------------------------------------------------------------------
DONU_HOSTNAME = os.environ.get("ASKE_DONU_HOSTNAME", "localhost")
DONU_WS_ADDRESS = f"ws://{DONU_HOSTNAME}:8000/:exposure"
vega_lite_schema = "https://vega.github.io/schema/vega-lite/v4.json"


# -----------------------------------------------------------------------------
# Donu client
# -----------------------------------------------------------------------------

def on_message(resp_q):
    """
    This just adds any websocket messages to our
    internal response queue so that they can be
    processed by the protocol thread
    """
    def go(app, msg):
        resp_q.put(msg)
    return go

def on_open(req_q, resp_q, kernel_q):
    """
    Starts the protocol thread on connect
    """
    def go(app):
        threading.Thread(target=exposure_protocol, args=[app, req_q, resp_q, kernel_q]).start()
    return go

def exposure_protocol(app, req_q, resp_q, to_kernel_q):
    """
    Implements the exposure protocol for the client.
    This looks like:
    1. Client sends code to donu for evaluation
    2. On response,
      a. the response is a request for a file: we read the file and send it back, go to 2
      b. the response is a success or failure: enqueue the response on the kernel queue.
    """
    while True:
        app.send(req_q.get())
        while True:
            resp = json.loads(resp_q.get())

            if resp['type'] == 'read-file':
                try:
                    with open(resp['path']) as f:
                        app.send(json.dumps({'type':'file-contents', 'contents': f.read()}))
                except FileNotFoundError:
                    app.send(json.dumps({'type':'error', 'message': 'File not found'}))
                except Exception as e:
                    app.send(json.dumps({'type':'error', 'message': str(e)}))

            elif resp['type'] == 'write-file':
                try:
                    with open(resp['path'], 'w+') as f:
                        f.write(resp['contents'])
                except FileNotFoundError:
                    app.send(json.dumps({'type':'error', 'message': 'File not found'}))
                except Exception as e:
                    app.send(json.dumps({'type':'error', 'message': str(e)}))

            elif resp['type'] == 'success':
                break

            elif resp['type'] == 'failure':
                break

            else:
                raise Exception(resp)
        to_kernel_q.put(resp)

class Donu:
    """
    Represents a Donu session
    """
    # pylint: disable=too-few-public-methods
    def __init__(self, addr:str):
        self.request_q = queue.Queue()
        self.response_q = queue.Queue()
        self.kernel_q = queue.Queue()
        self.app = websocket.WebSocketApp(
            addr,
            on_message=on_message(self.response_q),
            on_open=on_open(self.request_q, self.response_q, self.kernel_q)
        )
        self.app_thread = threading.Thread(target=self.app.run_forever)
        self.app_thread.start()

    def execute_code(self, code:str) -> Dict:
        """
        Send a message to the protocol thread to execute a new Donu stmt. Returns a JSON response
        indicating success or failure
        """
        self.request_q.put(json.dumps({'type':'run-program', 'code':code}))
        response = self.kernel_q.get()
        return response

# -----------------------------------------------------------------------------
#  Kernel
# -----------------------------------------------------------------------------
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

def format_plot(title, series, vs, vs_label):
    """
    Returns a vega light chart + a textual representation
    """
    vals = []
    for (vi,v) in enumerate(vs):
        for s in series:
            vals.append({ vs_label: v,
                          s['label']: s['data'][vi],
                          'series': s['label'] })

    layers = []
    for s in series:
        layer_color = {"field": "series", "type": "nominal"}
        series_color = s['color']
        if isinstance(series_color, str):
            layer_color['scale'] = {'scheme': series_color}
        elif isinstance(series_color, list):
            layer_color['scale'] = {'range': series_color}

        layers.append({ "mark": s['style'],
                        "encoding": {
                            "x": {"field": vs_label, "type": "quantitative"},
                            "y": {"field": s['label'], "type": "quantitative"},
                            "color": layer_color
                        }
                      })

    out = {
        'application/vnd.vegalite.v4+json': {
            '$schema': vega_lite_schema,
            'width': 800,
            'height': 600,
            'description':'',
            'data': { "values": vals},
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

def format_array(arr):
    return {
        'text/plain': '[' + ', '.join([formatted_text(el) for el in arr]) + ']'
    }

def format_timed(v):
    return {
        'text/plain': formatted_text(v['value']) + ' @ ' + formatted_text(v['time'])
    }

def format_point(pt):
    def format_one(k,v):
        return k + '=' + formatted_text(v)
    return {
        'text/plain': '{{' + ', '.join([format_one(k,v) for k,v in pt.items()]) + '}}'
    }

def format_svg(v):

    return {
        "image/svg+xml": str(base64.b64decode(v), "utf8")
    }


def formatted_text(v):
    formatted = format_resp_value(v)
    return formatted['text/plain']

def format_resp_value(v):
    if isinstance(v, dict):
        ty = v['type']
        val = v['value']

        if ty == 'string':
            return { 'text/plain': val }

        if ty == 'double':
            return { 'text/plain': json.dumps(val) }

        if ty == 'timed':
            return format_timed(val)

        if ty == 'plot':
            return format_plot(val['title'], val['series'], val['vs'], val['vs_label'])

        if ty == 'point':
            return format_point(val)

        if ty == 'scatter':
            return format_scatter(val['xlabel'], val['ylabels'], val['xs'], val['yss'])

        if ty == 'array':
            return format_array(val)

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

        if ty == 'svg':
            return format_svg(val)

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
    donu   = Donu(DONU_WS_ADDRESS)

    def do_execute(self, code:str, silent:bool, store_history=True, user_expressions=None,
                   allow_stdin=False):
        """
        The main method we need to supply. This gets the user's code from a cell and evaluates it
        by sending a request to Donu.
        """
        try:
            resp, time  = self.execute_donu_cmd(code)
            # Resp should be a list of things to display
            lines = [format_resp_value(r) for r in resp]

            if not silent:
                for output in lines:
                    self.send_response(self.iopub_socket, "display_data", {"data": output, "metadata": {}})

            if not silent:
                if time is not None:
                    self.send_response(self.iopub_socket, "display_data", {"data": time, "metadata": {}})

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
            resp = self.donu.execute_code(cmd)
            time = None
            if resp['type'] == 'success':
                if DEBUG_TIME and 'time' in resp:
                    time = {'text/plain': f"Command took {resp['time']/1000000} ms."}
                return (resp['displays'], time)
            if resp['type'] == 'failure':
                return ([{'type':'string', 'value':"Error: %s" % resp['message']}], time)
            raise Exception("Unexpected response status %s" % resp['type'])
        return None

    def run_as_main():
        from ipykernel.kernelapp import IPKernelApp

        debug = os.getenv(DEBUG_ENV_VAR)
        if debug == '1':
            global DEBUG_TIME
            DEBUG_TIME = True

        IPKernelApp.launch_instance(kernel_class=ASKEEKernel)
