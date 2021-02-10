from ipykernel.kernelbase import Kernel
import json
import csv
import collections
import os
import requests
from typing import List, Set, Dict, Tuple, Optional, Generator

import antlr4
from traitlets.traitlets import Instance
from antlrgen.ASKEECommandVisitor import ASKEECommandVisitor
from antlrgen.ASKEECommandParser import ASKEECommandParser
from antlrgen.ASKEECommandLexer import ASKEECommandLexer
import traceback

import unittest

#------------------------------------------------------------------------------
# Config
#------------------------------------------------------------------------------

donuAddress = "http://localhost:8000"
vega_lite_schema = "https://vega.github.io/schema/vega-lite/v4.json"


# -----------------------------------------------------------------------------
#  AST
# -----------------------------------------------------------------------------

class AST:
    pass

class Expr(AST):
    pass

class ExprNumber(Expr):
    def __init__(self, value:float):
        self.value = value

class ExprString(Expr):
    def __init__(self, value:str):
        self.value = value

class ExprCall(Expr):
    def __init__(self, name:str, args:List[Expr]):
        self.functionName = name
        self.args = args

class ExprVar(Expr):
    def __init__(self, name:str):
        self.name = name

class ExprList(Expr):
    def __init__(self, elts:List[Expr]):
        self.elts = elts

class ExprMap(Expr):
    def __init__(self, elts:List[Tuple[str, Expr]]):
        self.elts = elts


class Stmt(AST):
    pass

class StmtAssign(Stmt):
    def __init__(self, name:str, expr:Expr):
        self.name = name
        self.expr = expr

class StmtEval(Stmt):
    def __init__(self, expr:Expr):
        self.expr = expr

# -----------------------------------------------------------------------------
#  Parser
# -----------------------------------------------------------------------------

class SyntaxError(Exception):
    def __init__(self, errors):
        self.errors = errors

class ASKEECommandASTVisitor(ASKEECommandVisitor):
    def identifier(self, ctx:antlr4.TerminalNode) -> str:
        return ctx.getText()

    def stringLit(self, ctx:antlr4.TerminalNode) -> str:
        return ctx.getText().strip('"')

    def number(self, ctx:antlr4.TerminalNode) -> float:
        return float(ctx.getText())

    def visitStmtAssign(self, ctx:ASKEECommandParser.StmtAssignContext) -> Stmt:
        name = self.identifier(ctx.IDENTIFIER())
        expr = self.visit(ctx.expr())
        return StmtAssign(name, expr)

    def visitStmtEval(self, ctx:ASKEECommandParser.StmtEvalContext) -> Stmt:
        return StmtEval(self.visit(ctx.expr()))

    def visitExprCall(self, ctx:ASKEECommandParser.ExprCallContext) -> Expr:
        name = self.identifier(ctx.IDENTIFIER())
        exprs = list([self.visit(expr) for expr in ctx.args])
        return ExprCall(name, exprs)

    def visitExprMap(self, ctx:ASKEECommandParser.ExprMapContext) -> Expr:
        values = [self.visit(expr) for expr in ctx.values]
        keys = [ident.text for ident in ctx.keys]
        return ExprMap(zip(keys, values))

    def visitExprVar(self, ctx:ASKEECommandParser.ExprVarContext) -> Expr:
        return ExprVar(self.identifier(ctx.IDENTIFIER()))

    def visitExprString(self, ctx:ASKEECommandParser.ExprStringContext) -> Expr:
        stripped = self.stringLit(ctx.STRING())
        return ExprString(stripped)

    def visitExprNumber(self, ctx:ASKEECommandParser.ExprNumberContext) -> Expr:
        return ExprNumber(self.number(ctx.NUMBER()))

    def visitExprList(self, ctx: ASKEECommandParser.ExprListContext):
        exprs = list([self.visit(expr) for expr in ctx.args])
        return ExprList(exprs)

class ErrorCollector(antlr4.error.ErrorListener.ErrorListener):
    def __init__(self):
        self.syntaxErrors = []

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.syntaxErrors.append(f"{str(line)}:{str(column)} - Syntax error: {msg}")

def parse_command(input: str) -> Stmt:
    stream = antlr4.InputStream(input)
    lexer = ASKEECommandLexer(stream)
    tokstream = antlr4.CommonTokenStream(lexer)
    errorCollector = ErrorCollector()
    parser = ASKEECommandParser(tokstream)
    parser.addErrorListener(errorCollector)
    visitor = ASKEECommandASTVisitor()
    stmt = parser.stmt()

    if len(errorCollector.syntaxErrors) > 0:
        raise SyntaxError(errorCollector.syntaxErrors)

    return visitor.visit(stmt)

# -----------------------------------------------------------------------------
# Donu client
# -----------------------------------------------------------------------------

class Donu:
    def __init__(self, addr:str):
        self.address = addr

    @staticmethod
    def modelTypeRNet():
        return "reaction-net"

    @staticmethod
    def modelTypeESL():
        return "askee"

    @staticmethod
    def modelTypeDEQ():
        return "diff-eqs"

    @staticmethod
    def modelTypeLEQArr():
        return "latex-eqnarray"

    @staticmethod
    def fileSource(fileName:str):
        return {
            "file": os.path.abspath(fileName)
        }

    def request(self, req):
        response = requests.post(self.address, json.dumps(req))
        if(response.status_code != 200):
            raise Exception("Call to donu failed - response text: " + response.text)
        return json.loads(response.text)

    def decodeResponse(self, resp, why):
        if resp['status'] == 'success':
            return resp['result']

        elif resp['status'] == 'error':
            raise IntepreterError(why, resp['error'])

        raise IntepreterError(why, f"[BUG] unexpected response status from Donu {resp['status']}")

    def setParams(self, model: str, paramMap: Dict[str, float], why: AST):
        req = {
            "command":"setparams-command",
            "definition":model,
            "parameters":paramMap
        }

        return self.decodeResponse(self.request(req), why)


    def deqSimulate(self, model: str, type: str, start: float, end: float, interval: float):
        req = {
            "command": "simulate",
            "start": start,
            "end": end,
            "model": type,
            "step": interval,
            "definition": model,
        }

        data = self.request(req)
        vals = dict(data["values"])
        vals["time"] = data["times"]
        return ValueDataSeries(vals)

    def checkModel(self, model: str, modelType: str, why: AST):
        req = {
            "command": "check-model",
            "model": modelType,
            "definition": model
        }

        self.decodeResponse(self.request(req), why)

    def convertModel(self, model:str, srcType:str, dstType:str, why: AST) -> List[str]:
        req = {
            "command": "convert-model",
            "model": srcType,
            "definition": model,
            "dest-model": dstType
        }

        return self.decodeResponse(self.request(req), why)

    def genCpp(self, model:str, srcType:str, why:AST) -> str:
        req = {
            "command": "generate-cpp",
            "model": srcType,
            "definition": model,
        }

        return self.decodeResponse(self.request(req), why)

    def stratifySpatial(self, model:str, connectionGraph:str, why:AST):
        req = {
            "command": "stratify-command",
            "definition": model,
            "connection-graph": Donu.fileSource(connectionGraph),
            "stratification-type": "spatial",
        }

        return self.request(req)['model']


# -----------------------------------------------------------------------------
# Interpreter
# -----------------------------------------------------------------------------

class IntepreterError(Exception):
    def __init__(self, why: AST, message: str):
        self.why = why
        self.message = message

class Value:
    pass

class ValueUnit(Value):
    pass

class ValueString(Value):
    def __init__(self, value:str):
        self.value = value

class ValueCode(ValueString):
    def __init__(self, value:str, language:str):
        self.value = value
        self.language = language

class ValueNumber(Value):
    def __init__(self, value:float):
        self.value = value

class ValueDataSeries(Value):
    def __init__(self, data:Dict[str, List[float]]):
        self.data = data

class ValueModel(Value):
    pass
class ValueDiffeqModel(ValueModel):
    def __init__(self, eqns:str):
        self.source = eqns

    @staticmethod
    def getDonuType():
        return Donu.modelTypeDEQ()

class ValueESLModel(ValueModel):
    def __init__(self, source:str):
        self.source = source

    @staticmethod
    def getDonuType():
        return Donu.modelTypeESL()

class ValueRNetModel(ValueModel):
    def __init__(self, source:str):
        self.source = source

    @staticmethod
    def getDonuType():
        return Donu.modelTypeRNet()

class ValueLatexEqnArrayModel(ValueModel):
    def __init__(self, source:str):
        self.source = source

    @staticmethod
    def getDonuType():
        return Donu.modelTypeLEQArr()

class ValueMap(Value):
    def __init__(self, value:Dict[str,Value]):
        self.value = value


class ASKEECommandInterpreter:
    def __init__(self):
        self.env = {}
        self.unit = ValueUnit()
        self.donu = Donu(donuAddress)

        self.builtins = {
            "loadCSV": self.loadCSV,
            "loadDiffEq": self.loadDiffEq,
            "plot": self.plot,
            "simulateODE": self.simulate,
            "loadESL": self.loadESL,
            "loadLatexEqnArray": self.loadLatexEqnArray,
            "loadReactionNet": self.loadReactionNet,
            "asEquationSystem": self.asEquationSystem,
            "asESL": self.asESL,
            "generateSimulator": self.generateSimulator,
            "save": self.saveAsFile,
            "scatter":self.scatterPlot,
            "stratifySpatial":self.stratifySpatial,
            "setParams":self.setParams,
            "asEqnArray":self.asEqnArray,
        }

    def setParams(self, call:ExprCall, output:List[Dict]) -> Value:
        [model, mp] = self.evalArgs(call, [ValueESLModel, ValueMap], output)
        if(any([not isinstance(v, ValueNumber) for v in mp.value.values()])):
            self.fail("All map values must be numeric for 'setParams'", call)

        imp = dict([(k,v.value) for (k,v) in mp.value.items()])

        result = self.donu.setParams(model.source, imp, call)
        return ValueESLModel(result)

    def stratifySpatial(self, call:ExprCall, output:List[Dict]) -> Value:
        [model, cgraph] = self.evalArgs(call, [ValueESLModel, ValueString], output)
        stratResult = self.donu.stratifySpatial(model.source, cgraph.value, call)
        return ValueString(stratResult)
        # return ValueString(stratResult)


    def scatterPlot(self, call:ExprCall, output:List[Dict]) -> Value:
        [data, xaxisVal, yaxisVal] = self.evalArgs(call, [ValueDataSeries, ValueString, ValueString], output)
        xaxis = xaxisVal.value
        yaxis = yaxisVal.value
        xs = data.data[xaxis]
        ys = data.data[yaxis]
        points = [{xaxis: xval, yaxis: yval} for (xval, yval) in zip(xs, ys) ]
        chart = {
            "$schema": vega_lite_schema,
            "description": "",
            "data": {"values": points},
            "mark": "point",
            "encoding": {
                "x": {"field": xaxis, "type": "quantitative"},
                "y": {"field": yaxis, "type": "quantitative"}
            }
        }

        self.outputChart(chart, output)
        return self.unit

    # TODO: this should check that paths don't escape the jupyter env
    def saveAsFile(self, call:ExprCall, output:List[Dict]) -> Value:
        [path, value] = self.evalArgs(call, [ValueString, Value], output)
        if isinstance(value, ValueString):
            with open(path.value, "w") as handle:
                handle.write(value.value)

            return self.unit

        self.fail("saving not implemented for this value -- yet", call)

    def loadDiffEq(self, call:ExprCall, output:List[Dict]) -> Value:
        [path] = self.evalArgs(call, [ValueString], output)
        with open(path.value) as f:
            return ValueDiffeqModel(f.read())

    def simulate(self, call:ExprCall, output:List[Dict]) -> Value:
        [model, start, end, by] = self.evalArgs(call, [ValueModel, ValueNumber, ValueNumber, ValueNumber], output)
        # XXX: overloading!
        if isinstance(model, ValueESLModel):
            return self.donu.deqSimulate(model.source, Donu.modelTypeESL(), start.value, end.value, by.value)

        elif isinstance(model, ValueDiffeqModel):
            return self.donu.deqSimulate(model.source, Donu.modelTypeDEQ(), start.value, end.value, by.value)

    def loadCSV(self, call:ExprCall, output:List[Dict]) -> Value:
        [filename] = self.evalArgs(call, [ValueString], output)
        return self.loadSeriesFromFile(filename.value)

    def loadESL(self, call:ExprCall, output:List[Dict]) -> Value:
        [filename] = self.evalArgs(call, [ValueString], output)
        return self.loadESLModelFromFile(filename.value, call)

    def loadReactionNet(self, call:ExprCall, output:List[Dict]) -> Value:
        [filename] = self.evalArgs(call, [ValueString], output)
        return self.loadReactionNetFromFile(filename.value, call)

    def loadLatexEqnArray(self, call:ExprCall, output:List[Dict]) -> Value:
        [filename] = self.evalArgs(call, [ValueString], output)
        with open(filename.value) as handle:
            source = handle.read()
            self.donu.checkModel(source, Donu.modelTypeLEQArr(), call)
            return ValueLatexEqnArrayModel(source)

    def asEqnArray(self, call:ExprCall, output:List[Dict]) -> Value:
        [model] = self.evalArgs(call, [ValueModel], output)
        if isinstance(model, ValueLatexEqnArrayModel):
            return model

        model = self.donu.convertModel(model.source, model.getDonuType(), Donu.modelTypeLEQArr(), call)
        return ValueLatexEqnArrayModel(model)

    def asEquationSystem(self, call:ExprCall, output:List[Dict]) -> Value:
        [model] = self.evalArgs(call, [ValueModel], output)
        if isinstance(model, ValueESLModel):
            result = self.donu.convertModel(model.source, Donu.modelTypeESL(), Donu.modelTypeDEQ(), call)
            return ValueDiffeqModel(result)

        if isinstance(model, ValueLatexEqnArrayModel):
            result = self.donu.convertModel(model.source, Donu.modelTypeLEQArr(), Donu.modelTypeDEQ(), call)
            return ValueDiffeqModel(result)

        elif isinstance(model, ValueESLModel):
            return model

        # TODO: a bit of a hack
        elif isinstance(model, ValueRNetModel):
            resultESL = self.donu.convertModel(model.source, Donu.modelTypeRNet(), Donu.modelTypeESL(), call)
            result = self.donu.convertModel(resultESL, Donu.modelTypeESL(), Donu.modelTypeDEQ(), call)
            return ValueDiffeqModel(result)

        self.fail("Conversion not implemented", call)

    def asESL(self, call:ExprCall, output:List[Dict]) -> Value:
        [model] = self.evalArgs(call, [Value], output)
        if isinstance(model, ValueRNetModel):
            result = self.donu.convertModel(model.source, Donu.modelTypeRNet(), Donu.modelTypeESL(), call)
            return ValueESLModel(result)

        elif isinstance(model, ValueESLModel):
            return model

    def generateSimulator(self, call:ExprCall, output:List[Dict]) -> Value:
        [model] = self.evalArgs(call, [ValueESLModel], output)
        sim = self.donu.genCpp(model.source, Donu.modelTypeESL(), call)
        return ValueCode(sim, "C++")

    def outputChart(self, chart, output):
        msg = {
            "application/vnd.vegalite.v4+json": chart,
            "text/plain": "Plot could not be displayed"
        }

        output.append(msg)


    def plot(self, call:ExprCall, output:List[Dict]):
        [series, xaxis] = self.evalArgs(call, [ValueDataSeries, ValueString], output)
        points = self.seriesVs(series, xaxis.value, call)
        xyc = [{"x":x, "y":y, "c": c } for (x,y,c) in points]

        chart = {
            "$schema": vega_lite_schema,
            "description": "Plot [TODO: real description]",
            "data": {"values": list(xyc)},
            "mark": "line",
            "encoding": {
                "x": {"field": "x", "type": "quantitative"},
                "y": {"field": "y", "type": "quantitative"},
                "color": {"field": "c", "type": "nominal"}
            }
        }

        self.outputChart(chart, output)

        return self.unit


    def fail(self, msg:str, why:AST) -> any:
        raise IntepreterError(msg, why)

    def getVar(self, nm:str, why:AST) -> Value:
        if nm in self.env:
            return self.env[nm]
        else:
            self.fail(f"Undefined variable '{nm}'", why)

    def evalTo(self, expr: Expr, ty: type, output: List[Dict]) -> Value:
        val = self.evalExpr(expr, output)
        # self.outputString(f"evals to {val}", output)
        if not isinstance(val, ty):
            # better error message
            self.fail("Expression has unexpected type", expr)
        return val

    def evalArgs(self, call:ExprCall, tys: List[type], output: List[Dict]) -> List[Value]:
        if len(call.args) != len(tys):
            self.fail(f"Function '{call.functionName}' called with {len(call.args)} arguments but takes {len(tys)}", call)

        return list([self.evalTo(e, t, output) for (e, t) in zip(call.args, tys)])

    def transpose(self, data):
        maxrow = min([len(l) for l in data])
        for row in range(0, maxrow):
            yield [series[row] for series in data]

    def mkTableRow(self, elts):
        return "|" + "|".join([str(s) for s in elts]) + "|"

    def seriesVs(self, data:ValueDataSeries, xaxis:str, why:AST) -> Generator[Tuple[float, float, str], None, None]:
        if xaxis not in data.data:
            self.fail(why, f"Data series has no field named '{xaxis}'")

        for point in self.seriesAsPoints(data):
            x = point[xaxis]
            for (k, v) in point.items():
                if k == xaxis:
                    continue

                yield (x, v, k)

    def seriesAsPoints(self, data:ValueDataSeries) -> Generator[Dict[str, float], None, None]:
        i = 0
        while True:
            point = collections.OrderedDict()
            for (k, vals) in data.data.items():
                if i < len(vals):
                    point[k] = vals[i]
                else:
                    return

            yield point
            i = i + 1

    def mkTable(self, headers, data):
        hdrRow = self.mkTableRow(headers)
        hdrSepRow = self.mkTableRow(["---" for _ in headers])
        # TODO: quoting
        rowsLst = [hdrRow, hdrSepRow] + [self.mkTableRow(row) for row in data]
        return { "text/markdown": "\n".join(rowsLst), "text/plain": "table could not be displayed" }

    def loadSeriesFromFile(self, filename:str) -> ValueDataSeries:
        series = collections.OrderedDict()
        with open(filename, newline='') as f:
            reader = csv.DictReader(f)
            for row in reader:
                for k in row.keys():
                    if k not in series:
                        series[k] = []
                    series[k].append(row[k])

        return ValueDataSeries(series)

    def loadESLModelFromFile(self, filename:str, why:AST) -> Value:
        with open(filename, newline='') as f:
            source = f.read()
            self.donu.checkModel(source, Donu.modelTypeESL(), why)
            return ValueESLModel(source)

    def loadReactionNetFromFile(self, filename:str, why:AST) -> Value:
        with open(filename, newline='') as f:
            source = f.read()
            self.donu.checkModel(source, Donu.modelTypeRNet(), why)
            return ValueRNetModel(source)

    def callFn(self, call:ExprCall, output:List[Dict]) -> Value:
        if call.functionName in self.builtins:
            return self.builtins[call.functionName](call, output)

        self.fail(f"Unknown function '{call.functionName}'", call)

    def outputString(self, string:str, output:List[Dict]):
        output.append({ "text/plain": string })

    def outputVal(self, value: Value, output:List[Dict]):
        if value is ValueUnit:
            return
        elif isinstance(value, ValueString):
            if isinstance(value, ValueCode):
                aug_sim = "```C++\n" + value.value + "\n```\n"
                output.append({"text/markdown": aug_sim, "text/plain": value.value })
            else:
                self.outputString(value.value, output)
        elif isinstance(value, ValueNumber):
            self.outputString(str(value.value), output)
        elif isinstance(value, ValueDataSeries):
            headers = value.data.keys()
            rows = self.transpose(value.data.values())
            output.append(self.mkTable(headers, rows))
        elif isinstance(value, ValueDiffeqModel):
            self.outputString(value.source, output)
        elif isinstance(value, ValueESLModel):
            self.outputString(value.source, output)
        elif isinstance(value, ValueRNetModel):
            self.outputString(value.source, output)
        elif isinstance(value, ValueLatexEqnArrayModel):
            output.append({"text/latex": value.source, "text/plain":value.source})

    def evalExpr(self, expr:Expr, output: List[Dict]) -> Value:
        if isinstance(expr, ExprNumber):
            return ValueNumber(expr.value)
        elif isinstance(expr, ExprString):
            return ValueString(expr.value)
        elif isinstance(expr, ExprVar):
            return self.getVar(expr.name, expr)
        elif isinstance(expr, ExprCall):
            return self.callFn(expr, output)
        elif isinstance(expr, ExprMap):
            entries = [(k, self.evalExpr(subExpr, output)) for (k, subExpr) in expr.elts]
            return ValueMap(dict(entries))

        else:
            self.fail("Expression form not implemented", expr)

    def evalStmt(self, stmt:Stmt, output:List[Dict]):
        if isinstance(stmt, StmtAssign):
            self.env[stmt.name] = self.evalExpr(stmt.expr, output)
        elif isinstance(stmt, StmtEval):
            val = self.evalExpr(stmt.expr, output)
            self.outputVal(val, output)
        else:
            self.fail(f"Statement form not implemented: {stmt}", stmt)





# -----------------------------------------------------------------------------
#  Kernel
# -----------------------------------------------------------------------------

class ASKEEKernel(Kernel):
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

    interpreter = ASKEECommandInterpreter()

    def outputErr(self, msg: str):
        stream_content = {'name': 'stdout', 'text': msg}
        self.send_response(self.iopub_socket, 'stream', stream_content)

    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):
        try:
            parsed = parse_command(code)
            outputs = []
            self.interpreter.evalStmt(parsed, outputs)

            if not silent:
                for output in outputs:
                    self.send_response(self.iopub_socket, "display_data", {"data": output, "metadata": {}})

            return {
                'status': 'ok',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
            }

        except Exception as e:
            tb = traceback.format_exc()
            self.outputErr(tb)
            return {
                'status': 'error',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
            }

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=ASKEEKernel)