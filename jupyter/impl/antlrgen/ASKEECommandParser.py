# Generated from ASKEECommand.g4 by ANTLR 4.9
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO


def serializedATN():
    with StringIO() as buf:
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\n")
        buf.write("$\4\2\t\2\4\3\t\3\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\5\2")
        buf.write("\17\n\2\3\3\3\3\3\3\3\3\3\3\7\3\26\n\3\f\3\16\3\31\13")
        buf.write("\3\3\3\5\3\34\n\3\3\3\3\3\3\3\3\3\5\3\"\n\3\3\3\2\2\4")
        buf.write("\2\4\2\2\2\'\2\16\3\2\2\2\4!\3\2\2\2\6\7\7\t\2\2\7\b\7")
        buf.write("\3\2\2\b\t\5\4\3\2\t\n\7\2\2\3\n\17\3\2\2\2\13\f\5\4\3")
        buf.write("\2\f\r\7\2\2\3\r\17\3\2\2\2\16\6\3\2\2\2\16\13\3\2\2\2")
        buf.write("\17\3\3\2\2\2\20\21\7\t\2\2\21\33\7\4\2\2\22\23\5\4\3")
        buf.write("\2\23\24\7\5\2\2\24\26\3\2\2\2\25\22\3\2\2\2\26\31\3\2")
        buf.write("\2\2\27\25\3\2\2\2\27\30\3\2\2\2\30\32\3\2\2\2\31\27\3")
        buf.write("\2\2\2\32\34\5\4\3\2\33\27\3\2\2\2\33\34\3\2\2\2\34\35")
        buf.write("\3\2\2\2\35\"\7\6\2\2\36\"\7\t\2\2\37\"\7\b\2\2 \"\7\7")
        buf.write("\2\2!\20\3\2\2\2!\36\3\2\2\2!\37\3\2\2\2! \3\2\2\2\"\5")
        buf.write("\3\2\2\2\6\16\27\33!")
        return buf.getvalue()


class ASKEECommandParser ( Parser ):

    grammarFileName = "ASKEECommand.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'='", "'('", "','", "')'" ]

    symbolicNames = [ "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                      "<INVALID>", "NUMBER", "STRING", "IDENTIFIER", "WS" ]

    RULE_stmt = 0
    RULE_expr = 1

    ruleNames =  [ "stmt", "expr" ]

    EOF = Token.EOF
    T__0=1
    T__1=2
    T__2=3
    T__3=4
    NUMBER=5
    STRING=6
    IDENTIFIER=7
    WS=8

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.9")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class StmtContext(ParserRuleContext):

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser


        def getRuleIndex(self):
            return ASKEECommandParser.RULE_stmt

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)



    class StmtEvalContext(StmtContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ASKEECommandParser.StmtContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def expr(self):
            return self.getTypedRuleContext(ASKEECommandParser.ExprContext,0)

        def EOF(self):
            return self.getToken(ASKEECommandParser.EOF, 0)

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterStmtEval" ):
                listener.enterStmtEval(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitStmtEval" ):
                listener.exitStmtEval(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitStmtEval" ):
                return visitor.visitStmtEval(self)
            else:
                return visitor.visitChildren(self)


    class StmtAssignContext(StmtContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ASKEECommandParser.StmtContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def IDENTIFIER(self):
            return self.getToken(ASKEECommandParser.IDENTIFIER, 0)
        def expr(self):
            return self.getTypedRuleContext(ASKEECommandParser.ExprContext,0)

        def EOF(self):
            return self.getToken(ASKEECommandParser.EOF, 0)

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterStmtAssign" ):
                listener.enterStmtAssign(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitStmtAssign" ):
                listener.exitStmtAssign(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitStmtAssign" ):
                return visitor.visitStmtAssign(self)
            else:
                return visitor.visitChildren(self)



    def stmt(self):

        localctx = ASKEECommandParser.StmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_stmt)
        try:
            self.state = 12
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,0,self._ctx)
            if la_ == 1:
                localctx = ASKEECommandParser.StmtAssignContext(self, localctx)
                self.enterOuterAlt(localctx, 1)
                self.state = 4
                self.match(ASKEECommandParser.IDENTIFIER)
                self.state = 5
                self.match(ASKEECommandParser.T__0)
                self.state = 6
                self.expr()
                self.state = 7
                self.match(ASKEECommandParser.EOF)
                pass

            elif la_ == 2:
                localctx = ASKEECommandParser.StmtEvalContext(self, localctx)
                self.enterOuterAlt(localctx, 2)
                self.state = 9
                self.expr()
                self.state = 10
                self.match(ASKEECommandParser.EOF)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExprContext(ParserRuleContext):

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser


        def getRuleIndex(self):
            return ASKEECommandParser.RULE_expr

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)



    class ExprVarContext(ExprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ASKEECommandParser.ExprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def IDENTIFIER(self):
            return self.getToken(ASKEECommandParser.IDENTIFIER, 0)

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExprVar" ):
                listener.enterExprVar(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExprVar" ):
                listener.exitExprVar(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExprVar" ):
                return visitor.visitExprVar(self)
            else:
                return visitor.visitChildren(self)


    class ExprCallContext(ExprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ASKEECommandParser.ExprContext
            super().__init__(parser)
            self._expr = None # ExprContext
            self.args = list() # of ExprContexts
            self.copyFrom(ctx)

        def IDENTIFIER(self):
            return self.getToken(ASKEECommandParser.IDENTIFIER, 0)
        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(ASKEECommandParser.ExprContext)
            else:
                return self.getTypedRuleContext(ASKEECommandParser.ExprContext,i)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExprCall" ):
                listener.enterExprCall(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExprCall" ):
                listener.exitExprCall(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExprCall" ):
                return visitor.visitExprCall(self)
            else:
                return visitor.visitChildren(self)


    class ExprStringContext(ExprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ASKEECommandParser.ExprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def STRING(self):
            return self.getToken(ASKEECommandParser.STRING, 0)

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExprString" ):
                listener.enterExprString(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExprString" ):
                listener.exitExprString(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExprString" ):
                return visitor.visitExprString(self)
            else:
                return visitor.visitChildren(self)


    class ExprNumberContext(ExprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ASKEECommandParser.ExprContext
            super().__init__(parser)
            self.copyFrom(ctx)

        def NUMBER(self):
            return self.getToken(ASKEECommandParser.NUMBER, 0)

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExprNumber" ):
                listener.enterExprNumber(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExprNumber" ):
                listener.exitExprNumber(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExprNumber" ):
                return visitor.visitExprNumber(self)
            else:
                return visitor.visitChildren(self)



    def expr(self):

        localctx = ASKEECommandParser.ExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_expr)
        self._la = 0 # Token type
        try:
            self.state = 31
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,3,self._ctx)
            if la_ == 1:
                localctx = ASKEECommandParser.ExprCallContext(self, localctx)
                self.enterOuterAlt(localctx, 1)
                self.state = 14
                self.match(ASKEECommandParser.IDENTIFIER)
                self.state = 15
                self.match(ASKEECommandParser.T__1)
                self.state = 25
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if (((_la) & ~0x3f) == 0 and ((1 << _la) & ((1 << ASKEECommandParser.NUMBER) | (1 << ASKEECommandParser.STRING) | (1 << ASKEECommandParser.IDENTIFIER))) != 0):
                    self.state = 21
                    self._errHandler.sync(self)
                    _alt = self._interp.adaptivePredict(self._input,1,self._ctx)
                    while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                        if _alt==1:
                            self.state = 16
                            localctx._expr = self.expr()
                            localctx.args.append(localctx._expr)
                            self.state = 17
                            self.match(ASKEECommandParser.T__2) 
                        self.state = 23
                        self._errHandler.sync(self)
                        _alt = self._interp.adaptivePredict(self._input,1,self._ctx)

                    self.state = 24
                    localctx._expr = self.expr()
                    localctx.args.append(localctx._expr)


                self.state = 27
                self.match(ASKEECommandParser.T__3)
                pass

            elif la_ == 2:
                localctx = ASKEECommandParser.ExprVarContext(self, localctx)
                self.enterOuterAlt(localctx, 2)
                self.state = 28
                self.match(ASKEECommandParser.IDENTIFIER)
                pass

            elif la_ == 3:
                localctx = ASKEECommandParser.ExprStringContext(self, localctx)
                self.enterOuterAlt(localctx, 3)
                self.state = 29
                self.match(ASKEECommandParser.STRING)
                pass

            elif la_ == 4:
                localctx = ASKEECommandParser.ExprNumberContext(self, localctx)
                self.enterOuterAlt(localctx, 4)
                self.state = 30
                self.match(ASKEECommandParser.NUMBER)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





