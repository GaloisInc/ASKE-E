# Generated from ASKEECommand.g4 by ANTLR 4.9
from antlr4 import *
from io import StringIO
from typing.io import TextIO
import sys



def serializedATN():
    with StringIO() as buf:
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\n")
        buf.write("G\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7")
        buf.write("\4\b\t\b\4\t\t\t\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\6")
        buf.write("\6\35\n\6\r\6\16\6\36\3\6\3\6\6\6#\n\6\r\6\16\6$\5\6\'")
        buf.write("\n\6\3\6\3\6\6\6+\n\6\r\6\16\6,\5\6/\n\6\3\7\3\7\7\7\63")
        buf.write("\n\7\f\7\16\7\66\13\7\3\7\3\7\3\b\3\b\7\b<\n\b\f\b\16")
        buf.write("\b?\13\b\3\t\6\tB\n\t\r\t\16\tC\3\t\3\t\2\2\n\3\3\5\4")
        buf.write("\7\5\t\6\13\7\r\b\17\t\21\n\3\2\7\3\2\62;\3\2$$\5\2C\\")
        buf.write("aac|\6\2\62;C\\aac|\5\2\13\f\17\17\"\"\2N\2\3\3\2\2\2")
        buf.write("\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r")
        buf.write("\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\3\23\3\2\2\2\5\25\3")
        buf.write("\2\2\2\7\27\3\2\2\2\t\31\3\2\2\2\13.\3\2\2\2\r\60\3\2")
        buf.write("\2\2\179\3\2\2\2\21A\3\2\2\2\23\24\7?\2\2\24\4\3\2\2\2")
        buf.write("\25\26\7*\2\2\26\6\3\2\2\2\27\30\7.\2\2\30\b\3\2\2\2\31")
        buf.write("\32\7+\2\2\32\n\3\2\2\2\33\35\t\2\2\2\34\33\3\2\2\2\35")
        buf.write("\36\3\2\2\2\36\34\3\2\2\2\36\37\3\2\2\2\37&\3\2\2\2 \"")
        buf.write("\7\60\2\2!#\t\2\2\2\"!\3\2\2\2#$\3\2\2\2$\"\3\2\2\2$%")
        buf.write("\3\2\2\2%\'\3\2\2\2& \3\2\2\2&\'\3\2\2\2\'/\3\2\2\2(*")
        buf.write("\7\60\2\2)+\t\2\2\2*)\3\2\2\2+,\3\2\2\2,*\3\2\2\2,-\3")
        buf.write("\2\2\2-/\3\2\2\2.\34\3\2\2\2.(\3\2\2\2/\f\3\2\2\2\60\64")
        buf.write("\7$\2\2\61\63\n\3\2\2\62\61\3\2\2\2\63\66\3\2\2\2\64\62")
        buf.write("\3\2\2\2\64\65\3\2\2\2\65\67\3\2\2\2\66\64\3\2\2\2\67")
        buf.write("8\7$\2\28\16\3\2\2\29=\t\4\2\2:<\t\5\2\2;:\3\2\2\2<?\3")
        buf.write("\2\2\2=;\3\2\2\2=>\3\2\2\2>\20\3\2\2\2?=\3\2\2\2@B\t\6")
        buf.write("\2\2A@\3\2\2\2BC\3\2\2\2CA\3\2\2\2CD\3\2\2\2DE\3\2\2\2")
        buf.write("EF\b\t\2\2F\22\3\2\2\2\13\2\36$&,.\64=C\3\2\3\2")
        return buf.getvalue()


class ASKEECommandLexer(Lexer):

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    T__0 = 1
    T__1 = 2
    T__2 = 3
    T__3 = 4
    NUMBER = 5
    STRING = 6
    IDENTIFIER = 7
    WS = 8

    channelNames = [ u"DEFAULT_TOKEN_CHANNEL", u"HIDDEN" ]

    modeNames = [ "DEFAULT_MODE" ]

    literalNames = [ "<INVALID>",
            "'='", "'('", "','", "')'" ]

    symbolicNames = [ "<INVALID>",
            "NUMBER", "STRING", "IDENTIFIER", "WS" ]

    ruleNames = [ "T__0", "T__1", "T__2", "T__3", "NUMBER", "STRING", "IDENTIFIER", 
                  "WS" ]

    grammarFileName = "ASKEECommand.g4"

    def __init__(self, input=None, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.9")
        self._interp = LexerATNSimulator(self, self.atn, self.decisionsToDFA, PredictionContextCache())
        self._actions = None
        self._predicates = None


