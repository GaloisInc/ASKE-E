# Generated from ASKEECommand.g4 by ANTLR 4.9
from antlr4 import *
from io import StringIO
from typing.io import TextIO
import sys



def serializedATN():
    with StringIO() as buf:
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\f")
        buf.write("O\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7")
        buf.write("\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\3\2\3\2\3\3\3\3\3\4")
        buf.write("\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3\b\6\b%\n\b\r\b\16\b&\3")
        buf.write("\b\3\b\6\b+\n\b\r\b\16\b,\5\b/\n\b\3\b\3\b\6\b\63\n\b")
        buf.write("\r\b\16\b\64\5\b\67\n\b\3\t\3\t\7\t;\n\t\f\t\16\t>\13")
        buf.write("\t\3\t\3\t\3\n\3\n\7\nD\n\n\f\n\16\nG\13\n\3\13\6\13J")
        buf.write("\n\13\r\13\16\13K\3\13\3\13\2\2\f\3\3\5\4\7\5\t\6\13\7")
        buf.write("\r\b\17\t\21\n\23\13\25\f\3\2\7\3\2\62;\3\2$$\5\2C\\a")
        buf.write("ac|\6\2\62;C\\aac|\5\2\13\f\17\17\"\"\2V\2\3\3\2\2\2\2")
        buf.write("\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3")
        buf.write("\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2")
        buf.write("\2\2\3\27\3\2\2\2\5\31\3\2\2\2\7\33\3\2\2\2\t\35\3\2\2")
        buf.write("\2\13\37\3\2\2\2\r!\3\2\2\2\17\66\3\2\2\2\218\3\2\2\2")
        buf.write("\23A\3\2\2\2\25I\3\2\2\2\27\30\7?\2\2\30\4\3\2\2\2\31")
        buf.write("\32\7*\2\2\32\6\3\2\2\2\33\34\7.\2\2\34\b\3\2\2\2\35\36")
        buf.write("\7+\2\2\36\n\3\2\2\2\37 \7]\2\2 \f\3\2\2\2!\"\7_\2\2\"")
        buf.write("\16\3\2\2\2#%\t\2\2\2$#\3\2\2\2%&\3\2\2\2&$\3\2\2\2&\'")
        buf.write("\3\2\2\2\'.\3\2\2\2(*\7\60\2\2)+\t\2\2\2*)\3\2\2\2+,\3")
        buf.write("\2\2\2,*\3\2\2\2,-\3\2\2\2-/\3\2\2\2.(\3\2\2\2./\3\2\2")
        buf.write("\2/\67\3\2\2\2\60\62\7\60\2\2\61\63\t\2\2\2\62\61\3\2")
        buf.write("\2\2\63\64\3\2\2\2\64\62\3\2\2\2\64\65\3\2\2\2\65\67\3")
        buf.write("\2\2\2\66$\3\2\2\2\66\60\3\2\2\2\67\20\3\2\2\28<\7$\2")
        buf.write("\29;\n\3\2\2:9\3\2\2\2;>\3\2\2\2<:\3\2\2\2<=\3\2\2\2=")
        buf.write("?\3\2\2\2><\3\2\2\2?@\7$\2\2@\22\3\2\2\2AE\t\4\2\2BD\t")
        buf.write("\5\2\2CB\3\2\2\2DG\3\2\2\2EC\3\2\2\2EF\3\2\2\2F\24\3\2")
        buf.write("\2\2GE\3\2\2\2HJ\t\6\2\2IH\3\2\2\2JK\3\2\2\2KI\3\2\2\2")
        buf.write("KL\3\2\2\2LM\3\2\2\2MN\b\13\2\2N\26\3\2\2\2\13\2&,.\64")
        buf.write("\66<EK\3\2\3\2")
        return buf.getvalue()


class ASKEECommandLexer(Lexer):

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    T__0 = 1
    T__1 = 2
    T__2 = 3
    T__3 = 4
    T__4 = 5
    T__5 = 6
    NUMBER = 7
    STRING = 8
    IDENTIFIER = 9
    WS = 10

    channelNames = [ u"DEFAULT_TOKEN_CHANNEL", u"HIDDEN" ]

    modeNames = [ "DEFAULT_MODE" ]

    literalNames = [ "<INVALID>",
            "'='", "'('", "','", "')'", "'['", "']'" ]

    symbolicNames = [ "<INVALID>",
            "NUMBER", "STRING", "IDENTIFIER", "WS" ]

    ruleNames = [ "T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "NUMBER", 
                  "STRING", "IDENTIFIER", "WS" ]

    grammarFileName = "ASKEECommand.g4"

    def __init__(self, input=None, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.9")
        self._interp = LexerATNSimulator(self, self.atn, self.decisionsToDFA, PredictionContextCache())
        self._actions = None
        self._predicates = None


