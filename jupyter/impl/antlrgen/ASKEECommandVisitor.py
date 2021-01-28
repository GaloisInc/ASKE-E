# Generated from ASKEECommand.g4 by ANTLR 4.9
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .ASKEECommandParser import ASKEECommandParser
else:
    from ASKEECommandParser import ASKEECommandParser

# This class defines a complete generic visitor for a parse tree produced by ASKEECommandParser.

class ASKEECommandVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by ASKEECommandParser#StmtAssign.
    def visitStmtAssign(self, ctx:ASKEECommandParser.StmtAssignContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ASKEECommandParser#StmtEval.
    def visitStmtEval(self, ctx:ASKEECommandParser.StmtEvalContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ASKEECommandParser#ExprCall.
    def visitExprCall(self, ctx:ASKEECommandParser.ExprCallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ASKEECommandParser#ExprVar.
    def visitExprVar(self, ctx:ASKEECommandParser.ExprVarContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ASKEECommandParser#ExprString.
    def visitExprString(self, ctx:ASKEECommandParser.ExprStringContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ASKEECommandParser#ExprNumber.
    def visitExprNumber(self, ctx:ASKEECommandParser.ExprNumberContext):
        return self.visitChildren(ctx)



del ASKEECommandParser