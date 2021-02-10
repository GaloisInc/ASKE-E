grammar ASKEECommand;

stmt
  : IDENTIFIER '=' expr EOF                             #StmtAssign
  | expr                EOF                             #StmtEval
  ;

expr
  : IDENTIFIER '(' ((args+=expr ',')* args+=expr)? ')'  #ExprCall
  | IDENTIFIER                                          #ExprVar
  | STRING                                              #ExprString
  | NUMBER                                              #ExprNumber
  | '[' expr* ']'                                       #ExprList
  | '{' ((keys+=IDENTIFIER '=' values+=expr)
    (',' keys+=IDENTIFIER '=' values+=expr)*)? '}'      #ExprMap
  ;

NUMBER: ([0-9]+ ('.' [0-9]+)?) | ('.' [0-9]+) ;
STRING: '"' (~'"')* '"' ;
IDENTIFIER: [A-Za-z_][A-Za-z0-9_]* ;
WS: [ \n\t\r]+ -> channel(HIDDEN);
