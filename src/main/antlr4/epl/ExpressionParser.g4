grammar ExpressionParser;
expr:   '-' expr
    |   expr ('*'|'/') expr
    |   expr ('+'|'-') expr
    |   expr ('<'|'<='|'>'|'>=') expr
    |   expr ('=='|'!=') expr
    |   '!' expr
    |   expr '&' expr
    |   expr '|' expr
    |   INT
    |   BOOL
    |   '(' expr ')'
    ;
WH      : [ \t]+ -> skip;
INT     : [0-9]+ ;
BOOL    : 'true'|'false' ;