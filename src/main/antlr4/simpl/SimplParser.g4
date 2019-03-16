grammar SimplParser;
type: 'int' | 'bool'
    | <assoc=right> type '->' type
    | '(' type ')'
    ;

formalArgs: (ID)*;
actualArgs: (expr) +;


letDef:  '{' type '}' ID '=' expr
    ;
letDefs: (letDef)+
    ;

expr:   '~' expr
    |   expr ('*'|'/') expr
    |   expr ('+'|'-') expr
    |   expr ('<'|'<='|'>'|'>=') expr
    |   expr ('='|'!=') expr
    |   '!' expr
    |   expr '&' expr
    |   expr '|' expr
    |   INT
    |   BOOL
    |   IF expr THEN expr ELSE expr END
    |   '(' expr actualArgs ')'
    |   FUN '{' type '}' formalArgs '->' expr END
    |   RECFUN ID '{' type '}' formalArgs '->' expr END
    |   LET letDefs IN '{' type '}' expr END
    |   ID
    |   '(' expr ')'
    ;


IF      : 'if';
THEN    : 'then';
ELSE    : 'else';
FUN     : 'fun';
RECFUN  : 'recfun';
LET     : 'let';
IN      : 'in';
END     : 'end';
WH      : [ \t\n]+ -> skip;
INT     : [0-9]+ ;
BOOL    : 'true'|'false' ;
ID      : [a-zA-Z_][a-zA-Z0-9_]*;