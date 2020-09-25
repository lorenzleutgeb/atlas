grammar Tactic;

options {
    language = Java;
}

tactic : tacticExpression EOF;

tacticExpression : PAREN_OPEN elements+=tacticExpression* PAREN_CLOSE # listTacticExpression
                | identifier=IDENTIFIER # terminalTacticExpression
                | annotation=IDENTIFIER AT tacticExpression # annotatedTacticExpression
                ;

IDENTIFIER : ('a'..'z' | 'A' .. 'Z' | '_' | '?') ('a'..'z' | 'A' .. 'Z' | '0' .. '9' | ':' | '_' | ',' | '{' | '}')* ;

AT : '@';

PAREN_OPEN : '(';
PAREN_CLOSE : ')';

// ML-style nested comments.
COMMENT : '(*' (COMMENT|.)*? '*)' -> channel(HIDDEN) ;
LINE_COMMENT  : '(*)' .*? '\n' -> channel(HIDDEN) ;

BLANK : [ \t\r\n\f]+ -> channel(HIDDEN);
