grammar Tactic;

import Annotation;

options {
    language = Java;
}

tactic : tacticExpression EOF;

tacticExpression : PAREN_OPEN elements+=tacticExpression* PAREN_CLOSE # listTacticExpression
                 | PAREN_OPEN EXCLAMATION_MARK CURLY_OPEN from=annotation ARROW to=annotation CURLY_CLOSE next=tacticExpression PAREN_CLOSE # fixedAnnotation
                 | identifier=IDENTIFIER # terminalTacticExpression
                 | name=IDENTIFIER AT tacticExpression # namedTacticExpression
                 ;

NUMBER : ('0' .. '9' );
ZERO : '0';
SLASH : '/' ;
TIMES : '*';
CURLY_OPEN : '{';
CURLY_CLOSE : '}';
SQUARE_OPEN : '[';
SQUARE_CLOSE : ']';
SEMICOLON : ';';
/*UNDERSCORE : '_';*/
COMMA : ',';
EXCLAMATION_MARK : '!';
ARROW : '->';
MAPSTO : '↦' | '|->';

IDENTIFIER : ('a'..'z' | 'A' .. 'Z' | '_' | '?') ('a'..'z' | 'A' .. 'Z' | '0' .. '9' | ':' | '_' | ',' | '{' | '}')* ;

AT : '@';

PAREN_OPEN : '(';
PAREN_CLOSE : ')';

// ML-style nested comments.
COMMENT : '(*' (COMMENT|.)*? '*)' -> channel(HIDDEN) ;

BLANK : [ \t\r\n\f]+ -> channel(HIDDEN);
