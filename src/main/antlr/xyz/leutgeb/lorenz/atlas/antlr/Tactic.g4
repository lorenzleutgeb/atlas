grammar Tactic;

import Annotation;

options {
    language = Java;
}

tactic : tacticExpressionOrList EOF;

tacticExpression : (applicationName=IDENTIFIER AT)? EXCLAMATION_MARK CURLY_OPEN from=annotation ARROW to=annotation CURLY_CLOSE # fix
                 | (applicationName=IDENTIFIER AT)? ruleName=(IDENTIFIER | UNDERSCORE | QUESTION_MARK) (CURLY_OPEN arguments+=argumentMapEntry+ CURLY_CLOSE)? # rule
                 ;

tacticExpressionOrList : PAREN_OPEN first=tacticExpression elements+=tacticExpressionOrList+ PAREN_CLOSE # list
                       | tacticExpression # immediate
                       ;

// For rules that take arguments, this provides some structure.
argumentMapEntry : key=IDENTIFIER (EQUALS value=(IDENTIFIER|NUMBER))? ;

NUMBER : ('0' .. '9' );
ZERO : '0';
EQUALS : '=';
DIV : '/' ;
TIMES : '*';
CURLY_OPEN : '{';
CURLY_CLOSE : '}';
SQUARE_OPEN : '[';
SQUARE_CLOSE : ']';
PAREN_OPEN : '(';
PAREN_CLOSE : ')';
SEMICOLON : ';';
COMMA : ',';
EXCLAMATION_MARK : '!';
QUESTION_MARK : '?';
ARROW : '->';
MAPSTO : '↦' | '|->';
UNDERSCORE : '_';

IDENTIFIER : ('a'..'z' | 'A'..'Z') ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' | ':')* ;

AT : '@';

// ML-style nested comments.
COMMENT : '(*' (COMMENT|.)*? '*)' -> channel(HIDDEN) ;

BLANK : [ \t\r\n\f]+ -> channel(HIDDEN);
