grammar Splay;

options {
    language = Java;
}

//@header {
//}

// Parser

program: (func)* EOF;

// Function declaration (with list of arguments):
func: name=VARIABLE (args+=VARIABLE)* EQUAL body=expression ;

// Expressions
expression : IF condition THEN expression ELSE expression # iteExpression
           | MATCH expression WITH cases=matchCase+ # matchExpression
           | variable # variableExpression
           | constant # constantExpression
           | tuple # tupleExpression
           | name=VARIABLE (params+=VARIABLE)* # callExpression
           ;

condition : expression binop expression ;

constant : integer | nil;
variable : VARIABLE ;
integer : NUMBER ;
nil : NIL ;

pattern : constant | variable | tuple ;

tuple : PAREN_OPEN elements+=tuple_element? (COMMA elements+=tuple_element)* PAREN_CLOSE ;

tuple_element : tuple | variable | constant ;

matchCase : OR pattern ARROW expression ;

binop : EQUAL | UNEQUAL | LESS | GREATER | LESS_OR_EQ | GREATER_OR_EQ;

// Lexer

ANONYMOUS_VARIABLE : '_';
DOT : '.';
COMMA : ',';
COLON : ':';
SEMICOLON : ';';
OR : '|';
PLUS : '+';
MINUS : '-';
TIMES : '*';
DIV : '/';
POWER : '**';
MODULO : '\\';
BITXOR : '^';
AT : '@';
SHARP : '#';
AMPERSAND : '&';
QUOTE : '"';

PAREN_OPEN : '(';
PAREN_CLOSE : ')';
SQUARE_OPEN : '[';
SQUARE_CLOSE : ']';
CURLY_OPEN : '{';
CURLY_CLOSE : '}';
EQUAL : '=';
UNEQUAL : '<>' | '!=';
LESS : '<';
GREATER : '>';
LESS_OR_EQ : '<=';
GREATER_OR_EQ : '>=';

IF : 'if';
THEN : 'then';
ELSE : 'else';
ARROW : '->';
MATCH : 'match';
WITH : 'with';
NIL : 'nil';

VARIABLE : ('a'..'z') ( 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' )*;
TYPE : ('A'..'Z') ( 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' )*;
NUMBER : '0' | ('1'..'9') ('0'..'9')*;
QUOTED_STRING : QUOTE ( '\\"' | . )*? QUOTE;

COMMENT : '%' ~[\r\n]* -> channel(HIDDEN);
MULTI_LINE_COMMEN : '%*' .*? '*%' -> channel(HIDDEN);
BLANK : [ \t\r\n\f]+ -> channel(HIDDEN);

