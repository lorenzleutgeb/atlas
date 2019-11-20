grammar Splay;

options {
    language = Java;
}

program: (func)* EOF;

// Function declaration (with list of arguments):
func: name=IDENTIFIER (args+=IDENTIFIER)* ASSIGN body=expression SEMICOLON?;

// Expressions
expression : (IDENTIFIER | DERIVED_IDENTIFIER) # identifier
           | IF condition THEN truthy=expression ELSE falsy=expression # iteExpression
           | MATCH expression WITH cases+=matchCase+ # matchExpression
           | tuple # tupleExpression
           | name=IDENTIFIER (params+=expression)* # callExpression
           | LET name=(IDENTIFIER | DERIVED_IDENTIFIER) ASSIGN value=expression IN body=expression # letExpression
           | PAREN_OPEN expression PAREN_CLOSE # parenthesizedExpression
           | NUMBER # constant
           ;

// Conditions are expressed by comparing two expressions.
condition : left=expression op right=expression # comparison | expression # booleanExpression ;
op : EQ | NE | LT | LE | GT | GE ;

matchCase : OR pattern ARROW expression ;

// For match expression we only admit simpler tuples:
//  - non-recursive, i.e. it is only possible to match one level of a tree
//  - no derived identifiers
//  - anonymous identifiers are allowed
pattern : IDENTIFIER | patternTuple ;
patternTuple : PAREN_OPEN left=IDENTIFIER COMMA middle=IDENTIFIER COMMA right=IDENTIFIER PAREN_CLOSE;

// The following definition of tuples is much more liberal.
tuple: PAREN_OPEN left=expression COMMA middle=expression COMMA right=expression PAREN_CLOSE ;

// ANONYMOUS_IDENTIFIER : '_';
DOT : '.';
COMMA : ',';
COLON : ':';
SEMICOLON : ';';
OR : '|';
PLUS : '+';
MINUS : '-';
TIMES : '*';
DIV : '/';
QUOTE : '"';

PAREN_OPEN : '(';
PAREN_CLOSE : ')';
SQUARE_OPEN : '[';
SQUARE_CLOSE : ']';
CURLY_OPEN : '{';
CURLY_CLOSE : '}';

EQ : '==';
NE : '!=';
LT : '<';
LE : '<=';
GT : '>';
GE : '>=';

ASSIGN : '=';
IN : 'in';
IF : 'if';
THEN : 'then';
ELSE : 'else';
ARROW : '->' | '→' ;
MATCH : 'match';
WITH : 'with';
LET : 'let';

// TODO(lorenz.leutgeb): Add greek alphabet and lowercase letters.

SUBSCRIPT_NUMBER : [\u2080-\u2089];
DERIVED_IDENTIFIER : '∂' SUBSCRIPT_NUMBER+;
IDENTIFIER : (('A' .. 'Z' | 'a'..'z' | '∂') ( 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' | '.' )*);
TYPE : ('A'..'Z') ( 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' )*;
NUMBER : '0' | ('1'..'9') ('0'..'9')*;
QUOTED_STRING : QUOTE ( '\\"' | . )*? QUOTE;

// ML-style nested comments.
COMMENT : '(*' (COMMENT|.)*? '*)' -> channel(HIDDEN) ;
LINE_COMMENT  : '(*)' .*? '\n' -> channel(HIDDEN) ;

BLANK : [ \t\r\n\f]+ -> channel(HIDDEN);
