grammar Splay;

options {
    language = Java;
}

program: (func)* EOF;

// Function declaration (with list of arguments):
func: name=IDENTIFIER (args+=IDENTIFIER)* ASSIGN body=expression ;

// Expressions
expression : IDENTIFIER # identifier
           | IF condition THEN truthy=expression ELSE falsy=expression # iteExpression
           | MATCH expression WITH cases+=matchCase+ # matchExpression
           | tuple # tupleExpression
           | name=IDENTIFIER (params+=expression)* # callExpression
           | LET name=IDENTIFIER ASSIGN value=expression IN body=expression # letExpression
           ;

// Conditions are expressed by comparing two expressions.
condition : left=expression op right=expression ;
op : EQ | NE | LT | LE | GT | GE ;

matchCase : OR pattern ARROW expression ;

pattern : IDENTIFIER | tuple ;

tuple: PAREN_OPEN elements+=tupleElement? (COMMA elements+=tupleElement)* PAREN_CLOSE ;
tupleElement: tuple | IDENTIFIER ;

ANONYMOUS_IDENTIFIER : '_';
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
ARROW : '->';
MATCH : 'match';
WITH : 'with';
LET : 'let';

IDENTIFIER : ('a'..'z') ( 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' )*;
TYPE : ('A'..'Z') ( 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' )*;
NUMBER : '0' | ('1'..'9') ('0'..'9')*;
QUOTED_STRING : QUOTE ( '\\"' | . )*? QUOTE;

COMMENT : '%' ~[\r\n]* -> channel(HIDDEN);
MULTI_LINE_COMMEN : '%*' .*? '*%' -> channel(HIDDEN);
BLANK : [ \t\r\n\f]+ -> channel(HIDDEN);

