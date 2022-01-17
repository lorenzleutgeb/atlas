grammar Splay;

import Annotation;

options {
    language = Java;
}

program: (func)* EOF;

// Function declaration (with list of arguments):
func : signature? name=IDENTIFIER (args+=IDENTIFIER)* ASSIGN body=expression SEMICOLON?;

signature : name=IDENTIFIER DOUBLECOLON (constraints DOUBLE_ARROW)? from=noParenProduct ARROW to=type (PIPE annotatedAnnotation=combinedFunctionAnnotation)? ;

combinedFunctionAnnotation : SQUARE_OPEN with=functionAnnotation (COMMA CURLY_OPEN (without+=functionAnnotation (COMMA without+=functionAnnotation)*)? CURLY_CLOSE)? SQUARE_CLOSE ;

functionAnnotation : from=annotation ARROW to=annotation ;

type : IDENTIFIER
     | predefinedType
     | PAREN_OPEN items+=type (CROSS items+=type)* PAREN_CLOSE
     ;

noParenProduct : items+=type (CROSS items+=type)*;

variableType : IDENTIFIER ;

predefinedType : BOOL
               | treeType
               ;

treeType : TREE variableType ;

typeClass : TYC_EQ | TYC_ORD ;

constraint : typeClass variableType
           | typeClass PAREN_OPEN treeType PAREN_CLOSE
           ;

constraints : items+=constraint
            | PAREN_OPEN items+=constraint (COMMA items+=constraint)* PAREN_CLOSE
            ;

// Expressions
expression : identifier # variableExpression
           | IF condition=expression THEN truthy=expression ELSE falsy=expression # iteExpression
           | MATCH test=expression WITH (PIPE LEAF ARROW leafCase=expression)? PIPE nodePattern=pattern ARROW nodeCase=expression # matchExpression
           | MATCH test=expression WITH PIPE tuplePattern ARROW body=expression # matchTupleExpression
           | NODE left=expression middle=expression right=expression # nodeExpression
           | name=IDENTIFIER (params+=expression)* # callExpression
           | LET name=identifier ASSIGN value=expression IN body=expression # letExpression
           | PAREN_OPEN expression PAREN_CLOSE # parenthesizedExpression
           | NUMBER # constant
           | left=expression op right=expression # comparison
           | TILDE (numerator=NUMBER)? (denominator=NUMBER)? expression # tickExpression
           | UNDERSCORE # holeExpression
           | PAREN_OPEN items+=expression (COMMA items+=expression)+ PAREN_CLOSE # tuple
           ;

op : EQ | NE | LT | LE | GT | GE ;

// For patterns we only admit simpler tuples:
//  - non-recursive, i.e. it is only possible to match one level of a tree
//  - no derived identifiers
pattern : NODE left=maybeAnonymousIdentifier middle=maybeAnonymousIdentifier right=maybeAnonymousIdentifier # deconstructionPattern
        | maybeAnonymousIdentifier # aliasingPattern
        ;

tuplePattern : PAREN_OPEN items+=identifier (COMMA items+=identifier)* PAREN_CLOSE ;

identifier : LEAF | IDENTIFIER ;

maybeAnonymousIdentifier : IDENTIFIER | UNDERSCORE ;

TILDE : '~';
UNDERSCORE : '_';
DOT : '.';
COMMA : ',';
DOUBLECOLON : '::' | '∷';
SEMICOLON : ';';
PIPE : '|';
PLUS : '+';
MINUS : '-';
CROSS : '*' | '⨯' ;
// DIV : '/';
QUOTE : '"';

PAREN_OPEN : '(';
PAREN_CLOSE : ')';
SQUARE_OPEN : '[';
SQUARE_CLOSE : ']';
CURLY_OPEN : '{';
CURLY_CLOSE : '}';

EQ : '==' | '⩵';
NE : '!=' | '≠';
LT : '<';
LE : '<=' | '≤';
GT : '>';
GE : '>=' | '≥';

ASSIGN : '=' | '≔';
NODE : 'node';
LEAF : 'leaf';
IN : 'in';
IF : 'if';
THEN : 'then';
ELSE : 'else';
ARROW : '->' | '→';
DOUBLE_ARROW : '=>' | '⇒';
MATCH : 'match';
WITH : 'with';
LET : 'let';
TREE : 'Tree' | 'T';
BOOL : 'Bool' | 'Bo';
TYC_ORD : 'Ord';
TYC_EQ : 'Eq';

IDENTIFIER : (('A' .. 'Z' | 'a'..'z' | 'α' | 'β' | 'γ' | 'δ' | 'ε' | '∂') ( 'A'..'Z' | 'a'..'z' | 'α' | 'β' | 'γ' | 'δ' | 'ε' | '∂' | '0'..'9' | '_' | '.' )*) ;
TYPE : ('A'..'Z') ( 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' )*;
NUMBER : '0' | ('1'..'9') ('0'..'9')*;
QUOTED_STRING : QUOTE ( '\\"' | . )*? QUOTE;

// ML-style nested comments.
COMMENT : '(*' (COMMENT|.)*? '*)' -> channel(HIDDEN) ;
LINE_COMMENT  : '(*)' .*? '\n' -> channel(HIDDEN) ;

BLANK : [ \t\r\n\f]+ -> channel(HIDDEN);
