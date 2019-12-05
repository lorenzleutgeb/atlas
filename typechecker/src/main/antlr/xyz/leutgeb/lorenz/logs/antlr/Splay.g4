grammar Splay;

options {
    language = Java;
}

program: (func)* EOF;

// Function declaration (with list of arguments):
func : signature? name=IDENTIFIER (args+=IDENTIFIER)* ASSIGN body=expression SEMICOLON?;

signature : name=IDENTIFIER DOUBLECOLON (constraints DOUBLE_ARROW)? from=productType ARROW to=namedType;

variableType : IDENTIFIER ;

treeType : TREE variableType ;

// NOTE: This does NOT include product types!
constructedType : variableType
                | treeType
                ;

predefinedType : BOOL ;

unnamedType : constructedType | predefinedType ;

// Optionally named types.
namedType : (IDENTIFIER DOUBLECOLON)? unnamedType ;

typeClass : TYC_EQ | TYC_ORD;

constraint : typeClass variableType
           | typeClass PAREN_OPEN treeType PAREN_CLOSE
           ;

constraints : items+=constraint
            | PAREN_OPEN items+=constraint (COMMA items+=constraint)* PAREN_CLOSE
            ;

productType : (productTypeNaming DOUBLECOLON)? PAREN_OPEN items+=unnamedType (CROSS items+=unnamedType)* PAREN_CLOSE
            | (productTypeNaming DOUBLECOLON)? items+=unnamedType (CROSS items+=unnamedType)*
            ;

productTypeNaming : PAREN_OPEN names+=IDENTIFIER (COMMA names+=IDENTIFIER) PAREN_CLOSE ;

// Expressions
expression : (IDENTIFIER | DERIVED_IDENTIFIER) # identifier
           | IF condition=expression THEN truthy=expression ELSE falsy=expression # iteExpression
           | MATCH expression WITH cases+=matchCase+ # matchExpression
           | tuple # tupleExpression
           | name=IDENTIFIER (params+=expression)* # callExpression
           | LET name=(IDENTIFIER | DERIVED_IDENTIFIER) ASSIGN value=expression IN body=expression # letExpression
           | PAREN_OPEN expression PAREN_CLOSE # parenthesizedExpression
           | NUMBER # constant
           | left=expression op right=expression # comparison
           ;

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
DOUBLECOLON : '::' | '∷';
SEMICOLON : ';';
OR : '|';
PLUS : '+';
MINUS : '-';
CROSS : '*' | '⨯' ;
DIV : '/';
QUOTE : '"';

PAREN_OPEN : '(';
PAREN_CLOSE : ')';
SQUARE_OPEN : '[';
SQUARE_CLOSE : ']';
CURLY_OPEN : '{';
CURLY_CLOSE : '}';

EQ : '==';
NE : '!=' | '≠';
LT : '<';
LE : '<=' | '≤';
GT : '>';
GE : '>=' | '≥';

ASSIGN : '=' | '≔';
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

// TODO(lorenz.leutgeb): Add greek alphabet and lowercase letters.

SUBSCRIPT_NUMBER : [\u2080-\u2089];
DERIVED_IDENTIFIER : '∂' SUBSCRIPT_NUMBER+;

IDENTIFIER : (('A' .. 'Z' | 'a'..'z' | 'α' | 'β' | 'γ' | 'δ' | 'ε' | '∂') ( 'A'..'Z' | 'a'..'z' | 'α' | 'β' | 'γ' | 'δ' | 'ε' | '∂' | '0'..'9' | '_' | '\'' | '.' )*);
TYPE : ('A'..'Z') ( 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' )*;
NUMBER : '0' | ('1'..'9') ('0'..'9')*;
QUOTED_STRING : QUOTE ( '\\"' | . )*? QUOTE;

// ML-style nested comments.
COMMENT : '(*' (COMMENT|.)*? '*)' -> channel(HIDDEN) ;
LINE_COMMENT  : '(*)' .*? '\n' -> channel(HIDDEN) ;

BLANK : [ \t\r\n\f]+ -> channel(HIDDEN);
