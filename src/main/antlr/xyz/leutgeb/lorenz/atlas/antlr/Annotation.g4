grammar Annotation;

options {
    language = Java;
}

annotation : SQUARE_OPEN entries+=annotationEntry (COMMA entries+=annotationEntry)* SQUARE_CLOSE # nonEmptyAnnotation
           | SQUARE_OPEN SQUARE_CLOSE # zeroAnnotation
           /*| UNDERSCORE # dontCareAnnotation*/
           ;

annotationEntry : index MAPSTO coefficient=number ;

number : NUMBER # nat
       | numerator=NUMBER SLASH denominator=NUMBER # rat
       ;

index : NUMBER # rankIndex
      | PAREN_OPEN elements+=NUMBER* PAREN_CLOSE # otherIndex
      ;

// NUMBER : ('0' .. '9' );
NUMBER : '0' | ('1'..'9') ('0'..'9')*;
SLASH : '/' ;
// TIMES : '*';
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
