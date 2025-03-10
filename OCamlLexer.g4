lexer grammar OCamlLexer;
WS:[ \t\r\n\f]+ -> skip;
//Blanks : spaces. newlines, tabs, etc
COMMENT
   : '(*' .*? '*)' -> skip
   ;   
AND         : 'and';
AS          : 'as';
ASSERT      : 'assert';
ASR         : 'asr';
BEGIN       : 'begin';
CLASS       : 'class';
CONSTRAINT  : 'constraint';
DO          : 'do';
DONE        : 'done';
DOWNTO      : 'downto';
ELSE        : 'else';
END         : 'end';
EXCEPTION   : 'exception';
EXTERNAL    : 'external';
FALSE       : 'false';
FOR         : 'for';
FUN         : 'fun';
FUNCTION    : 'function';
FUNCTOR     : 'functor';
IF          : 'if';
IN          : 'in';
INCLUDE     : 'include';
INHERIT     : 'inherit';
INITIALIZER : 'initializer';
LAND        : 'land';
LAZY        : 'lazy';
LET         : 'let';
LOR         : 'lor';
LSL         : 'lsl';
LSR         : 'lsr';
LXOR        : 'lxor';
MATCH       : 'match';
METHOD      : 'method';
MOD         : 'mod';
MODULE      : 'module';
MUTABLE     : 'mutable';
NEW         : 'new';
NONREC      : 'nonrec';
OBJECT      : 'object';
OF          : 'of';
OPEN        : 'open';
OR          : 'or';
PRIVATE     : 'private';
REC         : 'rec';
SIG         : 'sig';
STRUCT      : 'struct';
THEN        : 'then';
TO          : 'to';
TRUE        : 'true';
TRY         : 'try';
TYPE        : 'type';
VAL         : 'val';
VIRTUAL     : 'virtual';
WHEN        : 'when';
WHILE       : 'while';
WITH        : 'with';
LPSTAR      :'(*';
NEQ         : '!=';
HASH        : '#';
AMP         : '&';
ANDAND      : '&&';
QUOTE       : '\'';
LPAREN      : '(';
RPAREN      : ')';
STAR        : '*';
PLUS        : '+';
COMMA       : ',';
ARROW       : '->';
MINUS       : '-';
DOTMINUS    : '-.';
DOT         : '.';
DOTDOT      : '..';
DOTTILDE    : '.~';
COLON       : ':';
COLONCOLON  : '::';
COLONEQ     : ':=';
COLONGT     : ':>';
SEMI        : ';';
SEMICOLON   : ';;';
LT          : '<';
LTLT        : '<<';
LTMINUS     : '<-';
EQ          : '=';
GT          : '>';
GTBRACK     : '>]';
GTBRACE     : '>}';
QUESTION    : '?';
LBRACKET    : '[';
LBRACKLT    : '[<';
LBRACKGT    : '[>';
LBRACKBAR   : '[|';
RBRACKET    : ']';
UNDERSCORE  : '_';
BACKQUOTE   : '`';
LBRACE      : '{';
LBRACELT    : '{<';
BAR         : '|';
BARBRACK    : '|]';
BARBAR      : '||';
RBRACE      : '}';
TILDE       : '~';
// PARSER      : 'parser';
// VALUE       : 'value';
// DOLLAR      : '$';
// DOLLARDOLLAR: '$$';
// DOLLARCOLON : '$:';
// COLONLT     : '<:';
GTGT        : '>>';
QUESTIONQUESTION : '??';
// IDENT
//     : LETTER (LETTER | DIGIT | '_' | '\'')*
//     ;
CAPITALIZED_IDENT
    : [A-Z] (LETTER | DIGIT | UNDERSCORE | QUOTE)*
    ;
LOWERCASE_IDENT
    : ([a-z] | UNDERSCORE) (LETTER | DIGIT | UNDERSCORE | QUOTE)*
    ;
fragment LETTER
    : [a-zA-Z]
    ;
fragment DIGIT : [0-9];
fragment HEX_DIGIT : [0-9a-fA-F];
fragment OCTAL_DIGIT : [0-7];
fragment BINARY_DIGIT : [0-1];
INTEGER_LITERAL
    : MINUS? DECIMAL_LITERAL (SUFFIX)?
    | MINUS? HEX_LITERAL (SUFFIX)?
    | MINUS? OCTAL_LITERAL (SUFFIX)?
    | MINUS? BINARY_LITERAL (SUFFIX)?
    ;
fragment DECIMAL_LITERAL
    : DIGIT (DIGIT | UNDERSCORE)*
    ;
fragment HEX_LITERAL
    : ('0x' | '0X') HEX_DIGIT (HEX_DIGIT | UNDERSCORE)*
    ;
fragment OCTAL_LITERAL
    : ('0o' | '0O') OCTAL_DIGIT (OCTAL_DIGIT | UNDERSCORE)*
    ;
fragment BINARY_LITERAL
    : ('0b' | '0B') BINARY_DIGIT (BINARY_DIGIT | UNDERSCORE)*
    ;
fragment SUFFIX
    : 'l' | 'L' | 'n'
    ;
//DOUBLE
FLOATING_LITERAL
    : MINUS? DECIMAL_FLOAT_LITERAL
    | MINUS? HEX_FLOAT_LITERAL
    ;
fragment DECIMAL_FLOAT_LITERAL
    : DECIMAL_PART DOT FRACTIONAL_PART? EXPONENT_PART?
    | DECIMAL_PART EXPONENT_PART
    ;
fragment HEX_FLOAT_LITERAL
    : ('0x' | '0X') HEX_PART (DOT HEX_FRACTIONAL_PART)? HEX_EXPONENT_PART?
    ;
fragment DECIMAL_PART
    : DIGIT (DIGIT | UNDERSCORE)*
    ;
fragment FRACTIONAL_PART
    : DIGIT (DIGIT | UNDERSCORE)*
    ;
fragment EXPONENT_PART
    : ('e' | 'E') (PLUS | MINUS)? DIGIT (DIGIT | UNDERSCORE)*
    ;
fragment HEX_PART
    : HEX_DIGIT (HEX_DIGIT | UNDERSCORE)*
    ;
fragment HEX_FRACTIONAL_PART
    : HEX_DIGIT (HEX_DIGIT | UNDERSCORE)*
    ;
fragment HEX_EXPONENT_PART
    : ('p' | 'P') (PLUS | MINUS)? DIGIT (DIGIT | UNDERSCORE)*
    ;
// PARTIAL_CHAR
//     : QUOTE (REGULAR_CHAR|ESCAPE_SEQUENCE) 
//     ;
CHAR_LITERAL
    : QUOTE (REGULAR_CHAR | ESCAPE_SEQUENCE) QUOTE
    ;
// CHAR_LITERAL 
//     : (PARTIAL_CHAR1|PARTIAL_CHAR2) QUOTE
// ;
fragment SYMBOL: ~[a-zA-Z0-9'\\]; 
fragment REGULAR_CHAR
    : ~['\\]   
    ;
fragment ESCAPE_SEQUENCE
    : '\\' (ESCAPE_CHAR | DECIMAL_ESCAPE | HEX_ESCAPE | OCTAL_ESCAPE)
    ;
fragment ESCAPE_CHAR
    : '\\'  
    | '"'   
    | QUOTE  
    | 'n'   
    | 't'  
    | 'b'  
    | 'r'   
    | ' '   
    ;
fragment DECIMAL_ESCAPE
    : DIGIT DIGIT DIGIT  
    ;
fragment HEX_ESCAPE
    : 'x' HEX_DIGIT HEX_DIGIT  
    ;
fragment OCTAL_ESCAPE
    : 'o' [0-3] [0-7] [0-7]  
    ;
STRING_LITERAL
    : '"' (STRING_CHARACTER | ESCAPE_SEQUENCE | UNICODE_ESCAPE | NEWLINE_ESCAPE)* '"'
    | QUOTED_STRING_LITERAL
    ;
fragment STRING_CHARACTER
    : ~["\\\r\n]   
    ;
fragment UNICODE_ESCAPE
    : '\\u{' HEX_DIGIT+ '}'
    ;
fragment NEWLINE_ESCAPE
    : '\\' '\r'? '\n' [ \t]*   
    ;
QUOTED_STRING_LITERAL
    : '{' QUOTED_STRING_ID BAR .*? '|}' 
    ;
fragment QUOTED_STRING_ID
    : [a-z_]+   
    ;
// LABEL
//     : TILDE LOWERCASE_IDENT COLON
//     ;
// OPTLABEL
//     : QUESTION LOWERCASE_IDENT COLON
//     ;
INFIX_SYMBOL
    : (CORE_OPERATOR_CHAR | '%' | LT) OPERATOR_CHAR*
    | HASH OPERATOR_CHAR+
    ;
PREFIX_SYMBOL
    : '!' OPERATOR_CHAR*
    | (QUESTION | TILDE) OPERATOR_CHAR+
    ;
fragment OPERATOR_CHAR
    : TILDE | '!' | QUESTION | CORE_OPERATOR_CHAR | '%' | LT | COLON | DOT
    ;
fragment CORE_OPERATOR_CHAR
    : '$' | AMP | STAR | PLUS | MINUS | '/' | EQ | GT | '@' | '^' | BAR
    ;
LINENUM_DIRECTIVE
    : HASH DIGIT+ '"' (STRING_CHARACTER | ESCAPE_SEQUENCE | UNICODE_ESCAPE)* '"' -> skip
    ;