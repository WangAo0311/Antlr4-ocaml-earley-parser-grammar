parser grammar OCamlParser;
options {
    tokenVocab = OCamlLexer;  
}

value_name
     :LOWERCASE_IDENT
     |LPAREN operator_name RPAREN
     ;
operator_name
     : PREFIX_SYMBOL
     | infix_op
     ;

// infix_op
//      : INFIX_SYMBOL
//      | '*' | '+' | '-' | '-.' | '=' | '!=' | '<' | '>' | 'or' | '||' | '&' | '&&' | ':='
//      | 'mod' | 'land' | 'lor' | 'lxor' | 'lsl' | 'lsr' | 'asr'
//      ;
infix_op
     : INFIX_SYMBOL
     | STAR | PLUS | MINUS | DOTMINUS | EQ | NEQ | LT | GT | OR | BARBAR | AMP | ANDAND | COLONEQ
     | MOD | LAND | LOR | LXOR | LSL | LSR | ASR
     ;


constr_name
     :CAPITALIZED_IDENT
     ;

tag_name
     :CAPITALIZED_IDENT
     ;
typeconstr_name
     :LOWERCASE_IDENT
     ;

field_name
     : LOWERCASE_IDENT
     ;
module_name
     : CAPITALIZED_IDENT
     ;
// INDENT ->
modtype_name
     : LOWERCASE_IDENT
     |CAPITALIZED_IDENT
     ;
class_name
     : LOWERCASE_IDENT
     ;
inst_var_name
     : LOWERCASE_IDENT
     ;
method_name
     : LOWERCASE_IDENT
     ;

module_path
    : module_name (DOT module_name)*
    ;

value_path
     : module_path DOT value_name
     |value_name
     ;

constr
     : module_path DOT constr_name
     |constr_name
     ;

typeconstr
    : extended_module_path DOT typeconstr_name
    | typeconstr_name
    ;
field
    : module_path DOT field_name
    | field_name
    ;


modtype_path
    : extended_module_path DOT modtype_name
    | modtype_name
    ;

class_path
    : module_path DOT class_name
    | class_name
    ;

classtype_path
    : extended_module_path DOT class_name
    | class_name
    ;



extended_module_path
    : extended_module_name (DOT extended_module_name)*
    ;

extended_module_name
    : module_name LPAREN extended_module_path RPAREN
    | module_name
    ;


typexpr

    : base_typexpr
   
    | typexpr (STAR typexpr)+
    | typexpr typeconstr
    
    | typexpr ARROW typexpr
    | typexpr HASH class_path
    //IDENT
    | typexpr AS (QUOTE ( LOWERCASE_IDENT | CAPITALIZED_IDENT))
    ;


base_typexpr
//INENT
     :(QUOTE ( LOWERCASE_IDENT | CAPITALIZED_IDENT))
     | UNDERSCORE
    | LPAREN typexpr RPAREN

    | (LOWERCASE_IDENT COLON) typexpr ARROW typexpr 
   
     | typeconstr
    
     | LPAREN typexpr (COMMA typexpr)* RPAREN typeconstr
    
     | polymorphic_variant_type 
     | LT DOTDOT GT
     | LT method_type (SEMI method_type)* (SEMI | SEMI DOTDOT)? GT
     | HASH class_path
     | LPAREN typexpr (COMMA typexpr)* RPAREN HASH class_path
     ;

poly_typexpr
    : typexpr   
    //INDENT                                  
    | (QUOTE ( LOWERCASE_IDENT | CAPITALIZED_IDENT))+ DOT typexpr                  
    ;
method_type
    : method_name COLON poly_typexpr              
    ;



polymorphic_variant_type
     : LBRACKET tag_spec_first (BAR tag_spec)* RBRACKET
     | LBRACKET GT (tag_spec (BAR tag_spec)*)? RBRACKET  
     |LBRACKET LT (BAR tag_spec_full)* (GT BACKQUOTE tag_name+)? RBRACKET               
     ;

tag_spec_first
    : BACKQUOTE tag_name (OF typexpr)?         
    | LBRACKET typexpr RBRACKET BAR tag_spec   

    ;

tag_spec
    : BACKQUOTE tag_name (OF typexpr)?         
    | typexpr                                 
    ;

tag_spec_full
    : BACKQUOTE tag_name (OF (AMP? typexpr (AMP typexpr)*))?  
    | typexpr                                     
    ;




constant
     : INTEGER_LITERAL
     | FLOATING_LITERAL
     | CHAR_LITERAL
     | STRING_LITERAL
     | constr
     //bug from constr_name to constr
     | FALSE
     | TRUE
     | LPAREN RPAREN
     | BEGIN END
     | LBRACKET RBRACKET
     | LBRACKBAR BARBRACK
     | BACKQUOTE

     ;


pattern
     :value_name
     | UNDERSCORE
     | constant
     | pattern AS value_name
     | LPAREN pattern RPAREN
     | LPAREN pattern COLON typexpr RPAREN
     | pattern BAR pattern
     | constr pattern
     | BACKQUOTE tag_name pattern
     | HASH typeconstr 
     | LPAREN pattern (COMMA pattern)+ RPAREN
     | pattern (COMMA pattern)+

  


     | LBRACE                                            
        field (COLON typexpr)? (EQ pattern)? (SEMI field (COLON typexpr)? (EQ pattern)?)* 
        (SEMI UNDERSCORE)? 
        (SEMI)? 
      RBRACE
      
     | LBRACKET pattern (SEMI pattern)* (SEMI)? RBRACKET
     | pattern COLONCOLON pattern
     | LBRACKBAR pattern (SEMI pattern)* (SEMI)? BARBRACK
     | CHAR_LITERAL DOTDOT CHAR_LITERAL
     | LAZY pattern
     | EXCEPTION pattern 
     | module_path DOT LPAREN pattern RPAREN             
     | module_path DOT LBRACKET pattern RBRACKET         
     | module_path DOT LBRACKBAR pattern BARBRACK        
     | module_path DOT LBRACE pattern RBRACE
     ;

 

expr
     :value_path  
     | constant
     | LPAREN expr RPAREN
     | BEGIN expr END
     | LPAREN expr COLON typexpr RPAREN
     | expr (COMMA expr)+
     | constr expr
     | BACKQUOTE tag_name expr
     | expr COLONCOLON expr
     | LBRACKET expr (SEMI expr)* (SEMI)? RBRACKET
     | LBRACKBAR expr (SEMI expr)* (SEMI)? BARBRACK
     | LBRACE field EQ expr (SEMI field EQ expr)* (SEMI)? RBRACE 
     | LBRACE expr WITH field EQ expr (SEMI field EQ expr)* (SEMI)? RBRACE 
     | expr (argument)+        
     | PREFIX_SYMBOL expr                    
     | MINUS expr                            
     | DOTMINUS expr                         
     | expr infix_op expr                    
     | expr DOT field                        
     | expr DOT field LTMINUS expr            
     | expr DOT LPAREN expr RPAREN           
     | expr DOT LPAREN expr RPAREN LTMINUS expr 
     | expr DOT LBRACKET expr RBRACKET       
     | expr DOT LBRACKET expr RBRACKET LTMINUS expr 
     | IF expr THEN expr (ELSE expr)?  
       
     | WHILE expr DO expr DONE               
     | FOR value_name EQ expr (TO | DOWNTO) expr DO expr DONE 
     | expr SEMI expr                        
     | MATCH expr WITH pattern_matching      
     | FUNCTION pattern_matching             
     | FUN multiple_matching                 
     | TRY expr WITH pattern_matching        
     | LET (REC)? let_binding (AND let_binding)* IN expr 
     | NEW class_path                        
     | OBJECT class_body END                 
     | expr HASH method_name                 
     | inst_var_name                         
     | inst_var_name LTMINUS expr             
     | LPAREN expr COLONGT typexpr RPAREN    
     | LPAREN expr COLON typexpr COLONGT typexpr RPAREN 
     | LBRACELT (inst_var_name EQ expr (SEMI inst_var_name EQ expr)* (SEMI)?)? GTBRACE 

             
   
     ;
     

argument
    : expr                                   
    | TILDE LOWERCASE_IDENT                       
    | TILDE LOWERCASE_IDENT COLON expr            
    | QUESTION LOWERCASE_IDENT                    
    | QUESTION LOWERCASE_IDENT COLON expr         
    ;
pattern_matching
    : BAR? pattern (WHEN expr)? ARROW expr (BAR pattern (WHEN expr)? ARROW expr)* 
    ;

multiple_matching
    : (parameter)+ (WHEN expr)? ARROW expr   
    ;

let_binding
    : pattern EQ expr                       
    | value_name (parameter)* (COLON typexpr)? (COLONGT typexpr)? EQ expr 
    ;

parameter
    : pattern                                   
    | TILDE LOWERCASE_IDENT                          
    | TILDE LPAREN LOWERCASE_IDENT (COLON typexpr)? RPAREN  
    | TILDE LOWERCASE_IDENT COLON pattern            
    | QUESTION LOWERCASE_IDENT                       
    | QUESTION LPAREN LOWERCASE_IDENT (COLON typexpr)? (EQ expr)? RPAREN 
    | QUESTION LOWERCASE_IDENT COLON pattern         
    | QUESTION LOWERCASE_IDENT COLON LPAREN pattern (COLON typexpr)? (EQ expr)? RPAREN 
    ;





type_definition
    : TYPE typedef (AND typedef)*                    
    ;
typedef
    : (type_params)? typeconstr_name type_information 
    ;

type_information
    : (type_equation)? (type_representation)? (type_constraint)* 
    ;

type_equation
    : EQ typexpr                                  
    ;

type_representation
    : EQ (BAR? constr_decl (BAR constr_decl)*      
       | LBRACE field_decl (SEMI field_decl)* (SEMI)? RBRACE) 
    ;


type_params
    : type_param                                  
    | LPAREN type_param (COMMA type_param)* RPAREN 
    ;

type_param
//INENT
    : 
    (variance)?(QUOTE ( LOWERCASE_IDENT | CAPITALIZED_IDENT))                     
    ;

variance
    : PLUS                                         
    | MINUS                                        
    ;


constr_decl
    : (constr_name | LPAREN RPAREN) (OF typexpr (STAR typexpr)*)? 
    ;

field_decl
    : (MUTABLE)? field_name COLON poly_typexpr     
    ;

type_constraint
//INENT
    : CONSTRAINT (QUOTE ( LOWERCASE_IDENT | CAPITALIZED_IDENT)) EQ typexpr            
    ;




exception_definition
    : EXCEPTION constr_name (OF typexpr (STAR typexpr)*)?   
    | EXCEPTION constr_name EQ constr                      
    ;




class_type
    : (LOWERCASE_IDENT COLON)? typexpr ARROW class_type  
    | class_body_type                             
    ;
class_body_type
    : OBJECT (LPAREN typexpr RPAREN)? LBRACE class_field_spec* RBRACE END  
    | LBRACKET (typexpr (COMMA typexpr)*)? RBRACKET classtype_path         
    ;

class_field_spec
    : INHERIT class_body_type                                                
    | VAL (MUTABLE)? (VIRTUAL)? inst_var_name COLON typexpr                  
    | VAL VIRTUAL MUTABLE inst_var_name COLON typexpr                        
    | METHOD (PRIVATE)? (VIRTUAL)? method_name COLON poly_typexpr            
    | METHOD VIRTUAL PRIVATE method_name COLON poly_typexpr                  
    | CONSTRAINT typexpr EQ typexpr                                          
    ;

class_expr
    : class_path                                         
    | LBRACKET typexpr (COMMA typexpr)* RBRACKET class_path 
    | LPAREN class_expr RPAREN                            
    | LPAREN class_expr COLON class_type RPAREN           
    | class_expr (LPAREN argument RPAREN)+                
    | FUN parameter+ ARROW class_expr                     
    | LET (REC)? let_binding (AND let_binding)* IN class_expr 
    | OBJECT class_body END                               
    ;

class_field
    : INHERIT class_expr (AS LOWERCASE_IDENT)?                
    | VAL (MUTABLE)? inst_var_name (COLON typexpr)? EQ expr   
    | VAL (MUTABLE)? VIRTUAL inst_var_name COLON typexpr      
    | VAL VIRTUAL MUTABLE inst_var_name COLON typexpr         
    | METHOD (PRIVATE)? method_name (parameter)* (COLON typexpr)? EQ expr 
    | METHOD (PRIVATE)? method_name COLON poly_typexpr EQ expr 
    | METHOD (PRIVATE)? VIRTUAL method_name COLON poly_typexpr 
    | METHOD VIRTUAL PRIVATE method_name COLON poly_typexpr    
    | CONSTRAINT typexpr EQ typexpr                           
    | INITIALIZER expr                                        
    ;

class_body
    : (LPAREN pattern (COLON typexpr)? RPAREN)? class_field*  
    ;

class_definition
    : CLASS class_binding (AND class_binding)*  
    ;
class_binding
    : (VIRTUAL)? (type_parameters)? class_name (LPAREN parameter RPAREN)* (COLON class_type)? EQ class_expr  
    ;
type_parameters

    : (QUOTE ( LOWERCASE_IDENT | CAPITALIZED_IDENT)) (COMMA(QUOTE ( LOWERCASE_IDENT | CAPITALIZED_IDENT)))*  
    ;



class_specification
    : CLASS class_spec (AND class_spec)*  
    ;
class_spec
    : (VIRTUAL)? (type_parameters)? class_name COLON class_type  
    ;


classtype_definition
    : CLASS TYPE classtype_def (AND classtype_def)*  
    ;

classtype_def
    : (VIRTUAL)? (type_parameters)? class_name EQ class_body_type  
    ;


module_type
    : modtype_path                                         
    | SIG specification* (SEMICOLON)? END                  
    | FUNCTOR LPAREN module_name COLON module_type RPAREN ARROW module_type  
    | module_type WITH mod_constraint (AND mod_constraint)*  
    | LPAREN module_type RPAREN                            
    ;

mod_constraint
    : TYPE (type_parameters)? typeconstr type_equation  
    | MODULE module_path EQ extended_module_path        
    ;

specification
    : VAL value_name COLON typexpr                                      
    | EXTERNAL value_name COLON typexpr EQ external_declaration          
    | type_definition                                                   
    | EXCEPTION constr_decl                                              
    | class_specification                                                
    | classtype_definition                                               
    | MODULE module_name COLON module_type                               
    | MODULE module_name LBRACE (LPAREN module_name COLON module_type RPAREN)* RBRACE COLON module_type  
    | MODULE TYPE modtype_name                                           
    | MODULE TYPE modtype_name EQ module_type                            
    | OPEN module_path                                                   
    | INCLUDE module_type                                                
    ;



module_expr
    : module_path                                         
    | STRUCT (module_items)? END                          
    | FUNCTOR LPAREN module_name COLON module_type RPAREN ARROW module_expr  
    | module_expr LPAREN module_expr RPAREN               
    | LPAREN module_expr RPAREN                           
    | LPAREN module_expr COLON module_type RPAREN         
    ;

module_items
    : (SEMICOLON)* (definition | expr) ((SEMICOLON)* (definition | SEMICOLON expr))* (SEMICOLON)*
    ;

definition
    : LET (REC)? let_binding (AND let_binding)*           
    | EXTERNAL value_name COLON typexpr EQ external_declaration  
    | type_definition                                    
    | exception_definition                               
    | class_definition                                   
    | classtype_definition                               
    | MODULE module_name LBRACE (LPAREN module_name COLON module_type RPAREN)* RBRACE (COLON module_type)? EQ module_expr  
    | MODULE TYPE modtype_name EQ module_type             
    | OPEN module_path                                    
    | INCLUDE module_expr                                 
    ;



unit_interface
    : (specification (SEMICOLON)?)*  
    ;

unit_implementation
    : (module_items(SEMI)?)?  
    ;






external_declaration
    : STRING_LITERAL (STRING_LITERAL (STRING_LITERAL)?)?  
    ;


program
    : unit_implementation 
    | unit_interface
    ;