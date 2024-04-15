This document defines a formal grammar for micro C.
I created this to give myself a structure guideline for writing the compiler.
Items denoted with a $ at the front such as $start, or $identifier are non-terminal symbols.
This is done to denote this even though they are text and not a symbol literal. Anything inside of single quotes, such
as
'const' are keywords or programmatic symbols, they are non-terminals.
This will follow similar structure to the ansi C specification, with a few key exceptions and items missing.

The ANSI C specification can be found [here](https://www.lysator.liu.se/c/ANSI-C-grammar-y.html#declaration).

If you are not familiar with formal grammars,
I recommend starting [here](https://en.wikipedia.org/wiki/Formal_grammar).

<pre>
$start
    : compilation_unit
    ;

compilation_unit
    : init_declaration compilation_unit 
    | compilation_unit $end_of_file
    ;

init_declaration
    : declaration_specifier declarator_list  ';'        // variable declaration
    | struct $identifier '{' struct_body '}' ';'        // struct definition
    | function_definition 
    ;

struct_body 
    : declarator ';'
    | declarator ';' struct_body
    ;

function_definition
    | type_specification declarator '(' parameter_list ')' '{' statement '}'
    | type_specification declarator '(' ')' '{' statement '}' ';'
    ;

declaration_specifier
    : storage_specifier type_specification 
    | type_specification
    ;

type_specification
    : type_qualifier type_specifier type 
    | type_qualifier type
    | type_specifier type
    | type
    ;

storage_specifier 
    : static
    | auto
    ;

type_qualifier
    : 'const'
    ;

type_specifier
    : 'signed'
    | 'unsigned'
    ;

type
    : 'void'
    | 'char'
    | 'int'
    | 'long'
    | 'double'
    | 'struct' $identifier
    ;

declarator_list
    : init_declarator
    | init_declarator ',' declarator_list
    ;

init_declarator
    : declarator 
    | declarator '=' initializer
    ;

declarator
    : pointer direct_declarator
    | direct_declarator
    ;

pointer
    : '*' 
    | '*' type_qualifier pointer                       
    ;

direct_declarator 
    : $identifier
    | $identifier '[' ']'                             
    | $identifier '[' constant_expression ']'          
    | '(' declarator ')' '(' parameter_type_list ')'    
    | '(' declarator ')' '(' ')'                      
    ;
    
parameter_type_list 
    : type_specification
    | type_specification ',' parameter_type_list
    ;

initializer
    : '{' expression_list '}'
    | '{' expression_list ',' '}'
    ;
    
expression_list
	: expression
	| expression_list ',' expression_list
	;

statement
    : compound_statement
    | iteration_statement
    | selection_statement
    | expression_statement
    | jump_statement
    ;

compound_statement
    : '{' '}'
    | '{' statement_list '}'
    ;

statement_list
    : statement
    | statement_list statement
    ;

iteration_statement
    : 'while' '(' expression ')' statement
    | 'for' '(' expression_statement expression_statement expression ')' statement
    ;

selection_statement
    : 'if' '(' expression ')' statement
    | 'if' '(' expression ')' statement 'else' statement
    ;

expression_statement
	: ';'
	| expression ';'
	;

jump_statement
    : 'return' ';'
    | 'return' expression ';'
    | 'break' ';'
    | 'continue' ';'
    ;
    
expression
    : $literal
    | $identifier
    | expression binary_operator expression
    | prefix_unary_operator expression
    | expression postfix_unary_operator
    | '(' expression ')'
    ;

binary_operator
    : '='
    | '+'   
    | '-'
    | '*'
    | '/' 
    | '%'
    | '=='
    | '!=' 
    | '<'
    | '>'
    | '<='
    | '>='
    | '&&'
    | '||'
    ;

prefix_unary_operator
    : '++'
    | '--'
    | '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | '!'
    ;

postfix_unary_operator
    : '++'
    | '--'
    | '.' $identifier
    | '->' $identifier
    | '.' $identifier postfix_unary_operator
    | '->' $identifier postfix_unary_operator
    | '[' expression ']'
    | '(' ')'
    | '(' expression_list ')'
    ;

    

</pre>