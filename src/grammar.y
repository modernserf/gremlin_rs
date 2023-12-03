%token INT IDENT TYPE_IDENT TRUE FALSE UNDEFINED
%token ARRAY AS CASE CONST DEFER DO ELIF ELSE END IF LET LOOP MATCH ONEOF RECORD RETURN SUB THEN TYPE WHILE VAR

%nonassoc ":="
%nonassoc "==" "!=" "<" "<=" ">" ">="   
%left '-' '+' '|'
%left '*' '/' '&' '^'
%left AS
%precedence PRE

%%

block: %empty | stmt block;

stmt:
    LET assign_body  ';'
|   VAR assign_body  ';'
|   CONST assign_body  ';'
|   TYPE TYPE_IDENT ":=" type_expr ';'
|   SUB IDENT '(' sub_params ')' sub_return_type sub_body
|   RETURN ";"
|   RETURN expr ";"
|   WHILE expr LOOP block END
|   LOOP block END
|   IF expr THEN block if_else END
|   MATCH expr THEN case_list END
|   DEFER block END
|   expr ";"
|   ";"
;

assign_body: 
    binding ":=" expr
|   binding ':' type_expr ":=" expr
;
sub_body: 
    ';' // forward decl
|   DO block END
;

if_else: %empty
|   ELIF expr THEN block if_else
|   ELSE block
;

case_list: %empty 
|   CASE TYPE_IDENT ':' block case_list
;

binding: IDENT;

type_expr: TYPE_IDENT
|   TYPE_IDENT '.' TYPE_IDENT
|   '&' type_expr
|   '&' VAR type_expr
|   RECORD '{' record_type_fields '}'  
|   ONEOF '{' oneof_type_fields '}'
|   ARRAY '[' type_expr ']'
|   SUB '(' sub_type_params ')' sub_return_type
;
record_type_fields: %empty
|   IDENT ":" type_expr "," record_type_fields
|   IDENT ":" type_expr
|   CASE TYPE_IDENT ':' record_type_fields
;
oneof_type_fields: %empty
|   TYPE_IDENT "," oneof_type_fields
|   TYPE_IDENT
;

sub_params: %empty
|   IDENT ":" type_expr "," sub_params
|   IDENT ":" type_expr
;
sub_type_params: %empty
|   type_expr "," sub_type_params
|   type_expr
;
sub_return_type: %empty | "->" type_expr;

expr: 
    base_expr
|   expr ":=" expr
|   expr "==" expr
|   expr "!=" expr
|   expr "<" expr
|   expr "<=" expr
|   expr ">" expr
|   expr ">=" expr
|   expr '&' expr
|   expr '|' expr
|   expr '^' expr
|   expr '+' expr
|   expr '-' expr
|   expr '*' expr
|   expr '/' expr
|   expr AS type_expr
|   '-' expr %prec PRE
|   '!' expr %prec PRE
|   '&' expr %prec PRE
|   '&' VAR expr %prec PRE
;

base_expr:   
    '(' expr ')'
|   INT 
|   IDENT
|   TRUE
|   FALSE
|   UNDEFINED
    // subroutine call
|   base_expr '(' sub_args ')'
    // deref pointer
|   base_expr '[' ']'
    // index array or bitset
|   base_expr '[' expr ']'
    // record field
|   base_expr '.' IDENT
    // bitset field
|   base_expr '.' TYPE_IDENT
|   empty_record_or_bitset_literal
|   record_literal
|   bitset_literal
|   oneof_literal
|   array_literal
;

empty_record_or_bitset_literal: TYPE_IDENT '{' '}';

record_literal: TYPE_IDENT '{' record_pair record_items '}';
record_items: %empty
            | ',' record_pair record_items
            | ','
            ;
record_pair: IDENT ':' expr;

bitset_literal: TYPE_IDENT '{' TYPE_IDENT bitset_items '}';
bitset_items: %empty
            | ',' TYPE_IDENT bitset_items
            | ','
            ;

oneof_literal: TYPE_IDENT '.' TYPE_IDENT;

sub_args: %empty
        | expr
        | expr ',' sub_args
        ;

array_literal: ARRAY '[' TYPE_IDENT ':' INT ']' '{' array_items '}';
array_items: %empty | expr ',' array_items | expr;
%%
