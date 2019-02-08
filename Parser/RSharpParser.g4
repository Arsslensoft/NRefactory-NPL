parser grammar RSharpParser;


options { tokenVocab=VSharpLexer; }

// entry point
compilation_unit
	: BYTE_ORDER_MARK? import_directives?
	  global_attribute_section* package_member_declarations? EOF
	;

//B.2 Syntactic grammar

//B.2.1 Basic concepts
package_or_type_name 
	: ( simple_name | qualified_alias_member) (DOT simple_name_access)*
	;

simple_name
	: identifier type_argument_list?
	;
simple_name_access
	: identifier type_argument_list?
	;
//B.2.2 Types
type 
	: base_type type_specifier*
	;
type_specifier
    : INTERR
	| rank_specifier 
	| STAR 
	;

base_type
	: simple_type
	| class_type  // represents types: enum, class, interface, delegate, type_parameter
	| VOID '*'
	;

simple_type 
	: numeric_type
	| BOOL
	;

numeric_type 
	: integral_type
	| floating_point_type
	;

integral_type 
	: SBYTE
	| BYTE
	| SHORT
	| USHORT
	| INT
	| UINT
	| LONG
	| ULONG
	| VLONG
	| CHAR
	;

floating_point_type 
	: FLOAT
	| DOUBLE
	| COMPLEX
	;

class_type 
	: package_or_type_name
	| OBJECT
	| STRING
	;

type_argument_list 
	: LT type ( COMMA type)* GT
	;

//B.2.4 Expressions
argument_list 
	: argument ( COMMA argument)*
	;

argument
	: (identifier COLON)? refout=(REF | OUT)? expression
	;

expression
	: assignment
	| non_assignment_expression
	;

non_assignment_expression
	: lambda_expression
	| query_expression
	| conditional_expression
	;

assignment
	: unary_expression assignment_operator expression
	;

assignment_operator
	: ASSIGNMENT | OP_ADD_ASSIGNMENT | OP_SUB_ASSIGNMENT | OP_MULT_ASSIGNMENT | OP_DIV_ASSIGNMENT | OP_MOD_ASSIGNMENT | OP_AND_ASSIGNMENT | OP_OR_ASSIGNMENT | OP_XOR_ASSIGNMENT | OP_LEFT_SHIFT_ASSIGNMENT | OP_LEFT_ROTATE_ASSIGNMENT | OP_RIGHT_ROTATE_ASSIGNMENT | OP_RIGHT_SHIFT_ASSIGNMENT
	;

conditional_expression
	:	null_coalescing_expression
	|	null_coalescing_expression INTERR expression COLON conditional_expression
	;

null_coalescing_expression
	: null_coalescing_expression OP_COALESCING binary_operator_expression
	| binary_operator_expression
	;

binary_operator_expression
	: binary_operator_expression BINARY_OPERATOR_LITERAL conditional_or_expression
	| conditional_or_expression
	;

conditional_or_expression
	: conditional_or_expression OP_OR conditional_and_expression
	| conditional_and_expression
	;

conditional_and_expression
	: conditional_and_expression OP_AND inclusive_or_expression
	| inclusive_or_expression
	;

inclusive_or_expression
	: inclusive_or_expression BITWISE_OR exclusive_or_expression
	| exclusive_or_expression
	;

exclusive_or_expression
	: exclusive_or_expression CARRET and_expression
	| and_expression
	;

and_expression
	: and_expression AMP equality_expression
	| equality_expression
	;

equality_expression
	: equality_expression OP_NE  relational_expression
	| equality_expression OP_EQ  relational_expression
	| relational_expression
	;

relational_expression
	: relational_expression (LT | GT | OP_LE| OP_GE) shift_expression  #relationalExpression
	| relational_expression IS type  #isExpression
	| relational_expression AS type  #asExpression
	| shift_expression #relationalShiftExpression
	;

shift_expression
	: shift_expression (OP_LEFT_SHIFT | OP_RIGHT_SHIFT | OP_LEFT_ROTATE | OP_RIGHT_ROTATE )  additive_expression
	| additive_expression
	;

additive_expression
	: additive_expression (PLUS | MINUS)  multiplicative_expression
	| multiplicative_expression
	;

multiplicative_expression
	: multiplicative_expression (STAR | DIV | PERCENT)  unary_expression
	| unary_expression
	;

unary_expression
	: primary_expression
	| PLUS unary_expression
	| MINUS unary_expression
	| BANG unary_expression
	| TILDE unary_expression
	| OP_INC unary_expression
	| OP_DEC unary_expression
	| OP_ZERO unary_expression
	| OP_PARITY unary_expression
	| OPEN_PARENS type CLOSE_PARENS unary_expression
	| AMP unary_expression
	| STAR unary_expression
	| UNARY_OPERATOR_LITERAL unary_expression
	;



primary_expression  // Null-conditional operators C# 6: https://msdn.microsoft.com/en-us/library/dn986595.aspx
	: element_access_expression primary_expression_end*
	| primary_expression_start primary_expression_end*
	;
element_access_expression
	: primary_expression_start bracket_expression+
	;
primary_expression_end
	: member_access bracket_expression* #memberAccessExpression
	| method_invocation bracket_expression*  #invocationExpression
	| OP_INC bracket_expression* #postIncrementExpression
	| OP_DEC bracket_expression* #postDecrementExpression
	| OP_PTR identifier  type_argument_list? bracket_expression* #pointerMemberAccessExpression
	;

primary_expression_start
	: literal                                   #literalExpression
	| simple_name           #simpleNameExpression
	| OPEN_PARENS expression CLOSE_PARENS       #parenthesisExpressions
	| predefined_type                           #memberAccessStartExpression
	| qualified_alias_member                    #memberAccessStartExpression
	| LITERAL_ACCESS                            #literalAccessExpression
	| SELF                                      #selfReferenceExpression
	| SUPER (DOT identifier type_argument_list? | OPEN_BRACKET expression_list CLOSE_BRACKET) #superAccessExpression
	| NEW type object_creation_expression  #objectOrDelegateCreationExpression
	| NEW OPEN_BRACE ( member_declarator ( COMMA member_declarator)* COMMA?)? CLOSE_BRACE  #anonymousTypeExpression
	| NEW (type ( OPEN_BRACKET expression_list CLOSE_BRACKET rank_specifier* array_initializer?   | rank_specifier+ array_initializer) | rank_specifier array_initializer) #arrayCreationExpression 
	| TYPEOF OPEN_PARENS (unbound_type_name | type | VOID) CLOSE_PARENS   #typeofExpression
	| CHECKED OPEN_PARENS expression CLOSE_PARENS                   #checkedExpression
	| UNCHECKED OPEN_PARENS expression CLOSE_PARENS                 #uncheckedExpression
	| DEFAULT OPEN_PARENS type CLOSE_PARENS                         #defaultValueExpression
	| DELEGATE (OPEN_PARENS explicit_anonymous_function_parameter_list? CLOSE_PARENS)? block #anonymousMethodExpression
	| SIZEOF OPEN_PARENS type CLOSE_PARENS #sizeofExpression
	| ADRESSOF OPEN_PARENS expression CLOSE_PARENS    #adressofExpression
	| NAMEOF OPEN_PARENS (identifier '.')* identifier CLOSE_PARENS  #nameofExpression
	;

member_access
	: INTERR? DOT identifier type_argument_list?
	;

bracket_expression
	: INTERR? OPEN_BRACKET indexer_argument ( COMMA indexer_argument)* CLOSE_BRACKET
	;

indexer_argument
	: (identifier COLON)? expression
	;

predefined_type
	: BOOL | BYTE | CHAR | DOUBLE | FLOAT | INT | LONG
	| OBJECT | SBYTE | SHORT | STRING | UINT | ULONG | VLONG | USHORT
	;

expression_list
	: expression (COMMA expression)*
	;

member_declarator
	: primary_expression
	| identifier ASSIGNMENT expression
	;

unbound_type_name
	: identifier ( generic_dimension_specifier? | '::' identifier generic_dimension_specifier?)
	  ('.' identifier generic_dimension_specifier?)*
	;

generic_dimension_specifier
	: '<' ','* '>'
	;


lambda_expression
	: anonymous_function_signature RIGHT_ARROW anonymous_function_body
	;

anonymous_function_signature
	: OPEN_PARENS CLOSE_PARENS
	| OPEN_PARENS explicit_anonymous_function_parameter_list CLOSE_PARENS
	| OPEN_PARENS implicit_anonymous_function_parameter_list CLOSE_PARENS
	| identifier
	;

explicit_anonymous_function_parameter_list
	: explicit_anonymous_function_parameter ( COMMA explicit_anonymous_function_parameter)*
	;

explicit_anonymous_function_parameter
	: refout=(REF | OUT)? type identifier
	;

implicit_anonymous_function_parameter_list
	: identifier (COMMA identifier)*
	;

anonymous_function_body
	: expression
	| block
	;

query_expression
	: from_clause query_body
	;

from_clause
	: FROM type? identifier IN expression
	;

query_body
	: query_body_clause* select_or_group_clause query_continuation?
	;

query_body_clause
	: from_clause
	| let_clause
	| where_clause
	| combined_join_clause
	| orderby_clause
	;

let_clause
	: LET identifier ASSIGNMENT expression
	;

where_clause
	: WHERE expression
	;

combined_join_clause
	: JOIN type? identifier IN expression ON expression EQUALS expression (INTO identifier)?
	;

orderby_clause
	: ORDERBY ordering (COMMA  ordering)*
	;

ordering
	: expression dir=(ASCENDING | DESCENDING)?
	;

select_or_group_clause
	: select_clause
	| group_clause
	;

group_clause
	: GROUP expression BY expression
	;
select_clause
	: SELECT expression
	;

query_continuation
	: INTO identifier query_body
	;

	//B.2.5 Statements
statement
	: identifier COLON statement                                       #labeledStatement
	| (local_variable_declaration | local_constant_declaration) SEMICOLON  #declarationStatement
	| embedded_statement                                             #embeddedStatement
	;

embedded_statement
	: block
	| simple_embedded_statement
	;

simple_embedded_statement
	: SEMICOLON                                                         #emptyStatement
	| expression SEMICOLON                                              #expressionStatement

	// selection statements
	| IF OPEN_PARENS expression CLOSE_PARENS if_body (ELSE if_body)?               #ifStatement
    | SWITCH OPEN_PARENS expression CLOSE_PARENS OPEN_BRACE switch_section* CLOSE_BRACE           #switchStatement

    // iteration statements
	| WHILE OPEN_PARENS expression CLOSE_PARENS embedded_statement                                        #whileStatement
	| WHILE OPEN_PARENS expression CLOSE_PARENS embedded_statement ELSE  embedded_statement               #whileElseStatement
	| DO embedded_statement WHILE OPEN_PARENS expression CLOSE_PARENS SEMICOLON                             #doStatement
	| FOR OPEN_PARENS for_initializer? SEMICOLON expression? SEMICOLON for_iterator? CLOSE_PARENS embedded_statement  #forStatement
	| FOREACH OPEN_PARENS type identifier IN expression CLOSE_PARENS embedded_statement                   #foreachStatement

    // jump statements
	| BREAK SEMICOLON                                                   #breakStatement
	| CONTINUE SEMICOLON                                            #continueStatement
	| GOTO identifier  SEMICOLON      #gotoStatement
	| GOTO CASE expression SEMICOLON  #gotoCaseStatement
	| GOTO DEFAULT SEMICOLON  #gotoDefaultStatement
	| RETURN expression? SEMICOLON                                     #returnStatement
	| THROW expression? SEMICOLON                                    #throwStatement

	| TRY block finally_clause  #tryFinallyStatement
	| TRY block catch_clauses finally_clause?   #tryCatchStatement 
	| CHECKED block                                               #checkedStatement
	| UNCHECKED block                                             #uncheckedStatement
	| SYNC OPEN_PARENS expression CLOSE_PARENS embedded_statement                  #syncStatement
	| USING OPEN_PARENS resource_acquisition CLOSE_PARENS embedded_statement       #usingStatement
	| YIELD RETURN expression SEMICOLON                       #yieldReturnStatement
	| YIELD BREAK SEMICOLON										#yieldBreakStatement
	| MIXIN identifier SEMICOLON                                        #mixinStatement
	| RESTRICT OPEN_PARENS expression CLOSE_PARENS embedded_statement                  #restrictStatement
	// unsafe statements
	| UNSAFE block                                                                       #unsafeStatement
	;
block
	: OPEN_BRACE statement_list? CLOSE_BRACE
	;

local_variable_declaration
	: type local_variable_declarator ( COMMA  local_variable_declarator)*
	;

local_variable_declarator
	: identifier (ASSIGNMENT variable_initializer)?
	;


local_constant_declaration
	: CONST type constant_declarators
	;

if_body
	: block
	| simple_embedded_statement
	;

switch_section
	: switch_label+ statement_list
	;

switch_label
	: CASE expression COLON
	| DEFAULT COLON
	;

statement_list
	: statement+
	;

for_initializer
	: local_variable_declaration
	| expression (COMMA  expression)*
	;

for_iterator
	: expression (COMMA  expression)*
	;

catch_clauses
	: specific_catch_clause (specific_catch_clause)* general_catch_clause?
	| general_catch_clause
	;

specific_catch_clause
	: CATCH OPEN_PARENS package_or_type_name identifier? CLOSE_PARENS exception_filter? block
	;

general_catch_clause
	: CATCH exception_filter? block
	;

exception_filter
	: WHEN OPEN_PARENS expression CLOSE_PARENS
	;

finally_clause
	: FINALLY block
	;

resource_acquisition
	: local_variable_declaration
	| expression
	;

//B.2.6 Packages;
package_declaration
	: PACKAGE qualified_identifier package_body ';'?
	;

qualified_identifier
	: identifier ( DOT identifier )*
	;

package_body
	: OPEN_BRACE import_directives? package_member_declarations? CLOSE_BRACE
	;


import_directives
	: import_directive+
	;

import_directive
	: IMPORT identifier ASSIGNMENT package_or_type_name SEMICOLON           #importAliasDirective
	| IMPORT package_or_type_name SEMICOLON                         #importPackageDirective
//	| IMPORT STATIC package_or_type_name SEMICOLON                 #importStaticDirective
	;

package_member_declarations
	: package_member_declaration+
	;

package_member_declaration
	: package_declaration
	| type_declaration
	;


default_member_declaration
	: constant_declaration
	| method_declaration
	| property_declaration
	| indexer_declaration
	| operator_declaration
	| field_declaration
	| event_declaration
	| conversion_operator_declarator 
	| mixin_definition
	| interrupt_definition
	;


type_declaration
	: class_definition
    | struct_definition 
	| union_definition 
	| interface_definition 
	| enum_definition 
	| delegate_definition
    ;

qualified_alias_member
	: identifier DOUBLE_COLON identifier type_argument_list?
	;

//B.2.7 Classes;
type_parameter_list
	: LT type_parameter (COMMA  type_parameter)* GT
	;

type_parameter
	: identifier
	;

class_base
	: COLON class_type (COMMA  class_type)*
	;

interface_type_list
	: package_or_type_name (COMMA  package_or_type_name)*
	;

type_parameter_constraints_clauses
	: type_parameter_constraints_clause+
	;

type_parameter_constraints_clause
	: WHERE identifier COLON type_parameter_constraints
	;

type_parameter_constraints
	:  primary_constraint (COMMA secondary_constraints)?
	;

primary_constraint
	: class_type
	| CLASS
	;

secondary_constraints
	: package_or_type_name (COMMA package_or_type_name)*
	;

class_body
	: OPEN_BRACE class_member_declarations? CLOSE_BRACE
	;

class_member_declarations
	: class_member_declaration+
	;

class_member_declaration
	: common_member_declaration 
	| destructor_definition
	;

method_modifiers
	: method_modifier+
	;

member_modifiers
	: member_modifier+
	;
type_modifiers
	: type_modifier+
	;

type_modifier
	: PUBLIC
	| PROTECTED
	| FRIEND
	| PRIVATE
	| SEALED
	| ABSTRACT
	| STATIC
	;

member_modifier
	: NEW
	| PUBLIC
	| PROTECTED
	| FRIEND
	| PRIVATE
	| READONLY
	| VIRTUAL
	| OVERRIDE
	| STATIC
	| EXTERN
	| SYNC
	;

method_modifier
	: NEW
	| PUBLIC
	| PROTECTED
	| FRIEND
	| PRIVATE
	| READONLY
	| VIRTUAL
	| OVERRIDE
	| STATIC
	| EXTERN
	| SYNC
	| SUPERSEDE 
	| INLINE
	| SYNC
	| ABSTRACT
	;

common_member_declaration
	: constant_declaration
	| method_declaration
	| property_declaration
	| indexer_declaration
	| operator_declaration
	| field_declaration
	| event_declaration
	| conversion_operator_declarator
	| constructor_declaration
	| mixin_definition
	;

constant_declarators
	: constant_declarator (COMMA constant_declarator)*
	;

constant_declarator
	: identifier ASSIGNMENT expression
	;

variable_declarators
	: variable_declarator (COMMA  variable_declarator)*
	;

variable_declarator
	: identifier (ASSIGNMENT variable_initializer)?
	;

variable_initializer
	: expression
	| array_initializer
	;

return_type
	: type
	| VOID
	;

member_name
	: package_or_type_name DOT identifier
	| identifier
	;

method_body
	: block
	| SEMICOLON
	| RIGHT_ARROW expression SEMICOLON
	;

formal_parameter_list
	: parameter_array
	| fixed_parameters (COMMA parameter_array)?
	;

fixed_parameters
	: fixed_parameter ( COMMA fixed_parameter )*
	;

fixed_parameter
	: attributes? parameter_modifier? arg_declaration
	;

parameter_modifier
	: REF
	| OUT
	| SELF
	;

parameter_array
	: attributes? PARAMS array_type identifier
	;

accessor_declarations
	: get_accessor_declaration set_accessor_declaration
	| set_accessor_declaration get_accessor_declaration
	| get_accessor_declaration
	;

get_accessor_declaration
	: attributes? GET accessor_body
	;

set_accessor_declaration
	: attributes? accessor_modifier? SET accessor_body
	;

accessor_modifier
	: PROTECTED
	| FRIEND
	| PRIVATE
	| PROTECTED FRIEND
	| FRIEND PROTECTED
	;

accessor_body
	: block
	| SEMICOLON
	;

event_accessor_declarations
	: add_accessor_declaration remove_accessor_declaration raise_accessor_declaration?
	| remove_accessor_declaration add_accessor_declaration raise_accessor_declaration?
	| add_accessor_declaration remove_accessor_declaration raise_accessor_declaration?
	;

add_accessor_declaration
	: attributes? accessor_modifier? ADD block
	;

remove_accessor_declaration
	: attributes? accessor_modifier? REMOVE block
	;

raise_accessor_declaration
	: attributes? RAISE block
	;

overloadable_operator
	: '+'
	| '-'
	| BANG
	| '~'
	| '++'
	| '--'
	| TRUE
	| FALSE
	| '*'
	| '/'
	| '%'
	| '&'
	| '|'
	| '^'
	| '<<'
	| OP_RIGHT_SHIFT
	| OP_EQ
	| OP_NE
	| '>'
	| '<'
	| '>='
	| '<='
	| BINARY_OPERATOR_LITERAL
	| UNARY_OPERATOR_LITERAL
	;

conversion_operator_declarator
	: attributes? method_modifiers? (IMPLICIT | EXPLICIT) OPERATOR type OPEN_PARENS arg_declaration CLOSE_PARENS method_body
	;

constructor_initializer
	: COLON (SUPER | SELF) OPEN_PARENS argument_list? CLOSE_PARENS
	;

body
	: block
	| ';'
	;

//B.2.8 Structs
struct_interfaces
	: COLON interface_type_list
	;

struct_body
	: OPEN_BRACE common_member_declaration* CLOSE_BRACE
	;



//B.2.9 Arrays
array_type
	: base_type ((STAR | INTERR)* rank_specifier)+
	;

rank_specifier
	: OPEN_BRACKET COMMA* CLOSE_BRACKET
	;

array_initializer
	: OPEN_BRACE (variable_initializer (COMMA  variable_initializer)* COMMA?)? CLOSE_BRACE
	;

//B.2.10 Interfaces


interface_base
	: COLON interface_type_list
	;

interface_body
	: OPEN_BRACE interface_member_declaration* CLOSE_BRACE
	;

interface_member_declaration
	: attributes? NEW?
	  (UNSAFE? type
	    ( identifier type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? ';'
	    | identifier OPEN_BRACE interface_accessors CLOSE_BRACE
	    | SELF '[' formal_parameter_list ']' OPEN_BRACE interface_accessors CLOSE_BRACE)
	  | VOID identifier type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? ';'
	  | EVENT type identifier ';')
	;

interface_accessors
	: attributes? (GET ';' (attributes? SET ';')? | SET ';' (attributes? GET ';')?)
	;

//B.2.11 Enums
enum_base
	: COLON integral_type
	;

enum_body
	: OPEN_BRACE (enum_member_declaration (COMMA  enum_member_declaration)* COMMA?)? CLOSE_BRACE
	;

enum_member_declaration
	: attributes? identifier (ASSIGNMENT expression)?
	;

//B.2.12 Delegates

//B.2.13 Attributes
global_attribute_section
	: OPEN_BRACKET global_attribute_target COLON attribute_list COMMA? CLOSE_BRACKET
	;

global_attribute_target
	: keyword
	| identifier
	;

attributes
	: attribute_section+
	;

attribute_section
	: OPEN_BRACKET (attribute_target COLON)? attribute_list COMMA? CLOSE_BRACKET
	;

attribute_target
	: keyword
	| identifier
	;

attribute_list
	: attribute (COMMA  attribute)*
	;

attribute
	: package_or_type_name (OPEN_PARENS (attribute_argument (COMMA  attribute_argument)*)? CLOSE_PARENS)?
	;

attribute_argument
	: (identifier COLON)? expression
	;

//B.3 Grammar extensions for unsafe code
pointer_type
	: (simple_type | class_type) (rank_specifier | '?')* '*'
	| VOID '*'
	;


literal
	: boolean_literal
	| string_literal
	| INTEGER_LITERAL
	| HEX_INTEGER_LITERAL
	| OCT_INTEGER_LITERAL
	| BIN_INTEGER_LITERAL
	| REAL_LITERAL
	| CHARACTER_LITERAL
	| NULL
	;

boolean_literal
	: TRUE
	| FALSE
	;

string_literal
	: interpolated_regular_string
	| interpolated_verbatium_string
	| REGULAR_STRING
	| VERBATIUM_STRING
	;

interpolated_regular_string
	: INTERPOLATED_REGULAR_STRING_START interpolated_regular_string_part* DOUBLE_QUOTE_INSIDE
	;


interpolated_verbatium_string
	: INTERPOLATED_VERBATIUM_STRING_START interpolated_verbatium_string_part* DOUBLE_QUOTE_INSIDE
	;

interpolated_regular_string_part
	: interpolated_string_expression
	| DOUBLE_CURLY_INSIDE
	| REGULAR_CHAR_INSIDE
	| REGULAR_STRING_INSIDE
	;

interpolated_verbatium_string_part
	: interpolated_string_expression
	| DOUBLE_CURLY_INSIDE
	| VERBATIUM_DOUBLE_QUOTE_INSIDE
	| VERBATIUM_INSIDE_STRING
	;

interpolated_string_expression
	: expression (',' expression)* (':' FORMAT_STRING+)?
	;

//B.1.7 Keywords
keyword
	: ABSTRACT
	| AS
	| SUPER
	| BOOL
	| BREAK
	| BYTE
	| CASE
	| CATCH
	| CHAR
	| CHECKED
	| CLASS
	| CONST
	| CONTINUE
	| COMPLEX
	| DEFAULT
	| DELEGATE
	| DO
	| DOUBLE
	| ELSE
	| ENUM
	| EVENT
	| EXPLICIT
	| EXTERN
	| FALSE
	| FINALLY
	| FLOAT
	| FOR
	| FOREACH
	| GOTO
	| IF
	| IMPLICIT
	| IN
	| INT
	| INTERFACE
	| FRIEND
	| IS
	| SYNC
	| LONG
	| PACKAGE
	| NEW
	| NULL
	| MIXIN
	| OBJECT
	| OPERATOR
	| OUT
	| OVERRIDE
	| PARAMS
	| PRIVATE
	| PROTECTED
	| PUBLIC
	| READONLY
	| REF
	| RETURN
	| SBYTE
	| SEALED
	| SHORT
	| SIZEOF
	| STATIC
	| STRING
	| STRUCT
	| SWITCH
	| SELF
	| THROW
	| TRUE
	| TRY
	| TYPEOF
	| UINT
	| ULONG
	| UNCHECKED
	| UNSAFE
	| UNION
	| USHORT
	| IMPORT
	| USING
	| VIRTUAL
	| VOID
	| VOLATILE
	| WHILE
	;

class_definition
	: attributes? type_modifiers? CLASS identifier type_parameter_list? class_base? type_parameter_constraints_clauses?
	    class_body SEMICOLON?
	;

mixin_definition
	: MIXIN identifier block
	;
interrupt_definition
	: attributes? INTERRUPT (INTEGER_LITERAL | HEX_INTEGER_LITERAL) block
	;

struct_definition
	: attributes? type_modifiers? STRUCT identifier type_parameter_list? struct_interfaces? type_parameter_constraints_clauses?
	    struct_body ';'?
	;

union_definition
	: attributes? type_modifiers? UNION identifier type_parameter_list? struct_interfaces? type_parameter_constraints_clauses?
	    struct_body ';'?
	;

interface_definition
	: attributes? type_modifiers? INTERFACE identifier type_parameter_list? interface_base?
	    type_parameter_constraints_clauses? interface_body SEMICOLON?
	;

enum_definition
	: attributes? type_modifiers? ENUM identifier enum_base? enum_body SEMICOLON?
	;

delegate_definition
	: attributes? type_modifiers? DELEGATE return_type identifier type_parameter_list?
	  OPEN_PARENS formal_parameter_list? CLOSE_PARENS type_parameter_constraints_clauses? SEMICOLON
	;

event_declaration
	: attributes? method_modifiers? EVENT type member_name OPEN_BRACE event_accessor_declarations CLOSE_BRACE
	;

field_declaration
	: attributes? member_modifiers? type variable_declarators SEMICOLON
	;

property_declaration
	: attributes? method_modifiers? type member_name (OPEN_BRACE accessor_declarations CLOSE_BRACE (ASSIGNMENT variable_initializer SEMICOLON)? | RIGHT_ARROW expression SEMICOLON)
	;

constant_declaration
	: attributes? member_modifiers? CONST type constant_declarators SEMICOLON
	;

indexer_declaration
	: attributes? method_modifiers? type SELF OPEN_BRACKET formal_parameter_list CLOSE_BRACKET (OPEN_BRACE accessor_declarations CLOSE_BRACE | RIGHT_ARROW expression ';')
	| attributes? method_modifiers? type package_or_type_name DOT SELF OPEN_BRACKET formal_parameter_list CLOSE_BRACKET (OPEN_BRACE accessor_declarations CLOSE_BRACE | RIGHT_ARROW expression ';')
	;

destructor_definition
	: attributes? method_modifiers? TILDE SELF OPEN_PARENS CLOSE_PARENS body
	;

constructor_declaration
	: attributes? method_modifiers? SELF OPEN_PARENS formal_parameter_list? CLOSE_PARENS constructor_initializer? body
	;

method_declaration 
	: attributes? method_modifiers? return_type member_name type_parameter_list? OPEN_PARENS formal_parameter_list? CLOSE_PARENS
	    type_parameter_constraints_clauses? method_body 
	;



operator_declaration 
	: attributes? method_modifiers? type OPERATOR overloadable_operator OPEN_PARENS arg_declaration
	       (',' arg_declaration)? CLOSE_PARENS method_body
	;

arg_declaration
	: type identifier ('=' expression)?
	;

method_invocation
	: OPEN_PARENS argument_list? CLOSE_PARENS
	;

object_creation_expression
	: OPEN_PARENS argument_list? CLOSE_PARENS
	;

identifier
	: IDENTIFIER
	| ADD
	| ARGLIST
	| SET
	| REMOVE
	| RAISE
	| NAMEOF
	| GET
	| ADRESSOF
	| PARTIAL
	| WHERE
	| ASCENDING
	| BY
	| DESCENDING
	| EQUALS
	| FROM
	| GROUP
	| INTO
	| JOIN
	| LET
	| ON
	| ORDERBY
	| SELECT
	;
