grammar XPath20;

options {
    output=AST;
}

tokens {
	AND_EXPR;
	OR_EXPR;
	BINDING;
	BINDINGS;
	FUNCTION_CALL;
	XPATH;
	EXPR_LIST;
	PREDICATE;
	VAR_NAME;
	VAR_REF;
	RANGE;
	EMPTY_PAREN;
	WILDCARD;
	
	OP_G_EQ			= '=';

	OP_G_NE			= '!=';
	OP_G_LT			= '<';
	OP_G_LTE		= '<=';
	OP_G_GT			= '>';
	OP_G_GTE		= '>=';
	
	OP_V_EQ			= 'eq';
	OP_V_NE			= 'ne';
	OP_V_LT			= 'lt';
	OP_V_LTE		= 'lte';
	OP_V_GT			= 'gt';
	OP_V_GTE		= 'gte';

	PLUS			= '+';
	MINUS			= '-';
	MULT			= '*';
	DIV			= 'div';
	IDIV			= 'idiv';
	MOD			= 'mod';
	
	UNION			= 'union';
	INTERSECT		= 'intersect';
	EXCEPT			= 'except';
	INSTANCE		= 'instance';
	OF			= 'of';
	TREAT			= 'treat';
	AS			= 'as';
	CASTABLE		= 'castable';
	CAST			= 'cast';
	ITEM			= 'item';
	EMPTY_SEQUENCE		= 'empty-sequence';
	ATTRIBUTE		= 'attribute';
	ELEMENT			= 'element';
	COMMENT			= 'comment';
	TEXT			= 'text';
	DOCUMENT_NODE		= 'document-node';
	NODE			= 'node';
	SCHEMA_ELEMENT		= 'schema-element';
	SCHEMA_ATTRIBUTE	= 'schema-attribute';
	PROCESSING_INSTRUCTION	= 'processing-instruction';
	
	IS			= 'is';
	L_SHIFT			= '<<';
	R_SHIFT			= '>>';

	APOS			= '\'';
	QUOTE			= '"';
	USCORE			= '_';	
	QMARK			= '?';
	AT			= '@';
	SEMI			= ';';
	DOLLAR			= '$';
	DOT			= '.';
	DDOT			= '..';
	COMMA			= ',';
	LPAREN			= '(';
	RPAREN			= ')';
	LBRACE			= '{';
	RBRACE			= '}';
	LBRACKET		= '[';
	RBRACKET		= ']';
	PIPE			= '|';
	COLON			= ':';
	SLASH			= '/';
	BSLASH			= '\\';
	DSLASH			= '//';
	RETURN			= 'return';
	IF			= 'if';
	THEN			= 'then';
	ELSE			= 'else';
	SOME			= 'some';
	EVERY			= 'every';
	IN			= 'in';
	SATISFIES		= 'satisfies';
	FOR			= 'for';
	AND			= 'and';
	OR			= 'or';
	TO			= 'to';
	FALSE			= 'false';
	TRUE			= 'true';
	
	AXIS_SEP		= '::';
	
	// forward axis
	
	CHILD	=	'child';
	DESCENDANT
		=	'descendant';
	SELF	=	'self';
	DESCENDANT_OR_SELF
		=	'descendant-or-self';
	FOLLOWING_SIBLING
		=	'following-sibling';
	FOLLOWING
		=	'following';
	NAMESPACE
		=	'namespace';
	
// reverse axis

	PARENT	=	'parent';
	ANCESTOR=	'ancestor';
	PRECEDING_SIBLING
		=	'preceding-sibling';
	PRECEDING
		=	'preceding';
	ANCESTOR_OR_SELF
		=	'ancestor-or-self';
}

@lexer::members {

List tokens = new ArrayList();

public void emit(Token token) {
        state.token = token;
    	tokens.add(token);
}

public Token nextToken() {
    	super.nextToken();
        if ( tokens.size()==0 ) {
            return Token.EOF_TOKEN;
        }
        return (Token)tokens.remove(0);
}

}

@members {
/*
public Object buildOptionalRepeatedOperandTree(Token op, Token first, List rest) {
	if (null == rest || rest.size() == 0) return first;
	
	Object root = adaptor.create(op);
	
	Object firstChild = adaptor.create(first);
	adaptor.setParent(firstChild, root);
	
	for (Token t : rest) {
		Object child = adaptor.create(t);
		adaptor.setParent(child, root);
	}
	
	return root;
}
*/
}

xpath	:	expr_list EOF
	->	^(XPATH expr_list);

expr_list
	:	expr ( COMMA expr )*
	->	^(EXPR_LIST expr+);

expr
	:	( for_expr | quantified_expr | if_expr | or_expr );

for_expr 
	:	FOR step_bindings RETURN expr
	->	^(FOR step_bindings ^(RETURN expr));

quantified_expr
	:	( t=SOME | t=EVERY ) step_bindings SATISFIES expr
	->	^($t step_bindings ^(SATISFIES expr));
	
step_bindings
	:	var_ref IN expr (COMMA var_ref IN expr)*
	->	^(BINDINGS ^(BINDING var_ref expr)+ );

if_expr :	IF^ LPAREN! expr_list RPAREN! THEN! expr ELSE! expr;

or_expr	:	first=and_expr ( OR rest+=and_expr )*;
/*	->	{
			buildOptionalRepeatedOperandTree(adaptor.createToken(OR_EXPR, "OR"), $first, $rest);
		};
//	->	^(OR_EXPR and_expr+);
*/

and_expr
	:	comparison_expr ( AND^ comparison_expr )*;
//	->	^(AND_EXPR comparison_expr+);

comparison_expr 
	:	range_expr ( ( general_comp^ | value_comp^ | node_comp^ ) range_expr )?;

range_expr
	:	additive_expr ( TO^ additive_expr )?;

additive_expr
	:	multiplicative_expr ( ( PLUS | MINUS )^ multiplicative_expr )*;
	
multiplicative_expr
	:	union_expr ( ( MULT | DIV | IDIV | MOD )^ union_expr )*;

union_expr
	:	intersect_except_expr ( ( UNION | PIPE )^ intersect_except_expr )*;

intersect_except_expr
	:	instance_of_expr ( ( INTERSECT | EXCEPT )^ instance_of_expr )*;

instance_of_expr
	:	treat_expr ( INSTANCE^ OF! sequence_type )?;

treat_expr
	:	castable_expr ( TREAT^ AS! sequence_type )?;

castable_expr
	:	cast_expr ( CASTABLE^ AS! single_type )?;

cast_expr
	:	unary_expr ( CAST^ AS! single_type )?;

unary_expr
	:	( MINUS | PLUS )* value_expr;

value_expr
	:	path_expr;

general_comp 
	:	op=OP_G_EQ | op=OP_G_NE | op=OP_G_LT | op=OP_G_LTE | op=OP_G_GT | op=OP_G_GTE
	->	$op;

value_comp 
	:	op=OP_V_EQ | op=OP_V_NE | op=OP_V_LT | op=OP_V_LTE | op=OP_V_GT | op=OP_V_GTE
	->	$op;

node_comp 
	:	op=IS | op=L_SHIFT | op=R_SHIFT
	->	$op;

// TODO: http://www.w3.org/TR/xpath20/#parse-note-leading-lone-slash
path_expr
	options { backtrack=true; memoize=false; }
	:	(DSLASH relative_path_expr)
		| ( SLASH relative_path_expr )
		| relative_path_expr;		

relative_path_expr
	:	step_expr ( ( SLASH | DSLASH ) step_expr )*;

step_expr
	:	filter_expr | axis_step;

axis_step
	:	( reverse_step | forward_step ) predicate_list;

forward_step
	:	( forward_axis node_test ) | abbrev_forward_step;

forward_axis
	:	( CHILD AXIS_SEP )
		| ( DESCENDANT AXIS_SEP )
		| ( ATTRIBUTE AXIS_SEP )
		| ( SELF AXIS_SEP )
		| ( DESCENDANT_OR_SELF AXIS_SEP )
		| ( FOLLOWING_SIBLING AXIS_SEP )
		| ( FOLLOWING AXIS_SEP )
		| ( NAMESPACE AXIS_SEP );

abbrev_forward_step
	:	AT? node_test;

reverse_step
	:	( reverse_axis node_test ) | abbrev_reverse_step;

reverse_axis
	:	(PARENT AXIS_SEP)
		| (ANCESTOR AXIS_SEP)
		| (PRECEDING_SIBLING AXIS_SEP)
		| (PRECEDING AXIS_SEP)
		| (ANCESTOR_OR_SELF AXIS_SEP);

abbrev_reverse_step
	:	DDOT;

node_test
	:	kind_test | name_test;

name_test
	:	qname | wildcard;

wildcard 
	:	MULT -> WILDCARD
		| ns=WILD_NS -> ^(WILDCARD WILD_NS)
		| ln=WILD_LN -> ^(WILDCARD WILD_LN);

filter_expr 
	:	primary_expr predicate_list;

predicate_list
	:	predicate*;

predicate 
	:	LBRACKET expr_list RBRACKET -> ^(PREDICATE expr_list);
	
primary_expr
	:	literal | var_ref | parenthesized_expr | context_item_expr | function_call;

literal :	numeric_literal | string_literal;

numeric_literal 
	:	INTEGER_LITERAL | DECIMAL_LITERAL;

// TODO
var_ref :	DOLLAR var_name -> ^(VAR_REF var_name);
	
var_name:	qname;

parenthesized_expr
	:	LPAREN RPAREN -> EMPTY_PAREN
		| LPAREN! expr_list^ RPAREN!;

context_item_expr
	:	DOT;

function_call
	:	qname LPAREN ( expr ( COMMA expr )*)? RPAREN -> ^(FUNCTION_CALL qname expr+);

single_type
	:	atomic_type QMARK?;

sequence_type
	options { backtrack=true; memoize=true; }
	:	( EMPTY_SEQUENCE LPAREN RPAREN )
		| ( item_type occurrence_indicator ) => item_type occurrence_indicator
		| item_type;

occurrence_indicator
	:	QMARK | MULT | PLUS;
	
item_type
	:	kind_test | ( ITEM LPAREN RPAREN ) | atomic_type;

atomic_type 
	:	qname;

kind_test
	:	document_test
		| element_test
		| attribute_test
		| schema_element_test
		| schema_attribute_test
		| pi_test
		| comment_test
		| text_test
		| any_kind_test ;

any_kind_test
	:	NODE LPAREN RPAREN;

document_test
	:	DOCUMENT_NODE LPAREN ( element_test | schema_element_test )? RPAREN;

text_test
	:	TEXT LPAREN RPAREN;

comment_test
	:	COMMENT LPAREN RPAREN;

pi_test	:	PROCESSING_INSTRUCTION^ LPAREN! ( NCNAME | string_literal )? RPAREN!;

attribute_test
	:	ATTRIBUTE^ LPAREN! ( attribute_name_or_wildcard ( COMMA! type_name )?)? RPAREN!;

attribute_name_or_wildcard
	:	attribute_name | MULT;

schema_attribute_test
	:	SCHEMA_ATTRIBUTE^ LPAREN! attribute_declaration RPAREN!;

attribute_declaration
	:	attribute_name;

element_test
	:	ELEMENT^ LPAREN! ( element_name_or_wildcard ( COMMA! type_name QMARK?)?)? RPAREN!;

element_name_or_wildcard
	:	element_name | MULT;

schema_element_test
	:	SCHEMA_ELEMENT^ LPAREN! element_declaration RPAREN!;

element_declaration
	:	element_name;

attribute_name
	:	qname;

element_name
	:	qname;

type_name
	:	qname;
/*
qname	:	pf=NCNAME COLON ln=NCNAME -> ^(QNAME $pf $ln)
		| NCNAME -> ^(QNAME NCNAME);
*/

qname	:	(QNAME | NCNAME);

string_literal 
	:	STRING_LITERAL;

// Lexer Grammar

//

//VAR_REF	:	DOLLAR! QNAME { System.out.println("foo"); };

fragment NCNAME_START_CHAR
	:	USCORE | LETTER;
/*
QNAME :
	(pf=NCNAME COLON)? ln=NCNAME
	{
		emit(new CommonToken(QNAME));
		if (null != $pf) {
			$pf.setType(NCNAME);
			emit($pf);
		}
		$ln.setType(NCNAME);
		emit($ln);
	};
*/

NCNAME
	:	LETTER ( LETTER | DIGIT | DOT | MINUS)*;

WILD_NS	:	MULT COLON NCNAME;

WILD_LN	:	NCNAME COLON MULT;

QNAME : NCNAME COLON NCNAME;

INTEGER_LITERAL
	:	DIGITS;
	
DECIMAL_LITERAL
	:	( DOT DIGITS )  | ( DIGITS DOT DIGITS* );
	
DOUBLE_LITERAL
	:	( ( DOT DIGITS ) | ( DIGITS ( DOT DIGITS? )? ) ) ( 'e' | 'E' ) ( PLUS | MINUS )? DIGITS; 

//((DOT ('0'..'9')+) | (('0'..'9')+ (DOT '0'..'9'*)?)) ('e' | 'E') (PLUS | MINUS)? ('0'..'9')+;

STRING_LITERAL : QUOTE ~(QUOTE)* QUOTE | APOS ~(APOS)* APOS;

fragment DIGITS
	:	DIGIT+;

fragment DIGIT
	:	'0'..'9';

WS  : (' '|'\r'|'\t'|'\u000C'|'\n') {$channel=HIDDEN;};

// '\u0024' | 

fragment LETTER : '\u005f' |
       '\u0041'..'\u005a' | '\u0061'..'\u007a' | 
       '\u00c0'..'\u00d6' | '\u00d8'..'\u00f6' | 
       '\u00f8'..'\u00ff' | '\u0100'..'\u1fff' | 
       '\u3040'..'\u318f' | '\u3300'..'\u337f' | 
       '\u3400'..'\u3d2d' | '\u4e00'..'\u9fff' | 
       '\uf900'..'\ufaff';
