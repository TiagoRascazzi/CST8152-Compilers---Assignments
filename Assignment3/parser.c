/*
* File name: parser.c
* Compiler: MS Visual Studio 2015
* Author: Tiago Donchegay, 040867850, Nicholas Richer,
* Course: CST8152_010 Compilers
* Assignment: 3
* Date: April 20th 2018
* Professor: Svillen Ranev
* Purpose: -----TODO-----
*/

#include "buffer.h"
#include "token.h"
#include "parser.h"

extern int line;
extern char * kw_table[];
extern Buffer * str_LTBL;
extern Token malar_next_token(Buffer * sc_buf);

static Token lookahead;
static Buffer* sc_buf;
int synerrno;

void parser(Buffer * in_buf){
	sc_buf = in_buf;
	lookahead = malar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

void match(int pr_token_code, int pr_token_attribute) {

	if (lookahead.code != pr_token_code){
		syn_eh(pr_token_code);
		return;
	}

	if (lookahead.code == KW_T
		|| lookahead.code == LOG_OP_T
		|| lookahead.code == ART_OP_T
		|| lookahead.code == REL_OP_T) {

		if (lookahead.attribute.get_int != pr_token_attribute) {
			syn_eh(pr_token_code); 
			return;
		}
	}

	if (lookahead.code != SEOF_T) {
		lookahead = malar_next_token(sc_buf);

		if (lookahead.code == ERR_T) {
			syn_printe();
			lookahead = malar_next_token(sc_buf);
			++synerrno;
			return;
		}
	}
}

void syn_eh(int sync_token_code) {
	syn_printe();
	++synerrno;

	while (lookahead.code != sync_token_code) {

		if (sync_token_code != SEOF_T && lookahead.code == SEOF_T) {
			exit(synerrno);
		}
		lookahead = malar_next_token(sc_buf);
	}

	if (lookahead.code != SEOF_T)
		lookahead = malar_next_token(sc_buf);

}

/* error printing function for Assignment 3 (Parser), W18 */
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

void gen_incode(char* str) {
	printf("%s\n", str);
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<program> ->
PLATYPUS { <opt_statements> }

FIRST(<program> = { KW_T(PLATYPUS) })
*****************************************************************************/
void program(void) {
#ifdef DEBUG 
		printf(">>> <program>\n"); 
#endif
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<statements> ->
<statement> <statements_prime>

FIRST(<statements> = {AVID, SVID, KW_T=(IF, READ, WHILE, WRITE, ϵ)})
*****************************************************************************/
void statements(void) {
#ifdef DEBUG 
	printf(">>> <statements>\n");
#endif
	statement();
	statements_prime();
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<statements_prime> -> 
<statement> <statements prime> | ϵ

FIRST(<statements_prime> = {AVID, SVID, KW_T=(IF, READ, WHILE, WRITE, ϵ)})
*****************************************************************************/
void statements_prime(void) {
#ifdef DEBUG 
	printf(">>> <statements_prime>\n");
#endif
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: 
		statement(); 
		statements_prime();  
		break;

	case KW_T:
		if (lookahead.attribute.kwt_idx == IF
			|| lookahead.attribute.kwt_idx == WHILE
			|| lookahead.attribute.kwt_idx == READ
			|| lookahead.attribute.kwt_idx == WRITE) {
			statement();
			statements_prime();
			break;
		}

	default: ;
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<opt_statements> -> 
<statements> | ϵ

FIRST(<opt_statements> = {AVID, SVID, KW_T=(IF, READ, WHILE, WRITE, ϵ)})
*****************************************************************************/
void opt_statements() {
#ifdef DEBUG 
	printf(">>> <opt_statements>\n");
#endif
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: 
		statements(); 
		break;

	case KW_T:
		if (lookahead.attribute.kwt_idx == IF
			|| lookahead.attribute.kwt_idx == WHILE
			|| lookahead.attribute.kwt_idx == READ
			|| lookahead.attribute.kwt_idx == WRITE) {
			statements();
			break;
		}

	default: gen_incode("PLATY: Opt_statements parsed");
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<statement> ->
  <assignment_statement>
| <selection_statement>
| <iteration_statement>
| <input_statement>
| <output_statement>

FIRST(<statements> = {AVID, SVID, KW_T=(IF, READ, WHILE, WRITE)})
*****************************************************************************/
void statement(void) {
#ifdef DEBUG 
	printf(">>> <statement>\n");
#endif
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: 
		assignment_statement(); 
		break;

	case KW_T:
		if (lookahead.attribute.kwt_idx == IF) {
			selection_statement();  break;
		}else if (lookahead.attribute.kwt_idx == WHILE) {
			iteration_statement();  break;
		}else if (lookahead.attribute.kwt_idx == READ) {
			input_statement();  break;
		}else if (lookahead.attribute.kwt_idx == WRITE) {
			output_statement();  break;
		}

	default: syn_printe();
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<assignment_statement> ->
<assignment_expression>;

FIRST(<assignment_statement> = {AVID, SVID})
*****************************************************************************/
void assignment_statement(void){
#ifdef DEBUG 
	printf(">>> <assignment_statement>\n");
#endif
	assignment_expression();
	match(EOS_T, NO_ATTR); 
	gen_incode("PLATY: Assignment statement parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<assignment_expression> ->
  AVID = <arithmetic_expression>
| SVID = <string_expression>

FIRST(<assignment_expression> = {AVID, SVID})
*****************************************************************************/
void assignment_expression(void){
#ifdef DEBUG 
	printf(">>> <assignment_expression>\n");
#endif
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression(); 
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;

	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;

	default: syn_printe();
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<selection_statement> ->
IF <pre-condition> ( <conditional_expression> ) THEN { <opt_statements> }
ELSE { <opt_statements> } ;

FIRST(<selection_statement> = {KW_T = (IF)})
*****************************************************************************/
void selection_statement(void) {
#ifdef DEBUG 
	printf(">>> <selection_statement>\n");
#endif
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");

}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<iteration_statement> ->
WHILE <pre-condition> ( <conditional_expression> )
REPEAT { <statements> };

FIRST(<iteration_statement> = {KW_T = (WHILE)})
*****************************************************************************/
void iteration_statement(void) {
#ifdef DEBUG 
	printf(">>> <iteration_statement>\n");
#endif
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<pre-condition> ->
TRUE | FALSE

FIRST(<pre-condition> = {KW_T = (FALSE, TRUE)})
*****************************************************************************/
void pre_condition(void) {
#ifdef DEBUG 
	printf(">>> <pre_condition>\n");
#endif
	switch (lookahead.code)
	{
	case KW_T:
		if (lookahead.attribute.kwt_idx == TRUE)
			match(KW_T, TRUE);
		else if (lookahead.attribute.kwt_idx == FALSE)
			match(KW_T, FALSE);
		break;

	default:
		syn_printe();
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<input_statement> ->
READ ( <variable_list> );

FIRST(<input_statement> = {KW_T = (READ)})
*****************************************************************************/
void input_statement(void) {
#ifdef DEBUG 
	printf(">>> <input_statement>\n");
#endif
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<variable_list> ->
<variable_identifier> <variable_list_prime>

FIRST(<variable_list> = {AVID_T, SVID_T})
*****************************************************************************/
void variable_list(void) {
#ifdef DEBUG 
	printf(">>> <variable_list>\n");
#endif
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<variable_list_prime> -> 
, <variable_identifier> <variable_list_prime> | ϵ

FIRST(<variable_list_prime> = { , , ϵ})
*****************************************************************************/
void variable_list_prime(void) {
#ifdef DEBUG 
	printf(">>> <variable_list_prime>\n");
#endif
	switch (lookahead.code)
	{
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_prime();
		break;

	default:
		break;
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<opt_variable_list> -> 
<variable_list> | ϵ

FIRST(<opt_variable_list> = { AVID_T, SVID_T , ϵ})
*****************************************************************************/
void opt_variable_list(void) {
#ifdef DEBUG 
	printf(">>> <opt_variable_list>\n");
#endif
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		variable_list();
		break;

	default: ;
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<variable_identifier> ->
AVID_T | SVID_T

FIRST(<variable_identifier> = { AVID_T, SVID_T})
*****************************************************************************/
void variable_identifier(void) {
#ifdef DEBUG 
	printf(">>> <variable_identifier>\n");
#endif
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;

	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;

	default: syn_printe();
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<output_statement> ->
WRITE ( <output_statement_argument> );

FIRST(<output_statement> = { KW_T = (WRITE)})
*****************************************************************************/
void output_statement(void) {
#ifdef DEBUG 
	printf(">>> <output_statement>\n");
#endif
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_statement_argument();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<output_statement_argument> ->
<opt_variable_list> | STR_T

FIRST(<output_statement_argument> = { AVID_T, STR_T, SVID_T, ϵ})
*****************************************************************************/
void output_statement_argument(void) {
#ifdef DEBUG 
	printf(">>> <output_statement_argument>\n");
#endif
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T:
			opt_variable_list();
			break;

		case STR_T:
			match(STR_T, NO_ATTR);
			gen_incode("PLATY: Output list (string literal) parsed");
			break;

	default: gen_incode("PLATY: Output list (empty) parsed");
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<arithmetic_expression> - >
  <unary_arithmetic_expression>
| <additive_arithmetic_expression>

FIRST(<arithmetic_expression> = { (, +, -, AVID_T, FPL_T, INL_T})
*****************************************************************************/
void arithmetic_expression(void) {
#ifdef DEBUG 
	printf(">>> <arithmetic_expression>\n");
#endif
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS || (lookahead.attribute.arr_op == MINUS))
			unary_arithmetic_expression(); 
		gen_incode("PLATY: Arithmetic expression parsed");
		break;

	case LPR_T:
	case AVID_T:
	case FPL_T:
	case INL_T:
		additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;

	default: syn_printe();
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<unary_arithmetic_expression> ->
  - <primary_arithmetic_expression>
| + <primary_arithmetic_expression>

FIRST(<unary_arithmetic_expression> = { +, - })
*****************************************************************************/
void unary_arithmetic_expression(void) {
#ifdef DEBUG 
	printf(">>> <unary_arithmetic_expression>\n");
#endif
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS) {
			match(ART_OP_T, PLUS);
			primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
		}
		else if (lookahead.attribute.arr_op == MINUS) {
			match(ART_OP_T, MINUS);
			primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
		}
		break;

	default: syn_printe();
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<additive_arithmetic_expression> ->
<multiplicative_arithmetic_expression> <additive_arithmetic_expression_prime>

FIRST(<additive_arithmetic_expression> = { (, AVID_T, FPL_T, INL_T })
*****************************************************************************/
void additive_arithmetic_expression(void) {
#ifdef DEBUG 
	printf(">>> <additive_arithmetic_expression>\n");
#endif
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();

}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<additive_arithmetic_expression_prime> ->
  + <multiplicative_arithmetic_expression> <additive_arithmetic_expression_prime>
| - <multiplicative_arithmetic_expression> <additive_arithmetic_expression_prime> 
| ϵ

FIRST(<additive_arithmetic_expression> = { +, - })
*****************************************************************************/
void additive_arithmetic_expression_prime(void) {
#ifdef DEBUG 
	printf(">>> <additive_arithmetic_expression_prime>\n");
#endif
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS) {
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime(); 
			gen_incode("PLATY: Additive arithmetic expression parsed");
		}
		else if (lookahead.attribute.arr_op == MINUS) {
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
		}
		break;

	default: ;
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<multiplicative_arithmetic_expression> ->
<primary_arithmetic_expression> <multiplicative_arithmetic_expression_prime>

FIRST(<multiplicative_arithmetic_expression> = { (, AVID_T, FPL_T, INL_T })
*****************************************************************************/
void multiplicative_arithmetic_expression(void) {
#ifdef DEBUG 
	printf(">>> <multiplicative_arithmetic_expression>\n");
#endif
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();

}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<multiplicative_arithmetic_expression_prime> ->
  * <primary_arithmetic_expression> multiplicative_arithmetic_expression_prime>
| / <primary_arithmetic_expression> multiplicative_arithmetic_expression_prime> 
| ϵ

FIRST(<multiplicative_arithmetic_expression_prime> = { *, /, ϵ })
*****************************************************************************/
void multiplicative_arithmetic_expression_prime(void) {
#ifdef DEBUG 
	printf(">>> <multiplicative_arithmetic_expression_prime>\n");
#endif
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT) {
			match(ART_OP_T, MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		}
		else if (lookahead.attribute.arr_op == DIV) {
			match(ART_OP_T, DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		}
		break;

	default: ;
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<primary_arithmetic_expression> ->
  AVID_T
| FPL_T
| INL_T
| ( <arithmetic_expression> )

FIRST(<primary_arithmetic_expression> = { (, AVID_T, FPL_T, INL_T })
*****************************************************************************/
void primary_arithmetic_expression(void) {
#ifdef DEBUG 
	printf(">>> <primary_arithmetic_expression>\n");
#endif
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR); 
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;

	case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;

	case INL_T:
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;

	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;

	default: syn_printe();
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<string_expression> ->
<primary_string_expression> <string_expression_prime>

FIRST(<string_expression> = { STR_T, SVID_T })
*****************************************************************************/
void string_expression(void) {
#ifdef DEBUG 
	printf(">>> <string_expression>\n");
#endif
	primary_string_expression(); 
	string_expression_prime();
	gen_incode("PLATY: String expression parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<string_expression_prime> -> 
# <primary_string_expression> <string_expression_prime> | ϵ

FIRST(<string_expression_prime> = { #, ϵ })
*****************************************************************************/
void string_expression_prime(void) {
#ifdef DEBUG 
	printf(">>> <string_expression_prime>\n");
#endif
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_prime();
		break;

	default: ;
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<primary_string_expression> ->
  SVID_T
| STR_T

FIRST(<primary_string_expression> = { STR_T, SVID_T })
*****************************************************************************/
void primary_string_expression(void) {
#ifdef DEBUG 
	printf(">>> <primary_string_expression>\n");
#endif
	switch (lookahead.code)
	{
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;

	case STR_T:
		match(STR_T, NO_ATTR);
		break;

	default: syn_printe();
	}

	gen_incode("PLATY: Primary string expression parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<conditional_expression> ->
<logical_OR_expression>

FIRST(<conditional_expression> = { AVID_T, FPL_T, INL_T, STR_T, SVID_T })
*****************************************************************************/
void conditional_expression(void) {
#ifdef DEBUG 
	printf(">>> <conditional_expression>\n");
#endif
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<logical_OR_expression> ->
<logical_AND_expression> <logical_OR_expression_prime>

FIRST(<logical_OR_expression> = { AVID_T, FPL_T, INL_T, STR_T, SVID_T })
*****************************************************************************/
void logical_OR_expression(void) {
#ifdef DEBUG 
	printf(">>> <logical_OR_expression>\n");
#endif
	logical_AND_expression();
	logical_OR_expression_prime();
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<logical_OR_expression_prime> -> 
 .OR. <logical_AND_expression> <logical_OR_expression_prime> 
| ϵ

FIRST(<logical_OR_expression_prime> = { .OR. , ϵ })
*****************************************************************************/
void logical_OR_expression_prime(void) {
#ifdef DEBUG 
	printf(">>> <logical_OR_expression_prime>\n");
#endif
	switch (lookahead.code)
	{
	case LOG_OP_T:
		if (lookahead.attribute.log_op == OR) {
			match(LOG_OP_T, OR);
			logical_AND_expression();
			logical_OR_expression_prime();
			gen_incode("PLATY: Logical OR expression parsed");
		}
		break;

	default: ;
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<logical_AND_expression> ->
<relational_expression> <logical_AND_expression_prime>

FIRST(<logical_AND_expression> = { AVID_T, FPL_T, INL_T, STR_T, SVID_T })
*****************************************************************************/
void logical_AND_expression(void) {
#ifdef DEBUG 
	printf(">>> <logical_AND_expression>\n");
#endif
	relational_expression();
	logical_AND_expression_prime();
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<logical_AND_expression_prime> -> 
 .AND. <relational_expression> <logical_AND_expression_prime> 
| ϵ

FIRST(<logical_AND_expression_prime> = { .AND., ϵ })
*****************************************************************************/
void logical_AND_expression_prime(void) {
#ifdef DEBUG 
	printf(">>> <logical_AND_expression_prime>\n");
#endif
	switch (lookahead.code)
	{
	case LOG_OP_T:
		if (lookahead.attribute.log_op == AND){
			match(LOG_OP_T, AND);
			relational_expression();
			logical_AND_expression_prime();
			gen_incode("PLATY: Logical AND expression parsed");
		}
		break;

	default: ;
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<relational_expression> ->
  <primary_a_relational_expression> <arithmetic_relational_expression>
| <primary_s_relational_expression> <string_relational_expression>

FIRST(<relational_expression> = { AVID_T, FPL_T, INL_T, STR_T, SVID_T })
*****************************************************************************/
void relational_expression(void) {
#ifdef DEBUG 
	printf(">>> <relational_expression>\n");
#endif
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		arithmetic_relational_expression();
		break;

	case STR_T:
	case SVID_T:
		primary_s_relational_expression();
		string_relational_expression();
		
		break;

	default:
		syn_printe();

	}

	gen_incode("PLATY: Relational expression parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<arithmetic_relational_expression> ->
  == <primary_a_relational_expression>
| <> <primary_a_relational_expression>
| > <primary_a_relational_expression>
| < <primary_a_relational_expression>

FIRST(<arithmetic_relational_expression> = { <, <>, ==, > })
*****************************************************************************/
void arithmetic_relational_expression(void) {
#ifdef DEBUG 
	printf(">>> <arithmetic_relational_expression>\n");
#endif
	switch (lookahead.code)
	{
	case REL_OP_T:
		if (lookahead.attribute.rel_op == EQ) {
			match(REL_OP_T, EQ);
			primary_a_relational_expression();
		}else if (lookahead.attribute.rel_op == GT) {
			match(REL_OP_T, GT);
			primary_a_relational_expression();
		}else if (lookahead.attribute.rel_op == LT) {
			match(REL_OP_T, LT);
			primary_a_relational_expression();
		}else if (lookahead.attribute.rel_op == NE) {
			match(REL_OP_T, NE);
			primary_a_relational_expression();
		}
		break;

	default:
		syn_printe();
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<string_relational_expression> ->
  == <primary_s_relational_expression>
| <> <primary_s_relational_expression>
| > <primary_s_relational_expression>
| < <primary_s_relational_expression>

FIRST(<string_relational_expression> = { <, <>, ==, > })
*****************************************************************************/
void string_relational_expression(void){
#ifdef DEBUG 
	printf(">>> <string_relational_expression>\n");
#endif
	switch (lookahead.code)
	{
	case REL_OP_T:
		if (lookahead.attribute.rel_op == EQ) {
			match(REL_OP_T, EQ);
			primary_s_relational_expression();
		}else if (lookahead.attribute.rel_op == GT) {
			match(REL_OP_T, GT);
			primary_s_relational_expression();
		}else if (lookahead.attribute.rel_op == LT) {
			match(REL_OP_T, LT);
			primary_s_relational_expression();
		}else if (lookahead.attribute.rel_op == NE) {
			match(REL_OP_T, NE);
			primary_s_relational_expression();
		}
		break;

	default: syn_printe();
	}
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<primary_a_relational_expression> ->
  AVID_T
| FPL_T
| INL_T

FIRST(<primary_a_relational_expression> = { AVID_T, FPL_T, INL_T })
*****************************************************************************/
void primary_a_relational_expression(void) {
#ifdef DEBUG 
	printf(">>> <primary_a_relational_expression>\n", lookahead.code);
#endif
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR); break;
	case FPL_T:
		match(FPL_T, NO_ATTR); break;
	case INL_T:
		match(INL_T, NO_ATTR); break;

	default:
		syn_printe();
	}

	gen_incode("PLATY: Primary a_relational expression parsed");
}

/*****************************************************************************
Author:
-----------------------------------------------------------------------------
<primary_s_relational_expression> ->
<primary_string_expression>

FIRST(<primary_s_relational_expression> = { STR_T, SVID_T })
*****************************************************************************/
void primary_s_relational_expression(void) {
#ifdef DEBUG 
	printf(">>> <primary_s_relational_expression>\n");
#endif
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}