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

	if (lookahead.code != pr_token_code) {
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

	while (lookahead.code != sync_token_code) { //TODO not sure if only the semicolon
		lookahead = malar_next_token(sc_buf);

		if (sync_token_code != SEOF_T && lookahead.code == SEOF_T) {
			exit(synerrno);
		}
	}

	if (lookahead.code != SEOF_T)
		lookahead = malar_next_token(sc_buf); //TODO maybe check for ERR_T

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

void program(void) {
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

void statements(void) {
	statement();
	statements_prime();
}

void statements_prime(void) {
	/* FIRST set: { AVID_T, SVID_T, KW_T(IF, WHILE, READ, WRITE), epsilon} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statement(); statements_prime();  break;
	case KW_T:
		/* check for IF, WHILE, READ, WRITE */
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statement(); statements_prime();
			break;
		}

	default: ;/*empty string � optional statements - epsilon*/
		//gen_incode("PLATY: statements_prime parsed");
		//TODO not sure if need statements_prime
	}
}

void opt_statements() {
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not � see above),e} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here
		and in statements_p()*/
		//TODO not sure if inverted is better
		/*if (lookahead.attribute.get_int != PLATYPUS
		&& lookahead.attribute.get_int != ELSE
		&& lookahead.attribute.get_int != THEN
		&& lookahead.attribute.get_int != REPEAT
		&& lookahead.attribute.get_int != TRUE
		&& lookahead.attribute.get_int != FALSE) {
		statements();
		break;
		}*/

		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();
			break;
		}

	default: /*empty string � optional statements*/
		gen_incode("PLATY: Opt_statements parsed");
	}
}

void statement(void) {
	/* FIRST set: { AVID_T, SVID_T, KW_T(IF, WHILE, READ, WRITE) } */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: assignment_statement();  break;
	case KW_T:
		/* check for IF, WHILE, READ, WRITE */
		if (lookahead.attribute.get_int == IF) {
			selection_statement();  break;
		}else if (lookahead.attribute.get_int == WHILE) {
			iteration_statement();  break;
		}else if (lookahead.attribute.get_int == READ) {
			input_statement();  break;
		}else if (lookahead.attribute.get_int == WRITE) {
			output_statement();  break;
		}

	default: /*empty string � optional statements - epsilon*/
		syn_printe(); //TODO not sure if need to print gen_incode
	}
}

void assignment_statement(void) {
	assignment_expression();
	match(EOS_T, NO_ATTR); 
	gen_incode("PLATY: Assignment statement parsed");
}

void assignment_expression(void) {
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
	default: /*empty string � optional statements - epsilon*/
		syn_printe(); //TODO not sure if need to print gen_incode
	}
}

void selection_statement(void) {
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

void iteration_statement(void) {
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

void pre_condition(void) {
	switch (lookahead.code)
	{
	case KW_T:
		if (lookahead.attribute.get_int == TRUE)
			match(KW_T, TRUE);
		else if (lookahead.attribute.get_int == FALSE)
			match(KW_T, FALSE);

	default:
		syn_printe();
	}
}

void input_statement(void) {
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

void variable_list(void) {
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}

void variable_list_prime(void) {
	switch (lookahead.code)
	{
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_prime();
		break;

	default: ;
		//gen_incode("PLATY: variable_list_prime parsed");
		//TODO check grammar if variable list prime is needed
	}
}

void opt_variable_list(void) {
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		variable_list();
		break;

	default:
		gen_incode("PLATY: opt_variable_list parsed");
	}
}

void variable_identifier(void) {
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;

	default:
		syn_printe();
	}
}

void output_statement(void) {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_statement_argument();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

void output_statement_argument(void) {
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T:
			opt_variable_list();
			//TODO Check epsiolon??
			gen_incode("PLATY: Output list (1) parsed");
			break;
		case STR_T:
			match(STR_T, NO_ATTR);
			gen_incode("PLATY: Output list (2) parsed");
			break;
	default:
		syn_printe();
	}
}

void arithmetic_expression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
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

	default: /*empty string � optional statements - epsilon*/
		syn_printe(); //TODO not sure if need to print gen_incode
	}
}

void unary_arithmetic_expression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS) {
			match(ART_OP_T, PLUS);
			primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
			break;
		}
		else if (lookahead.attribute.arr_op == MINUS) {
			match(ART_OP_T, MINUS);
			primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
			break;
		}

	default: /*empty string � optional statements - epsilon*/
		syn_printe(); //TODO not sure if need to print gen_incode
	}
}

void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();
	gen_incode("PLATY: Additive_arithmetic_expression parsed");

}

void additive_arithmetic_expression_prime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS) {
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			break;
		}
		else if (lookahead.attribute.arr_op == MINUS) {
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			break;
		}

	default: ;/*empty string � optional statements - epsilon*/
		//gen_incode("PLATY: Additive_arithmetic_expression_prime parsed");
		//TODO not display prime
	}
}

void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();
	gen_incode("PLATY: Multiplicative arithmetic expression parsed");

}

void multiplicative_arithmetic_expression_prime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT) {
			match(ART_OP_T, MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			break;
		}
		else if (lookahead.attribute.arr_op == DIV) {
			match(ART_OP_T, DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			break;
		}

	default: ;/*empty string � optional statements - epsilon*/
		//gen_incode("PLATY: Multiplicative_arithmetic_expression_prime parsed");
		//TODO not display prime
	}
}

void primary_arithmetic_expression(void) {
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

	default: /*empty string � optional statements - epsilon*/
		syn_printe();
	}
}

void string_expression(void) {
	primary_string_expression(); 
	string_expression_prime();
}

void string_expression_prime(void) {
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_prime();
		break;

	default:
		gen_incode("PLATY: string_expression_prime parsed");
	}
}

void primary_string_expression(void) {
	switch (lookahead.code)
	{
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
	default:
		syn_printe();
	}
}

void conditional_expression(void) {
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

void logical_OR_expression(void) {
	logical_AND_expression();
	logical_OR_expression_prime();
	gen_incode("PLATY: Logical OR expression parsed");
}

void logical_OR_expression_prime(void) {
	switch (lookahead.code)
	{
	case LOG_OP_T:
		match(LOG_OP_T, OR);
		logical_AND_expression();
		logical_OR_expression_prime();

	default: ;
		//gen_incode("PLATY: logical_OR_expression_prime parsed");
		//TODO not display prime
	}
}

void logical_AND_expression(void) {
	relational_expression();
	logical_AND_expression_prime();
	gen_incode("PLATY: Logical AND expression parsed");
}

void logical_AND_expression_prime(void) {
	switch (lookahead.code)
	{
	case LOG_OP_T:
		match(LOG_OP_T, AND);
		relational_expression();
		logical_AND_expression_prime();

	default: ;
		//gen_incode("PLATY: logical_AND_expression_prime parsed");
		//TODO not display prime
	}
}

void relational_expression(void) {
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		arithmetic_relational_expression();
		gen_incode("PLATY: Relational expression parsed");
		break;

	case STR_T:
	case SVID_T:
		primary_s_relational_expression();
		string_relational_expression();
		gen_incode("PLATY: Relational expression parsed");
		break;

	default:
		syn_printe();
	}
}

void arithmetic_relational_expression(void) {
	switch (lookahead.code)
	{
	case REL_OP_T:
		if (lookahead.attribute.rel_op == EQ)
			match(REL_OP_T, EQ);
		else if (lookahead.attribute.rel_op == GT)
			match(REL_OP_T, GT);
		else if (lookahead.attribute.rel_op == LT)
			match(REL_OP_T, LT);
		else if (lookahead.attribute.rel_op == NE)
			match(REL_OP_T, NE);
		primary_a_relational_expression();
		break;

	default:
		syn_printe();
	}
}

void string_relational_expression(void) {
	switch (lookahead.code)
	{
	case REL_OP_T:
		if (lookahead.attribute.rel_op == EQ)
			match(REL_OP_T, EQ);
		else if (lookahead.attribute.rel_op == GT)
			match(REL_OP_T, GT);
		else if (lookahead.attribute.rel_op == LT)
			match(REL_OP_T, LT);
		else if (lookahead.attribute.rel_op == NE)
			match(REL_OP_T, NE);
		primary_s_relational_expression();
		break;

	default:
		syn_printe();
	}
}

void primary_a_relational_expression(void) {
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;

	case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;

	case INL_T:
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;

	default:
		syn_printe();
	}
}

void primary_s_relational_expression(void) {
	switch (lookahead.code)
	{
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Primary s_relational expression parsed");
		break;

	case SVID_T:
		match(SVID_T, NO_ATTR);
		gen_incode("PLATY: Primary s_relational expression parsed");
		break;

	default:
		syn_printe();
	}
}
