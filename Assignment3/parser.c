#include "buffer.h"
#include "token.h"
#include "parser.h"

//#define	DEBUG
//#define MATCH_DEBUG

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

	//TODO GET RID OF THIS
#ifdef MATCH_DEBUG
	printf("In match function with t_code %s and attribute code %s\n", tokenCodeToString(pr_token_code),
		tokenAttributeToString(lookahead));
#endif

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

//TODO REMOVE
char* tokenCodeToString(int code) {
	switch (code) {
	case 0: return "ERR_T";
	case 1: return "SEOF_T";
	case 2: return "AVID_T";
	case 3: return "SVID_T";
	case 4: return "FPL_T";
	case 5: return "INL_T";
	case 6: return "STR_T";
	case 7: return "SCC_OP_T";
	case 8: return "ASS_OP_T";
	case 9: return "ART_OP_T";
	case 10: return "REL_OP_T";
	case 11: return "LOG_OP_T";
	case 12: return "LPR_T";
	case 13: return "RPR_T";
	case 14: return "LBR_T";
	case 15: return "RBR_T";
	case 16: return "KW_T";
	case 17: return "COM_T";
	case 18: return "EOS_T";
	default: return "NA";
	}
}

//TODO REMOVE
char* tokenAttributeToString(Token lookahead) {
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.kwt_idx) {
		case 0: return "ELSE";
		case 1: return "FALSE";
		case 2: return "IF";
		case 3: return "PLATYPUS";
		case 4: return "READ";
		case 5: return "REPEAT";
		case 6: return "THEN";
		case 7: return "TRUE";
		case 8: return "WHILE";
		case 9: return "WRITE";

		}
		break;
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case 0: return "PLUS";
		case 1: return "MINUS";
		case 2: return "MULT";
		case 3: return "DIV";
		}
		break;
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case 0: return "AND";
		case 1: return "OR";
		}
		break;
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) {
		case 0: return "EQ";
		case 1: return "NE";
		case 2: return "GT";
		case 3: return "LT";
		}
	default: return "NO_ATTR";
	}
}

void syn_eh(int sync_token_code) {
	syn_printe();
	++synerrno;

	while (lookahead.code != sync_token_code) { //TODO not sure if only the semicolon

		if (sync_token_code != SEOF_T && lookahead.code == SEOF_T) {
			exit(synerrno);
		}
		lookahead = malar_next_token(sc_buf);
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
#ifdef DEBUG 
		printf(">>> <program>\n"); 
#endif
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

void statements(void) {
#ifdef DEBUG 
	printf(">>> <statements>\n");
#endif
	statement();
	statements_prime();
}

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

	default: ; //TODO can remove all the empty default
	}
}

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

void assignment_statement(void){
#ifdef DEBUG 
	printf(">>> <assignment_statement>\n");
#endif
	assignment_expression();
	match(EOS_T, NO_ATTR); 
	gen_incode("PLATY: Assignment statement parsed");
}

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

void variable_list(void) {
#ifdef DEBUG 
	printf(">>> <variable_list>\n");
#endif
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}

void variable_list_prime(void) {
#ifdef DEBUG 
	printf(">>> <variable_list_prime>\n");
#endif
	//TODO maybe change to if
	switch (lookahead.code)
	{
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_prime();
		break;

	default: ;
	}
}

void opt_variable_list(void) {
#ifdef DEBUG 
	printf(">>> <opt_variable_list>\n");
#endif
	//TODO maybe change to if
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		variable_list();
		break;

	default: ;
	}
}

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

void additive_arithmetic_expression(void) {
#ifdef DEBUG 
	printf(">>> <additive_arithmetic_expression>\n");
#endif
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();
	//gen_incode("PLATY: Additive arithmetic expression parsed");

}

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

void multiplicative_arithmetic_expression(void) {
#ifdef DEBUG 
	printf(">>> <multiplicative_arithmetic_expression>\n");
#endif
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();
	//gen_incode("PLATY: Multiplicative arithmetic expression parsed");

}

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

void string_expression(void) {
#ifdef DEBUG 
	printf(">>> <string_expression>\n");
#endif
	primary_string_expression(); 
	string_expression_prime();
	gen_incode("PLATY: String expression parsed");
}

void string_expression_prime(void) {
#ifdef DEBUG 
	printf(">>> <string_expression_prime>\n");
#endif
	//TODO maybe change to if
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

void conditional_expression(void) {
#ifdef DEBUG 
	printf(">>> <conditional_expression>\n");
#endif
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

void logical_OR_expression(void) {
#ifdef DEBUG 
	printf(">>> <logical_OR_expression>\n");
#endif
	logical_AND_expression();
	logical_OR_expression_prime();
	//gen_incode("PLATY: Logical OR expression parsed");
}

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

void logical_AND_expression(void) {
#ifdef DEBUG 
	printf(">>> <logical_AND_expression>\n");
#endif
	relational_expression();
	logical_AND_expression_prime();
	//gen_incode("PLATY: Logical AND expression parsed");
}

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
		//gen_incode("PLATY: Relational expression parsed");
		syn_printe();

	}

	gen_incode("PLATY: Relational expression parsed");
}

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
		}//TODO maybe call synprinte() in else
		break;

	default:
		syn_printe();
	}
}

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
		}//TODO maybe call synprinte() in else
		break;

	default: syn_printe();
	}
}

void primary_a_relational_expression(void) {
#ifdef DEBUG 
	printf(">>> <primary_a_relational_expression> DEBUG: %d\n", lookahead.code);
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
		//gen_incode("PLATY: Primary a_relational expression parsed"); // TODO figure out why this fixes ass3w
	}

	gen_incode("PLATY: Primary a_relational expression parsed");
}

void primary_s_relational_expression(void) {
#ifdef DEBUG 
	printf(">>> <primary_s_relational_expression>\n");
#endif
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}
