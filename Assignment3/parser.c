#include "token.h"
#include "buffer.h"
#include "parser.h"

static Token lookahead;
static Buffer sc_buf;
static int synerrno;

/* Add global variables needed here*/

/*---------------------------------*/


/*
void parser(Buffer * in_buf) {
	sc_buf = in_buf;
	lookahead = malar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}
*/