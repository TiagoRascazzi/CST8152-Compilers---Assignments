/* Filename: scanner.c
/* PURPOSE:
*    SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
*    as required for CST8152, Assignment #2
*    scanner_init() must be called before using the scanner.
*    The file is incomplete;
*    Provided by: Svillen Ranev
*    Version: 1.18.1
*    Date: 1 February 2018
*******************************************************************
*    REPLACE THIS HEADER WITH YOUR HEADER
*******************************************************************
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */
#include <math.h>

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */

/* This buffer is used as a repository for string literals. It is defined in platy_st.c */
extern Buffer * str_LTBL;	/*String literal table */
int line;					/* current line number of the source code */
extern int scerrnum;		/* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c);					/* character class function */
static int get_next_state(int, char, int *);	/* state machine function */
static int iskeyword(char * kw_lexeme);			/*keywords lookup functuion */
static long atolh(char * lexeme);				/* converts hexadecimal string to decimal value */

/*Initializes scanner */ /*TODO initialize scanner*/
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/

	/* in case the buffer has been read previously  */
	b_rewind(sc_buf);
	b_clear(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;
	/*   scerrnum = 0;  */
	/*no need - global ANSI C */
}

/* TODO this is the main part of the scanner */
Token malar_next_token(Buffer * sc_buf)
{
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */

	/* DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED */

	while (1) { /* endless loop broken by token returns it will generate a warning */

		/* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */
		c = b_getc(sc_buf);


		if (c == '!') { /*Chracter might be comment?*/
			/*b_mark(sc_buf, b_getcoffset(sc_buf)); /* mark buffer */

			c = b_getc(sc_buf); /* get next character */

			if (c == '!') { /* confirmed comment */
				while (c != '\n') {
					c = b_getc(sc_buf);
					printf("%c", c);
				}

				printf("\nComment found \n");
				continue;
			}
			else {
				t.code = ERR_T; /*What is our error?*/
				return t;
			}

		}

		/* Part 1: Implementation of token driven scanner */
		/* every token is possessed by its own dedicated code */

		/* WRITE YOUR CODE FOR PROCESSING THE SPECIAL CASES TOKENS HERE. */
		/* COMMENTS AND STRING LITERALS ARE ALSO PROCESSED HERE. */

		/* WHAT FOLLOWS IS A PSEUDO CODE.YOU CAN USE switch STATEMENT */
		/* INSTEAD OF if - else TO PROCESS THE SPECIAL CASES */
		/* DO NOT FORGET TO COUNT THE PROGRAM LINES */

		/* NOTE :
		 *	IF ILLEGAL CHARACTER IS FOUND THE SCANNER MUST RETURN AN ERROR TOKEN.
		 * 	ILLEGAL CHARACTER IS ONE THAT IS NOT DEFINED IN THE LANGUAGE SPECIFICATION
		 * 	OR IT IS OUT OF CONTEXT.
		 *	THE ILLEGAL CHAR IS THE ATTRIBUTE OF THE ERROR TOKEN
		 *	IN A CASE OF RUNTIME ERROR, THE FUNCTION MUST STORE
		 *	A NON - NEGATIVE NUMBER INTO THE GLOBAL VARIABLE scerrnum
		 *	AND RETURN A RUN TIME ERROR TOKEN.THE RUN TIME ERROR TOKEN ATTRIBUTE
		 *	MUST BE THE STRING "RUN TIME ERROR: " 
		 */

	/*
			IF(c == SOME CHARACTER)
			...
			SKIP CHARACTER(FOR EXAMPLE SPACE)
			continue;
		OR SET TOKEN(SET TOKEN CODE AND TOKEN ATTRIBUTE(IF AVAILABLE))
			return t;
	EXAMPLE:
		if (c == ' ') continue;
		if (c == '{') {
			t.code = RBR_T; /*no attribute * / return t;
			if (c == '+') {
				t.code = ART_OP_T; t.attribute.arr_op = PLUS * / return t;
				...

					IF(c == '.') TRY TO PROCESS.AND. or .OR.
					IF SOMETHING ELSE FOLLOWS.OR THE LAST.IS MISSING
					RETURN AN ERROR TOKEN
					IF(c == '!') TRY TO PROCESS COMMENT
					IF THE FOLLOWING CHAR IS NOT !REPORT AN ERROR
					ELSE IN A LOOP SKIP CHARACTERS UNTIL line terminator is found THEN continue;
				...
					IF STRING(FOR EXAMPLE, "text") IS FOUND
					SET MARK TO MARK THE BEGINNING OF THE STRING
					IF THE STRING IS LEGAL
					USING b_addc(..)COPY THE text FROM INPUT BUFFER INTO str_LTBL
					ADD '\0' at the end make the string C - type string
					SET STRING TOKEN
					(the attribute of the string token is the offset from
						the beginning of the str_LTBL char buffer to the beginning
						of the string(TEXT in the example))

					return t;
				ELSE
					THE STRING LITERAL IS ILLEGAL
					SET ERROR TOKEN FOR ILLEGAL STRING(see assignment)
					DO NOT STORE THE ILLEGAL STRINg IN THE str_LTBL

					return t;

				IF(c == ANOTHER CHARACTER)
					SET TOKEN
					return t;
	*/


		/* Part 2: Implementation of Finite State Machine (DFA)
		or Transition Table driven Scanner
		Note: Part 2 must follow Part 1
		*/

		/* SET THE MARK AT THE BEGINING OF THE LEXEME AND SAVE IT IN lexstart */
		lexstart = b_mark(sc_buf, /*UNKNOWN for now*/0);

		/* CODE YOUR FINATE STATE MACHINE HERE(FSM or DFA)
		/* IT IMPLEMENTS THE FOLLOWING ALGORITHM :

		/*
		FSM0.Begin with state = 0 and the input character c
			FSM1.Get the next state from the transition table calling
			state = get_next_state(state, c, &accept);
		FSM2.Get the next character
			FSM3.If the state is not accepting(accept == NOAS), go to step FSM1
			If the step is accepting, token is found, leave the machine and
			call an accepting function as described below.


			RETRACT  getc_offset IF THE FINAL STATE IS A RETRACTING FINAL STATE

			SET lexend TO getc_offset USING AN APPROPRIATE BUFFER FUNCTION

			CREATE  A TEMPORRARY LEXEME BUFFER HERE;
		lex_buf = b_allocate(...);
		.RETRACT getc_offset to the MARK SET PREVIOUSLY AT THE BEGINNING OF THE LEXEME AND
			.USING b_getc() COPY THE LEXEME BETWEEN lexstart AND lexend FROM THE INPUT BUFFER INTO lex_buf USING b_addc(...),
			.WHEN VID(KEYWORDS INCLUDED), FPL OR IL IS RECOGNIZED
			.YOU MUST CALL THE ACCEPTING FUNCTION USING THE ARRAY aa_table, WHICH
			.CONTAINS POINTERS TO FUNCTIONS.THE ARRAY INDEX OF THE FUNCTION TO BE
			.CALLED IS STORED IN THE VARIABLE state.
			.YOU ARE NOT ALLOWED TO CALL ANY OF THE ACCEPTING FUNCTIONS BY NAME.
			.THE ARGUMENT TO THE FUNCTION IS THE STRING STORED IN lex_buf.
			....
		*/

		/*Token t = aa_func03("thisisaveryveryverylongvid");*/

		b_free(lex_buf);
		t.code = SEOF_T; /*TODO remove testinf end of file???*/
		return t;

	}//end while(1)
}

/* 
 * DO NOT MODIFY THE CODE OF THIS FUNCTION
 * YOU CAN REMOVE THE COMMENTS
 */
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];

#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	assert(next != IS);
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}


/*
 * Return the column of the transistion table based on charracter
 */
int char_class(char c)
{
	if (c >= 'a' && c <= 'w' || c == 'y' || c == 'z' || c >= 'G' && c <= 'Z') return 0;
	if (c >= 'A' && c <= 'F')	return 1;
	if (c == '0') return 2;
	if (c >= '1' && c <= '9') return 3;
	if (c == '.') return 4;
	if (c == '$') return 5;
	if (c == 'x') return 6;
	return 7;
}


/**************************************************************
 * HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS. 
 **************************************************************/

/*
 * ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords (VID - AVID/KW)
 */
Token aa_func02(char *lexeme) {
	Token t;

	int keywordIndex = iskeyword(lexeme);
	if (keywordIndex != -1) {
		t.code = KW_T;
		t.attribute.kwt_idx = keywordIndex;
	}else {
		t.code = AVID_T;
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN);
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	return t;
}

/*
 * ACCEPTING FUNCTION FOR THE string variable identifier (VID - SVID)
 */
Token aa_func03(char *lexeme) {
	Token t;

	t.code = SVID_T;
	if (strlen(lexeme) > VID_LEN) {
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN-1);
		t.attribute.vid_lex[VID_LEN - 1] = '$';
	}else {
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN);
	}

	t.attribute.vid_lex[VID_LEN] = '\0';

	return t;
}

/*
 * ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant (DIL)
 */
Token aa_func05(char *lexeme) {
	Token t;

	if (isValidIL(lexeme)) {
		t.code = INL_T;
		t.attribute.int_value = atoi(lexeme);
	}else {
		t.code = ERR_T;
		strcpy(t.attribute.err_lex, errorLexemeFormat(lexeme));
	}

	return t;
}

/*
 * ACCEPTING FUNCTION FOR THE floating-point literal (FPL)
 */
Token aa_func08(char *lexeme) {
	Token t;

	/* THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
		WHICH IS THE ATTRIBUTE FOR THE TOKEN.
		THE VALUE MUST BE IN THE SAME RANGE AS the value of 4-byte float in C.
		IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
		THE ERROR TOKEN ATTRIBUTE IS  lexeme. IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
		STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C-type string. 
		BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE 
	*/

	return t;
}

/*
 * ACCEPTING FUNCTION FOR THE integer literal(IL) - hexadecimal constant (HIL)
 */
Token aa_func11(char *lexeme) {
	Token t;

	long hex = atolh(lexeme);
	int length = strlen(lexeme);

	/* THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING AN HEXADECIMAL CONSTANT
		TO A DECIMAL INTEGER VALUE WHICH IS THE ATTRIBUTE FOR THE TOKEN.
		THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte integer in C.
		THIS FUNCTION IS SIMILAR TO THE FUNCTION ABOVE 
		THE MAIN DIFFERENCE IE THAT THIS FUNCTION CALLS
		THE FUNCTION atolh(char * lexeme) WHICH CONVERTS AN ASCII STRING
		REPRESENTING AN HEXADECIMAL NUMBER TO INTEGER VALUE
		IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
		THE ERROR TOKEN ATTRIBUTE IS  lexeme. IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
		STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C-type string. 
		BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE
	*/

	return t;
}

/*
 * ACCEPTING FUNCTION FOR THE ERROR TOKEN 
 */
Token aa_func12(char *lexeme) {
	Token t;

	/* THE FUNCTION SETS THE ERROR TOKEN. lexeme[] CONTAINS THE ERROR
		THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme ITSELF
		AND IT MUST BE STORED in err_lex. IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
		STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C-type string. 
		BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE
	*/

	return t;
}	// ER

/*
 *
 */
Token aa_func13(char *lexeme) {
	Token t;

	/*  */

	return t;
}	// ES
	

/**************************************************************
 * CONVERSION FUNCTIONS
 **************************************************************/
long atolh(char * lexeme) {
	/* THE FUNCTION CONVERTS AN ASCII STRING REPRESENTING AN HEXADECIMAL INTEGER CONSTANT TO INTEGER VALUE */
	/* If no valid conversion could be performed, it returns zero */
	if (lexeme[0] != '0')
		return 0;
	long l = 0;
	for (int i = 0; i < strlen(lexeme); i++) {
		char nextChar = lexeme[strlen(lexeme)-i] ;

		int digit = 0;
		switch (nextChar) {
		case '1': digit = 1; break;
		case '2': digit = 2; break;
		case '3': digit = 3; break;
		case '4': digit = 4; break;
		case '5': digit = 5; break;
		case '6': digit = 6; break;
		case '7': digit = 7; break;
		case '8': digit = 8; break;
		case '9': digit = 9; break;
		case 'A': digit = 10; break;
		case 'B': digit = 11; break;
		case 'C': digit = 12; break;
		case 'D': digit = 13; break;
		case 'E': digit = 14; break;
		case 'F': digit = 15; break;
		case 'x': return l;
		default: return 0;
		}
		int p = 16;
		if (i == 0) p = 1;
		for (int j = 0; j < i-1; j++)
			p *= p;
		l += digit * p;
	}
	return 0;
}

/**************************************************************
 * HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS(IF ANY).
 **************************************************************/
int iskeyword(char * kw_lexeme) {
	for (int i = 0; i < KWT_SIZE; ++i) {
		if (strcmp(kw_lexeme, kw_table[i]) == 0) {
			return i;
		}
	}
	return -1;
}
int isValidIL(char* lexeme) {
	/* 
	THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte integer in C.
	*/
}

char* errorLexemeFormat(char* er_lexeme) {
	/* IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
		THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
		STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C - type string. */
	return er_lexeme;
}