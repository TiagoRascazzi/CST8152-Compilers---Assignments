/*
* File name: scanner.c
* Compiler: MS Visual Studio 2015, gcc
* Author: Tiago Donchegay, 040867850
*	Nicholas Richer, 040828783
* Course: CST8152_010 Compilers
* Assignment: 2
* Date: March 23rd 2018
* Professor: Svillen Ranev
* Purpose: Drives almost all functionality of scanner, by 
*	initiating and reading lexeme formatted strings to tokens.
*
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
static Buffer *lex_buf;		/*pointer to temporary lexeme buffer*/

/* scanner.c static(local) function  prototypes */
static int char_class(char c);					/* character class function */
static int get_next_state(int, char, int *);	/* state machine function */
static int iskeyword(char * kw_lexeme);			/*keywords lookup functuion */
static long atolh(char * lexeme);				/* converts hexadecimal string to decimal value */
static int isValidIL(char* lexeme);				/* Checks if Integer literal is valid */
static int isValidFPL(char* lexeme);			/* Checks if Floating Point Literal is valid */
static int isValidHIL(char* lexeme);			/* Checks if Hexidecimal Literal is valid */

/*Initializes scanner */
int scanner_init(Buffer * sc_buf) {

	if (sc_buf == NULL || b_isempty(sc_buf)) return EXIT_FAILURE;

	/* in case the buffer has been read previously  */
	b_rewind(sc_buf);
	b_clear(str_LTBL);
	line = 1;
	scerrnum = 0;
	return EXIT_SUCCESS;
	/*no need - global ANSI C */
}

/*
* Purpose: Find the next token in the buffer
* Author: Nicholas Richer, Tiago Donchegay
* Versions: 1.0
* Called functions: b_getc(), printf(),
* Parameters:
*		sc_buf:	The buffer containing the source file to scan
* Return: The next token that was matched
*/
Token malar_next_token(Buffer * sc_buf)
{
	Token t = { 0 };	/* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c;	/* input symbol */
	int state = 0;		/* initial state of the FSM */
	short lexstart;		/* start offset of a lexeme in the input char buffer (array) */
	short lexend;		/* end offset of a lexeme in the input char buffer (array) */
	int accept = NOAS;	/* type of state - initially not accepting */

	/*Local variables*/
	int errBufStart = 0;
	int errBufEnd = 0;
	int i = 0;
	Buffer* errBuf = NULL;


	/*
	if (sc_buf == NULL) {
	 Generate a runtime error token
	t.code = RTE_T;
	strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
	scerrnum = 100;
	return t;
	*/


	while (1) { /* endless loop broken by token returns */

		/* get the next symbol from the buffer */
		c = b_getc(sc_buf);

		switch (c) {
		case ' ':
		case '\t':
		case '\v':
		case '\f':
		case '\r':
			continue;					/* Ignore the whitespaces */
		case '\n': ++line; continue;	/* Count the lines */
		case SEOF1:
		case SEOF2:
			t.code = SEOF_T; /* no attribute */ return t;					/*Check end of file 							  */
		case '(': t.code = LPR_T; /* no attribute */ return t;				/*Check for left parenthese						  */
		case ')': t.code = RPR_T; /* no attribute */ return t;				/*Check for right parenthese					  */
		case '{': t.code = LBR_T; /* no attribute */ return t;				/*Check for left braket							  */
		case '}': t.code = RBR_T; /* no attribute */ return t;				/*Check for right braket						  */
		case ',': t.code = COM_T; /* no attribute */ return t;				/*Check for comma								  */
		case ';': t.code = EOS_T; /* no attribute */ return t;				/*Check for semicolon							  */
		case '#': t.code = SCC_OP_T; /* no attribute */ return t;			/*Check for hash character						  */
		case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;	/*Check for plus arithmetic operator			  */
		case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t;	/*Check for minus arithmetic operator			  */
		case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t;	/*Check for multiplication arithmetic operator	  */
		case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t;	/*Check for division arithmetic operator		  */
																			
		case '>': t.code = REL_OP_T; t.attribute.rel_op = GT; return t;		/*Check for the greater then relational operator  */

		case '<':
		{
			b_mark(sc_buf, b_getcoffset(sc_buf));
			switch (b_getc(sc_buf)) {
			case '>': t.code = REL_OP_T; t.attribute.rel_op = NE; return t;						/*Check for the not equal relational operator */
			default: b_reset(sc_buf); t.code = REL_OP_T; t.attribute.rel_op = LT; return t;		/*Check for the lesser then relational operator */
			}
		}
		case '=':
		{
			b_mark(sc_buf, b_getcoffset(sc_buf));
			switch (b_getc(sc_buf)) {
			case '=': t.code = REL_OP_T; t.attribute.rel_op = EQ; return t;				/* Check for the equal relational operator */
			default: b_reset(sc_buf); t.code = ASS_OP_T; /*no attribute */ return t;	/* Check for the assignment operator */
			}
		}

		case '.': /* Logical operator */
		{
			b_mark(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);

			if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.' ) {
				t.code = LOG_OP_T; t.attribute.log_op = AND; return t;	/* Check for the logical operator and */
			}else if(c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.' ) {
				t.code = LOG_OP_T; t.attribute.log_op = OR; return t;	/* Check for the logical operator or */
			}else {
				/* If invalid logical operator */
				t.code = ERR_T;
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';

				b_reset(sc_buf);
				return t;
			}
		}

		case '"': /* String literal */
		{
			b_mark(sc_buf, b_getcoffset(sc_buf));

			errBufStart = b_getcoffset(sc_buf);
			/* loop until the next character is the closing quote */
			while ((c = b_getc(sc_buf)) != '"') {
				if (c == SEOF1 || c == SEOF2) {
					/* if the end of the file is reached */
					errBufEnd = b_getcoffset(sc_buf);

					/* Generate a buffer to hold the invalid string */
					errBuf = b_allocate(errBufEnd - errBufStart + 1, 0, 'f');

					/* Check if the buffer is null */
					if (errBuf == NULL) {
						/* Generate a runtime error token */
						t.code = RTE_T;
						strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
						scerrnum = 101;
						return t;
					}

					b_reset(sc_buf);

					/* Copy the string to the error buffer */
					b_addc(errBuf, '"'); 
					for (i = errBufStart; i < errBufEnd; i++) {
						c = b_getc(sc_buf);
						b_addc(errBuf, c);
					}
					b_addc(errBuf, '\0');

					/* generate an error token */
					t = aa_func12(b_location(errBuf, 0));
					b_free(errBuf);

					return t;
				}
			}

			b_reset(sc_buf);
			t.attribute.str_offset = b_limit(str_LTBL);
			while ( ( c = b_getc(sc_buf) ) != '"') b_addc(str_LTBL, c);
			b_addc(str_LTBL, '\0');

			t.code = STR_T;
			return t;
		}


		case '!': /* Character might be comment */
		{
			b_mark(sc_buf, b_getcoffset(sc_buf)); /* mark buffer */

			if (b_getc(sc_buf) == '!') {
				/* confirmed comment */
				while ((c = b_getc(sc_buf)) != '\n' && c != SEOF1 && c != SEOF2);
				b_retract(sc_buf);
				continue;
			}else{
				/* invalid comment */
				b_retract(sc_buf);
				t.code = ERR_T;

				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = (c = b_getc(sc_buf));
				t.attribute.err_lex[2] = '\0';

				while ((c = b_getc(sc_buf)) != '\n' && c != SEOF1 && c != SEOF2);
				b_retract(sc_buf);
				return t;
			}
			break;
		}


		}

		/* Part 2: Implementation of Finite State Machine (DFA)
		 * or Transition Table driven Scanner
		 */

		/* set the mark and lexstart at the begining of the lexeme */
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);

		/* get the next state until a accepting state */
		state = get_next_state(state, c, &accept);
		while (accept == NOAS)
			state = get_next_state(state, b_getc(sc_buf), &accept);

		/* retract if needed */
		if (accept == ASWR || accept == ER)
			b_retract(sc_buf);
		
		/* set the lexend at the end of the lexeme */
		lexend = b_getcoffset(sc_buf);

		/* create a temporary lexeme buffer */
		lex_buf = b_allocate( lexend - lexstart + 1, 0, 'f');

		/* Check if the buffer is null */
		if (lex_buf == NULL) {
			/* Generate a runtime error token */
			t.code = RTE_T;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			scerrnum = 102;
			return t;
		}

		b_reset(sc_buf);

		/* Copy the lexeme to the lexeme buffer */
		for (i = lexstart; i < lexend; i++)
			b_addc(lex_buf, b_getc(sc_buf));
		b_addc(lex_buf, '\0');

		/* Call the correct for the accepting state */
		t = (*aa_table[state])(b_location(lex_buf, 0) );

		/* free the buffer */
		b_free(lex_buf);
		return t;
	}
}

/*
* DO NOT MODIFY THE CODE OF THIS FUNCTION
* YOU CAN REMOVE THE COMMENTS
*/
int get_next_state(int state, char c, int *accept) {
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
* Purpose: Match the column of the transistion table to a given character
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		c:	The charactor to match to a column
* Return: The appropriate column number of the transistion table
*/
int char_class(char c) {
	if ((c >= 'a' && c <= 'w') || c == 'y' || c == 'z' || (c >= 'G' && c <= 'Z')) return 0;
	if (c >= 'A' && c <= 'F')	return 1;
	if (c == '0') return 2;
	if (c >= '1' && c <= '9') return 3;
	if (c == '.') return 4;
	if (c == '$') return 5;
	if (c == 'x') return 6;
	return 7;
}


/*
* Purpose: ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords (VID - AVID/KW)
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: iskeyword(), strncpy()
* Parameters:
*		lexeme:	The lexeme to convert to AVID/KW token
* Return: The appropriate token based on lexeme
*/
Token aa_func02(char *lexeme) {
	Token t = { 0 };

	/* get if/which keyword is the lexeme */
	int keywordIndex = iskeyword(lexeme);
	if (keywordIndex != -1) {
		/* Generate the correct keyword token */
		t.code = KW_T;
		t.attribute.kwt_idx = keywordIndex;
	}
	else {
		/* Generate a AVID token */
		t.code = AVID_T;
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN);
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	return t;
}


/*
* Purpose: ACCEPTING FUNCTION FOR THE string variable identifier (VID - SVID)
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: strlen(), strncpy()
* Parameters:
*		lexeme:	The lexeme to convert to SVID token
* Return: Return SVID token with formated name
*/
Token aa_func03(char *lexeme) {
	Token t = { 0 };
	
	/* Genarate SVID token */
	t.code = SVID_T;
	if (strlen(lexeme) > VID_LEN){
		/* if it is too long format lexeme */
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN - 1);
		t.attribute.vid_lex[VID_LEN - 1] = '$';
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else {
		strcpy(t.attribute.vid_lex, lexeme);
	}
	return t;
}

/*
* Purpose: ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant (DIL)
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: isValidIL(), atoi(), strcpy(), errorLexemeFormat()
* Parameters:
*		lexeme:	The lexeme to convert to DIL token
* Return: Return DIL token exept if out of range it return an error token
*/
Token aa_func05(char *lexeme) {
	Token t = { 0 };

	if (isValidIL(lexeme)) {
		/* Generate INT token */
		t.code = INL_T;
		t.attribute.int_value = atoi(lexeme);
	}else {
		/* Generate error token */
		return aa_func12(lexeme);
	}
	return t;
}


/*
* Purpose: ACCEPTING FUNCTION FOR THE floating-point literal (FPL)
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: isValidFPL(), atof(), strcpy(), errorLexemeFormat()
* Parameters:
*		lexeme:	The lexeme to convert to FPL token
* Return: Return FPL token exept if out of range it return an error token
*/
Token aa_func08(char *lexeme) {
	Token t = { 0 };

	if (isValidFPL(lexeme)){
		/* Generate FPL token */
		t.code = FPL_T;
		t.attribute.flt_value = atof(lexeme);
	}else {
		/* Generate error token */
		return aa_func12(lexeme);
	}
	return t;
}

/*
* Purpose: ACCEPTING FUNCTION FOR THE integer literal(IL) - hexadecimal constant (HIL)
* Author: Nicholas Richer
* Versions: 1.0
* Called functions:
* Parameters:
*		lexeme:	The lexeme to convert to HIL token
* Return: Return HIL token exept if out of range it return an error token
*/
Token aa_func11(char *lexeme) {
	Token t = { 0 };

	if (isValidHIL(lexeme)) {
		/* Generate INT token */
		t.code = INL_T;
		t.attribute.int_value = atolh(lexeme);
	}else {
		/* Generate error token */
		return aa_func12(lexeme);
	}
	return t;
}


/*
* Purpose: ACCEPTING FUNCTION FOR THE ERROR TOKEN with no retract
* Author: Nicholas Heggart-Richer
* Versions: 1.0
* Called functions: strlen(), strncpy()
* Parameters:
*		lexeme:	The lexeme to convert to ERROR token
* Return:
*/
Token aa_func12(char *lexeme) {
	Token t = { 0 };

	/* Genarate error token */
	t.code = ERR_T;
	if (strlen(lexeme) > ERR_LEN) {
		/* Format the lexeme if too long */
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else {
		strcpy(t.attribute.err_lex, lexeme);
	}
	return t;
}

/*
* Purpose: ACCEPTING FUNCTION FOR THE ERROR TOKEN with retract
* Author: Nicholas Heggart-Richer
* Versions: 1.0
* Called functions: aa_func12()
* Parameters:
*		lexeme:	The lexeme to convert to ERROR token
* Return:
*/
Token aa_func13(char *lexeme) {
	return aa_func12(lexeme);
}


/**************************************************************
* CONVERSION FUNCTIONS
**************************************************************/

/*
* Purpose: CONVERTS AN ASCII STRING REPRESENTING AN HEXADECIMAL INTEGER CONSTANT TO INTEGER VALUE
* Author: Nicholas Heggart-Richer
* Versions: 1.0
* Called functions: strtol()
* Parameters:
*		lexeme:	The lexeme to convert to Integer value
* Return:
*		The value calculated based on lexeme
*/
long atolh(char * lexeme) {
	return strtol(lexeme, NULL, 0);
}

/**************************************************************
* HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS(IF ANY).
**************************************************************/

/*
* Purpose: Get the index of the keyword lexeme
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: strcmp()
* Parameters:
*		lexeme:	The keyword lexeme to get id from
* Return:
*		The index of the keyword lexem. return -1 if invalid
*/
int iskeyword(char * kw_lexeme) {
	int i;
	/* loop througt all the keyword to try to find it */
	for (i = 0; i < KWT_SIZE; ++i) {
		if (strcmp(kw_lexeme, kw_table[i]) == 0) {
			return i;
		}
	}
	return -1;
}

/*
* Purpose: Verify that the lexeme can be convert to an int and that it is in the same range as the value of 2-byte integer in C.
* Author: Nicholas Heggart-Richer
* Versions: 1.0
* Called functions: atolh()
* Parameters:
*		lexeme:	The lexeme to verify if it can be convert to int and stay in the correct range
* Return:
*		True if the lexeme is valid
*/
int isValidHIL(char* lexeme) {
	long l = atolh(lexeme);
	return l >= 0 && l <= USHRT_MAX - 1;
}

/*
* Purpose: Verify that the lexeme can be convert to int and that it is in the same range as the value of 2-byte integer in C.
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: atol()
* Parameters:
*		lexeme:	The lexeme to verify if it can be convert to int and stay in the correct range
* Return:
*		True if the lexeme is valid
*/
int isValidIL(char* lexeme) {
	long l = atol(lexeme);
	return l >= 0 && l <= USHRT_MAX;
}

/*
* Purpose: Verify that the lexeme can be convert to float and that it is in the same range the value of 4 - byte float in C.
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: atof()
* Parameters:
*		lexeme:	The lexeme to verify if it can be convert to float and stay not lose data
* Return:
*		True if the lexeme is valid
*/
int isValidFPL(char* lexeme) {
	double d = strtod(lexeme, NULL);
	return (d >= FLT_MIN && d <= FLT_MAX) || d == 0.0f;
}
