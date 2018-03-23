/*
* File name: scanner.c
* Compiler: MS Visual Studio 2015, gcc
* Author: Tiago Donchegay, 040867850, Nicholas Richer,
* Course: CST8152_010 Compilers
* Assignment: 2
* Date:
* Professor: Svillen Ranev
* Purpose:
* Function list:
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
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

					   /* No other global variable declarations/definitiond are allowed */

					   /* scanner.c static(local) function  prototypes */
static int char_class(char c);					/* character class function */
static int get_next_state(int, char, int *);	/* state machine function */
static int iskeyword(char * kw_lexeme);			/*keywords lookup functuion */
static long atolh(char * lexeme);				/* converts hexadecimal string to decimal value */
static int isValidIL(char* lexeme);
static int isValidFPL(char* lexeme);
static int isValidHIL(char* lexeme);
//static Token errorLexemeFormat(char* er_lexeme);
static int containOnlyZeros(char* lexeme);

/*Initializes scanner */
/*TODO initialize scanner*/
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
	short lexstart;		/*start offset of a lexeme in the input char buffer (array) */
	short lexend;		/*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS;	/* type of state - initially not accepting */

	/* DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED */

	short coffset = 0;


	while (1) { /* endless loop broken by token returns it will generate a warning */

		/* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */
		c = b_getc(sc_buf);

		switch (c) {
		case ' ':
		case '\t':
		case '\v':
		case '\f':
		case '\r':
			continue;
		case '\n': ++line; continue;
		case SEOF1:
		case SEOF2:
			t.code = SEOF_T; /* no attribute */ return t;
		case '(': t.code = LPR_T; /* no attribute */ return t;
		case ')': t.code = RPR_T; /* no attribute */ return t;
		case '{': t.code = LBR_T; /* no attribute */ return t;
		case '}': t.code = RBR_T; /* no attribute */ return t;
		case ',': t.code = COM_T; /* no attribute */ return t;
		case ';': t.code = EOS_T; /* no attribute */ return t;
		case '#': t.code = SCC_OP_T; /* no attribute */ return t;
		case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;
		case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t;
		case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t;
		case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t;

		case '>': t.code = REL_OP_T; t.attribute.rel_op = GT; return t;
		case '<':
		{
			b_mark(sc_buf, b_getcoffset(sc_buf));
			switch (b_getc(sc_buf)) {
			case '>': t.code = REL_OP_T; t.attribute.rel_op = NE; return t;
			default: b_reset(sc_buf); t.code = REL_OP_T; t.attribute.rel_op = LT; return t;
			}
		}
		case '=':
		{
			b_mark(sc_buf, b_getcoffset(sc_buf));
			switch (b_getc(sc_buf)) {
			case '=': t.code = REL_OP_T; t.attribute.rel_op = EQ; return t;
			default: b_reset(sc_buf); t.code = ASS_OP_T; /*no attribute */ return t;
			}
		}

		case '.': //Logical operator
		{
			b_mark(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);
			if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.' ) {
				t.code = LOG_OP_T; t.attribute.log_op = AND; return t;
			}else if(c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.' ) {
				t.code = LOG_OP_T; t.attribute.log_op = OR; return t;
			}else {
				char cerr[2];
				cerr[0] = '.';
				cerr[1] = '\0';
				b_retract(sc_buf);
				return aa_func12(cerr);
			}
		}

		case '"': //String literal
		{
			b_mark(sc_buf, b_getcoffset(sc_buf));

			while ((c = b_getc(sc_buf)) != '"') {
				if (c == SEOF1 || c == SEOF2) {
					return aa_func12("String lit");
				}
			}

			b_reset(sc_buf);
			coffset = b_limit(str_LTBL);
			while ( ( c = b_getc(sc_buf) ) != '"') b_addc(str_LTBL, c);
			b_addc(str_LTBL, '\0');

			t.code = STR_T;
			t.attribute.str_offset = coffset;
			return t;
		}


		case '!': /* Character might be comment */
		{
			b_mark(sc_buf, b_getcoffset(sc_buf)); /* mark buffer */

			/* confirmed comment */
			if (b_getc(sc_buf) == '!') {
				while ((c = b_getc(sc_buf)) != '\n' && c != SEOF1 && c != SEOF2);
				b_retract(sc_buf);//TODO decide if use ++line; instead
				continue;
			}else{
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
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf)-1);

		/* CODE YOUR FINATE STATE MACHINE HERE(FSM or DFA)
			IT IMPLEMENTS THE FOLLOWING ALGORITHM : */


		/*
		FSM0.Begin with state = 0 and the input character c
		FSM1.Get the next state from the transition table calling
		state = get_next_state(state, c, &accept);
		FSM2.Get the next character
		FSM3.If the state is not accepting(accept == NOAS), go to step FSM1
		If the step is accepting, token is found, leave the machine and
		call an accepting function as described below.*/

		state = get_next_state(state, c, &accept);
		while(accept == NOAS){
			c = b_getc(sc_buf);
			state = get_next_state(state, c, &accept);
		}




		//RETRACT  getc_offset IF THE FINAL STATE IS A RETRACTING FINAL STATE
		if (accept == ASWR || accept == ER){
			b_retract(sc_buf);
		}

		//SET lexend TO getc_offset USING AN APPROPRIATE BUFFER FUNCTION
		lexend = b_getcoffset(sc_buf);

		/*CREATE  A TEMPORRARY LEXEME BUFFER HERE;
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
		lex_buf = b_allocate( lexend - lexstart + 1, 0, 'f');  //TODO check if buffer is null
		b_reset(sc_buf);
		for (int i = lexstart; i < lexend; i++) { // TODO State machine doens't work on n=001;
			c = b_getc(sc_buf);
			b_addc(lex_buf, c);
		}
		b_addc(lex_buf, '\0');

		t = (*aa_table[state])(b_location(lex_buf, 0) );


		b_free(lex_buf);
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
* Purpose: Match the column of the transistion table to a given character
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		c:	The charactor to match to a column
* Return: The appropriate column number of the transistion table
*/
int char_class(char c)
{
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
Token aa_func02(char *lexeme){
	Token t = { 0 };

	int keywordIndex = iskeyword(lexeme);
	if (keywordIndex != -1) {
		t.code = KW_T;
		t.attribute.kwt_idx = keywordIndex;
	}
	else {
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
Token aa_func03(char *lexeme){
	Token t = { 0 };

	t.code = SVID_T;
	if (strlen(lexeme) > VID_LEN){
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN - 1);
		t.attribute.vid_lex[VID_LEN - 1] = '$';
	}
	else {
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN);
	}
	t.attribute.vid_lex[VID_LEN] = '\0';

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
		t.code = INL_T;
		t.attribute.int_value = atoi(lexeme);
	}else {
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
Token aa_func08(char *lexeme){
	Token t = { 0 };

	if (isValidFPL(lexeme)){
		t.code = FPL_T;
		t.attribute.flt_value = atof(lexeme);
	}else {
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
Token aa_func11(char *lexeme){
	Token t = { 0 };

	if (isValidHIL(lexeme)) {
		t.code = INL_T;
		t.attribute.int_value = atolh(lexeme);
	}else {
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
Token aa_func12(char *lexeme){
	Token t = { 0 };
	t.code = ERR_T;

	if (strlen(lexeme) > ERR_LEN) {
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
* Author: Nicholas Richer
* Versions: 1.0
* Called functions: b_retract(), aa_func12()
* Parameters:
*		lexeme:	The lexeme to convert to ERROR token
* Return:
*		The lexeme if the lexeme if the lexeme is too long replace the end with "..."
*/
Token aa_func13(char *lexeme) {
	//b_retract(sc_buf);
	return aa_func12(lexeme);
}


/**************************************************************
* CONVERSION FUNCTIONS
**************************************************************/

/*
* Purpose: CONVERTS AN ASCII STRING REPRESENTING AN HEXADECIMAL INTEGER CONSTANT TO INTEGER VALUE
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: strlen()
* Parameters:
*		lexeme:	The lexeme to convert to Integer value
* Return:
*		The value calculated based on lexeme If no valid conversion could be performed, it returns zero
*/
long atolh(char * lexeme) {
	return strtol(lexeme, NULL, 0);

	/* laking negative number
	if (lexeme[0] != '0')
		return 0;
	long l = 0;
	for (int i = 0; i < strlen(lexeme); i++) {
		char nextChar = lexeme[strlen(lexeme) - i -1];

		int digit = 0;
		switch (nextChar) {
		case '0': digit = 0; break;
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
		for (int j = 0; j < i - 1; j++)
			p *= 16;
		l += digit * p;
	}
	return 0;
	*/
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
int isValidHIL(char* lexeme){
	long l = atolh(lexeme); /*Convert hex string to long TODO FIX Redundent??*/

	/*Is long less than USHRT_MAX*/
	return ((l >= 0 && l <= USHRT_MAX - 1)); //&& !(l == 0 && !containOnlyZeros(lexeme));

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
	return ((l >= SHRT_MIN && l <= SHRT_MAX) && !(l == 0 && !containOnlyZeros(lexeme)));
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
	return (d >= FLT_MIN && d <= FLT_MAX || d == 0.0f);
	//return (((d >= FLT_MIN && d <= FLT_MAX) && d != HUGE_VALF && d != -HUGE_VALF) && !(d == 0.0 && !containOnlyZeros(lexeme)));
}
/*
* Purpose: Verify that the lexeme has only zeros of dot
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: strlen()
* Parameters:
*		lexeme:	The lexeme to verify if it only contains zeros and or dot
* Return:
*		True if the lexeme only contains zeros and or dot
*/
int containOnlyZeros(char* lexeme){
	int i;
	for (i = 0; i < strlen(lexeme); i++)
		if (lexeme[i] != '0' && lexeme[i] != '.') { // || (i == 1 && lexeme[i] != 'x')
			return 0;
		}
	return 1;
}
