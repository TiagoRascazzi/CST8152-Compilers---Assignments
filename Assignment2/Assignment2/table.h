/* TT */
/* Filename: table.h
* Transition Table and function declarations necessary for the scanner implementation
* as required for CST8152 - Assignment #2.
* Version: 1.18.1
* Date: 1 February 2018
* Provided by: Svillen Ranev
* The file is incomplete. You are to complete it.
***************************************************
* REPLACE THIS HEADER WITH YOUR HEADER
***************************************************
*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or one of 255,0xFF,EOF
*/

/*  Special case tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
*  white space
*  !!comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', # ,
*  .AND., .OR. , SEOF, 'illigal symbol',
*/


/*REPLACE *ESN* WITH YOUR ERROR STATE NUMBER*/
#define ES 12	/* Error state */
#define ER 13	/* Error state with retract */
#define IS -1	/* Invalid state */

/* State transition table definition */
#define NUMBER_OF_STATES 14
#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int  st_table[NUMBER_OF_STATES][TABLE_COLUMNS] = 
{
	//[a-wyzG-Z]	[A-F]	0	[1-9]	.	$	x	other
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 0
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 1
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 2
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 3
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 4
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 5
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 6
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 7
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 8
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 9
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 10
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 11
	{ 0,			0,		0,	0,		0,	0,	0,	0 }, //State 12
	{ 0,			0,		0,	0,		0,	0,	0,	0 }  //State 13
};

/* Accepting state table definition */
/*REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS */
#define ASWR  1   /*N1*  /* accepting state with retract */
#define ASNR  2  /*N2*  /* accepting state with no retract */
#define NOAS  0  /*N3*  /* not accepting state */

int as_table[NUMBER_OF_STATES] = 
{  
	NOAS, //State 0
	NOAS, //State 1
	ASWR, //State 2
	ASNR, //State 3
	NOAS, //State 4
	ASWR, //State 5
	NOAS, //State 6
	NOAS, //State 7
	ASWR, //State 8
	NOAS, //State 9
	NOAS, //State 10
	ASWR, //State 11
	ASNR, //State 12
	ASWR  //State 13
};

/* Accepting action function declarations */

/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE.THEY ALL RETURN Token AND TAKE
ONE ARGUMENT : A string REPRESENTING A TOKEN LEXEME.
*/

Token aa_func02(char *lexeme);	// AVID
Token aa_func03(char *lexeme);	// SVID
Token aa_func05(char *lexeme);	// DIL
Token aa_func08(char *lexeme);	// FPL
Token aa_func11(char *lexeme);	// HIL
Token aa_func12(char *lexeme);	// ER
Token aa_func13(char *lexeme);	// ES


/* defining a new type: pointer to function (of one char * argument) returning Token */
typedef Token(*PTR_AAF)(char *lexeme);

PTR_AAF aa_table[NUMBER_OF_STATES] = 
{
	NULL,		//State 0
	NULL,		//State 1
	aa_func02,	//State 2
	aa_func03,	//State 3
	NULL,		//State 4
	aa_func05,	//State 5
	NULL,		//State 6
	NULL,		//State 7
	aa_func08,	//State 8
	NULL,		//State 9
	NULL,		//State 10
	aa_func11,	//State 11
	aa_func12,	//State 12
	aa_func13	//State 13
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  10
char * kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif

