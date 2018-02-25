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
 

REPLACE *ESN* WITH YOUR ERROR STATE NUMBER 
#define ES  *ESN* /* Error state */
#define IS -1    /* Inavalid state */

/* State transition table definition */

REPLACE *CN* WITH YOUR COLUMN NUMBER  

#define TABLE_COLUMNS *CN*
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/*        [a-zA-Z]   ---   0   ---   [1-9]   ---   .   ---   $   ---   other */
	/* 1 */  { 1,              6,         4,           ES,       ES,       ES   },
	/* 2 */  { 1,              1,         1,           ES / AS2, 3,        2    },
	/* 3 */  { IS,             IS,        IS,          IS,       IS,       IS   },
	/* 4 */  { IS,             IS,        IS,          IS,       IS,       IS   },
	/* 5 */  { ES / AS5,       4,         4,           7,        5,        5    },
	/* 6 */  { IS,             IS,        IS,          IS,       IS,       IS   },
	/* 7 */  { ,               ,          ,            7,        ES / AS5, 5    },
	/* 8 */  { ES / AS8,       7,         7,           ES / AS8, 8,        8    },
	/* 9 */  { IS,             IS,        IS,          IS,       IS,       IS   },
	/* 10 */ { ,               ,          ,            ,         ,              },
	/* 11 */ { ,               ,          ,            ,         ,              },
	/* 12 */ { IS,             IS,        IS,          IS,       IS,       IS   },
	/* 13 */ { IS,             IS,        IS,          IS,       IS,       IS   } };
 
/* Accepting state table definition */
/*REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS */
#define ASWR     *N1*  /* accepting state with retract */
#define ASNR     *N2*  /* accepting state with no retract */
#define NOAS     *N3*  /* not accepting state */

int as_table[ ] = {YOUR INITIALIZATION HERE - USE ASWR, ASNR, NOAS };

/* Accepting action function declarations */

FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. 

Token aa_funcXX(char *lexeme); 

Replace XX with the number of the accepting state: 02, 03 and so on.

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[ ] ={


HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
TO ACCEPTING FUNCTIONS. THE ARRAY HAS THE SAME SIZE AS as_table[ ].
YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
ACCEPTING FUNCTIONS (FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
THE REST OF THE ELEMENTS MUST BE SET TO NULL.

};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table []=
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
                     