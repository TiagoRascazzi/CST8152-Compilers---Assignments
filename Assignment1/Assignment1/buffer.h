/*
* File name: buffer.h
* Compiler: MS Visual Studio 2015, gcc
* Author: Tiago Donchegay, 040867850
* Course: CST8152_010 Compilers
* Assignment: 1
* Date: 2/09/2018
* Professor: Svillen Ranev
* Purpose: This is a buffer that has three operating mode the "fixedsize", "additive self-incrementing", and "multiplicative self-incrementing"
* Function list: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(),
*	 b_capcity(), b_mark(), b_mode(), b_incfactor(), b_load(), b_isempty(),
*	 b_eob(), b_getc(), b_print(), b_compact(), b_rflag(), b_retract(), b_reset(),
*	 b_getcoffset(), b_rewind(), b_location()
*
*/


#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) */
/*to enforce C89 type comments  - to make //comments an warning */
/*#pragma warning(error:4001)*/
/* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL1 -1			/* fail return value */
#define RT_FAIL2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail error */
#define SET_R_FLAG 1       /* realloc flag set value */

#define NULL_BUFFER 0x100

#define ADD_MAX_INC_FACTOR 255
#define MUL_MAX_INC_FACTOR 100

#define FIXED_SIZE 0				/* fixed sized buffer */
#define ADDITIVE_INCREMENT 1        /* additive self increment buffer */
#define MULTIPLICATIVE_INCREMENT -1 /* multiplicative self increment buffer */
#define BUFFER_MAX SHRT_MAX-1		/* maximum size of buffer */

typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_limit(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_mark(pBuffer const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer * b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char * b_location(Buffer * const pBD, short loc_offset);

#endif
