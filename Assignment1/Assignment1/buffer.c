#include "buffer.h"

/*
 * File name: buffer.c
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

/* TODO list of function to fix 
 * b_compact
 * 
 * 
 */


/*
 * Purpose: To create the buffer and initialize its atributes
 * Author: Tiago Donchegay
 * Versions: 1.0
 * Called functions: calloc(), malloc(), free()
 * Parameters: 
 *		init_capacity: The initial capacity of the buffer
 *		inc_factor:    The factor by which the buffer will increment
 *		o_mode:		   Which type of buffer should be used
 * Return: The newly created buffer
 *
 */
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	Buffer* pBD;
	unsigned char uinc_factor;
	uinc_factor = (unsigned char)inc_factor;
	
	/* Verify if the initial capacity is valid*/
	if (init_capacity < 0 || init_capacity > BUFFER_MAX) {
		return NULL;
	}

	/* Verify if the initial capacity is not zero when yhe mode is fixed */
	if (o_mode == 'f' && init_capacity == 0) {
		return NULL;
	}

	/* Allocate the memory for the buffer struc */
	pBD = (Buffer*)calloc(1, sizeof(Buffer));
	if (pBD == NULL) {
		return NULL;
	}

	/* Allocated the memory for the array of char */
	pBD->cb_head = malloc(init_capacity * sizeof(char*));
	if (pBD->cb_head == NULL) {
		free(pBD);
		return NULL;
	}

	/* Set the correct mode and inc_factor depending the parameters */
	if (o_mode == 'f' || uinc_factor == 0) {
		pBD->mode = FIXED_SIZE;
		pBD->inc_factor = 0;
	}else if (o_mode == 'f' && uinc_factor != 0) {
		pBD->mode = FIXED_SIZE;
		pBD->inc_factor = 0;
	}else if (o_mode == 'a' && uinc_factor >= 1 && uinc_factor <= 255) {
		pBD->mode = ADDITIVE_INCREMENT;
		pBD->inc_factor = uinc_factor;
	}else if (o_mode == 'm' && uinc_factor >= 1 && uinc_factor <= 100) {
		pBD->mode = MULTIPLICATIVE_INCREMENT;
		pBD->inc_factor = uinc_factor;
	}else {
		/* Clean memory if invalid mode or increment factor */
		free(pBD->cb_head);
		free(pBD);
		return NULL;
	}

	pBD->capacity = init_capacity;
	return pBD;
}

/*
* Purpose: To add a new symbol to the end of the array of char in the buffer and increase its size as required.
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: b_isfull(), b_mode(), b_capacity(), b_incfactor(), realloc()
* Parameters:
*		pBD:	The pointer to the buffer to be updated
*		symbol: The charracter symbol to be added to array of char in the buffer
* Return: The pointer to the buffer
*
*/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	/* Reset the realloc flag*/
	pBD->r_flag = 0;

	/* Check if the array is full and need to be resize */
	if (b_isfull(pBD)) {

		int mode = b_mode(pBD);
		char* newpArray;
		short newCapacity = 0;

		if (mode == ADDITIVE_INCREMENT) {
			/* Calculate the new capacity */
			newCapacity = b_capacity(pBD) + (short)b_incfactor(pBD) * sizeof(char);
			if (newCapacity < 0) {
				return NULL;
			}

		}else if (mode == MULTIPLICATIVE_INCREMENT) {
			short availableSpace;
			short newIncrement;

			/* Check if the buffer can be incremented */
			if ( b_capacity(pBD) == BUFFER_MAX) {
				return NULL;
			}

			/* Calculate the new capacity */
			availableSpace = BUFFER_MAX - b_capacity(pBD);
			newIncrement = availableSpace * (short)b_incfactor(pBD) / 100;
			newCapacity = (newIncrement == 0) ? newCapacity = BUFFER_MAX : b_capacity(pBD) + newIncrement;

		}else if (mode == FIXED_SIZE){
			return NULL;
		}

		/* Verify if the buffer is not bigger then its max size */
		if (newCapacity > BUFFER_MAX) {
			newCapacity = BUFFER_MAX;
		}

		/* Resize the buffer to the new capacity */
		newpArray = realloc(pBD->cb_head, newCapacity);
		if (newpArray == NULL) {
			return NULL;
		}
		pBD->capacity = newCapacity;

		/* Set the realloc flag if pointer is different */
		if (newpArray != pBD->cb_head) {
			pBD->cb_head = newpArray;
			pBD->r_flag = SET_R_FLAG;
		}
	}

	/* Add the symbol to the end of the array */
	pBD->cb_head[pBD->addc_offset] = symbol;
	++pBD->addc_offset;
	return pBD;

}

/*
* Purpose: To reset all the offset and flag of the buffer
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer to be reseted
* Return: If the function succeeded 0 or failed RT_FAIL1
*
*/
int b_clear(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	/* Reset all the offsets and flag */
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->r_flag = 0;
	return 0;
}

/*
* Purpose: Free all the allocated memory of the buffer
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: free()
* Parameters:
*		pBD:	The pointer to the buffer to be freed
*
*/
void b_free(Buffer * const pBD)
{
	if (pBD != NULL) {
		if (pBD->cb_head != NULL)
			free(pBD->cb_head);
		free(pBD);
	}
}

/*
* Purpose: Check if the buffer is full
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer to check if full
* Return: If its full or if failed RT_FAIL1
*
*/
int b_isfull(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	return pBD->addc_offset == pBD->capacity;
}

/*
* Purpose: Get the current limit of the buffer
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer to check from
* Return: The limit of the buffer or if failed RT_FAIL1
*
*/
short b_limit(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	return pBD->addc_offset;
}

/*
* Purpose: Get the current capacity of the buffer
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer to check from
* Return: The capacity of the buffer or if failed RT_FAIL1
*
*/
short b_capacity(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	return pBD->capacity;
}

/*
* Purpose: Get the pointer to the saved mark of the buffer
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer to get from
* Return: The pointer to the mark of the buffer or if failed RT_FAIL1
*
*/
short b_mark(pBuffer const pBD, short mark)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	/* Check if the mark is outside the array of char */
	if (mark < 0 || mark > pBD->addc_offset) {
		return RT_FAIL1;
	}
	return pBD->markc_offset = mark;
}

/*
* Purpose: Get the mode of the buffer
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer to verify from
* Return: The mode of the buffer or if failed RT_FAIL1
*
*/
int b_mode(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	return pBD->mode;
}

/*
* Purpose: Get the increment factor of the buffer
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer to get from
* Return: The increment factor of the buffer or if failed 0x100
*
*/
size_t b_incfactor(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return 0x100;
	}
	return (unsigned char)pBD->inc_factor;
}

/*
* Purpose: Load the buffer with the data in the file
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: fgetc(), feof(), b_addc()
* Parameters:
*		fi:		The pointer to the file to read from
*		pBD:	The pointer to the buffer to add to
* Return: The number of symbol added to the buffer or if could add to to buffer LOAD_FAIL else  RT_FAIL1 
*
*/
int b_load(FILE * const fi, Buffer * const pBD)
{
	char c;
	int count;
	/* Check if the file pointer is NULL */
	if (fi == NULL) {
		return RT_FAIL1;
	}
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	count = 0;
	c = (char)fgetc(fi);
	/* Loop until the end of the file*/
	while (!feof(fi)) {
		/* Add to the buffer */
		if (!b_addc(pBD, c)) {
			return LOAD_FAIL;
		}
		c = (char)fgetc(fi);
		++count;
	}
	return count;
}

/*
* Purpose: Verify is the buffer is empty
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none 
* Parameters:
*		pBD:	The pointer to the buffer to verify from
* Return: If the buffer is empty or if failed RT_FAIL1
*
*/
int b_isempty(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	return pBD->addc_offset == 0;
}

/*
* Purpose: Verify if the end of buffer flag is set
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none 
* Parameters:
*		pBD:	The pointer to the buffer to verify from
* Return: If the end of buffer is set or if failed RT_FAIL1
*
*/
int b_eob(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	return pBD->eob;
}

/*
* Purpose: Verify if the end of buffer flag is set
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none 
* Parameters:
*		pBD:	The pointer to the buffer to verify from
* Return: The next symbol in array if reached the end set the end of buffer flag and return RT_FAIL1 or if failed RT_FAIL2
*
*/
char b_getc(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL2;
	}
	/* Check if it has reached the end of the buffer */
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->eob = 1;
		return RT_FAIL1;
	}

	/* increment the offet and return the symbol */
	pBD->eob = 0;
	++pBD->getc_offset;
	return pBD->cb_head[pBD->getc_offset-1];
}

/*
* Purpose: Display the buffer in the console
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: b_isempty(), printf(), b_getc()
* Parameters:
*		pBD:	The pointer to the buffer to display
* Return: The count of symbol displayed or if failed RT_FAIL1
*
*/
int b_print(Buffer * const pBD)
{
	char c;
	int count;

	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	/* Check if the buffer is empty */
	if (b_isempty(pBD)) {
		printf("Empty buffer.\n");
		return RT_FAIL1;
	}

	count = 0;
	c = b_getc(pBD);
	/* Loop until the end of the buffer */
	while (!pBD->eob) {
		/* Display the symbol */
		printf("%c", c);
		c = b_getc(pBD);
		++count;
	}
	printf("\n");

	return count;
}

/*
* Purpose: Resize the buffer to the minimun required sized
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: b_limit(), realloc()
* Parameters:
*		pBD:	The pointer to the buffer to resize
* Return: The pointer to the buffer or if failed NULL
*
*/
Buffer * b_compact(Buffer * const pBD, char symbol)
{
	short newCapacity;
	char* newpArray;

	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return NULL;
	}

	/* Calculate the new capacity and realloc */
	newCapacity = (b_limit(pBD)+1) * sizeof(char);
	newpArray = realloc(pBD->cb_head, newCapacity);
	/* If the realloc failed */
	if (newpArray == NULL) {
		return NULL;
	}

	/* Set the realloc flag if pointer is different */
	if (newpArray != pBD->cb_head) {
		pBD->cb_head = newpArray;
		pBD->r_flag = SET_R_FLAG;
	}

	/* Set the new capacity,  add the symbol, and increment the addc offset */
	pBD->capacity = newCapacity;
	pBD->cb_head[pBD->addc_offset] = symbol;
	++pBD->addc_offset;
	return pBD;
}

/*
* Purpose: Verify if the realloc flag is set
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer to check from
* Return: The realloc flag or if failed RT_FAIL1
*
*/
char b_rflag(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	return pBD->r_flag;
}

/*
* Purpose: Move the get offset by one back
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer change the getc offset
* Return: The new getc offset or if failed RT_FAIL1
*
*/
short b_retract(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL or cannot retract*/
	if (pBD == NULL || pBD->getc_offset <= 0) {
		return RT_FAIL1;
	}
	return --pBD->getc_offset;
}

/*
* Purpose: Change the getc offset back to the mark offset
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer change the getc offset
* Return: The new getc offset or if failed RT_FAIL1
*
*/
short b_reset(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	return pBD->getc_offset = pBD->markc_offset;
}

/*
* Purpose: Get the getc offset of the buffer
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer get the getc offset
* Return: The getc offset of the buffer or if failed RT_FAIL1
*
*/
short b_getcoffset(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	return pBD->getc_offset;
}

/*
* Purpose: Reset the getc and markc offset
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer to reset the getc and markc offset
* Return: 0 if failed RT_FAIL1
*
*/
int b_rewind(Buffer * const pBD)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return RT_FAIL1;
	}
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return 0;
}

/*
* Purpose: Get pointer of the char at loc offset
* Author: Tiago Donchegay
* Versions: 1.0
* Called functions: none
* Parameters:
*		pBD:	The pointer to the buffer 
* Return: The pointer to the char at loc offset if failed NULL
*
*/
char * b_location(Buffer * const pBD, short loc_offset)
{
	/* Check if the buffer pointer is NULL */
	if (pBD == NULL) {
		return NULL;
	}
	/* Check if its a valid loc_offset */
	if (loc_offset < 0 || loc_offset > pBD->addc_offset) {
		return NULL;
	}
	return pBD->cb_head + loc_offset;
}
