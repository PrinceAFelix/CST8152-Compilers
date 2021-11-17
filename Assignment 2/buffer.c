/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Fall, 2020
* This Code is EXCLUSIVE for professors (must not be shared)
* Author: Svillen Ranev - Paulo Sousa - Abdulah.
*************************************************************
* File name: buffer.c
* Compiler: MS Visual Studio 2019
* Author: Mark Wright (040973261) and Prince Adrianne Felix (040933287)
* Course: CST 8152 – Compilers, Lab Section: 012
* Assignment: A1.
* Date: Feb 06 2021
* Professor: Paulo Sousa / Abdulah
* Purpose: This file is the main code for Buffer (A1)
* Function list: bCreate, bAddCh, bClean, bFree, bIsFull, bGetAddChOffset, bGetSize, bGetMode,
					bGetMarkOffset, bSetMarkOffset, bFinish, bDisplay, bLoad, bIsEmpty, bGetCh,
					bRewind, bRetract, bRestore, bGetChOffset, bGetIncrement, bGetContent, bufferAddCPosition,
					bGetFlags
*************************************************************/

#include "buffer.h"

/************************************************************
Function Name: bCreate
Purpose: This function creates a new buffer on the heap
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: calloc, malloc
Parameters: short size (0 to SHORT_MAX-1), char increment (mode 'f' (0)
mode 'a' (1 to 255) mode 'm' (1 to 100)
Return value: Buffer *bPointer, NULL for failure
Algorithm:
1. Check parameters for proper range
2. Initialize buffer structure and content
3. Set all buffer structure members according to params, set flags to default
**************************************************************/
bPointer bCreate(short size, char increment, char mode) {
	bPointer b;
	char bMode;
	if (size<0 || size>MAX_SIZE)
		return NULL;
	if (!size) {
		size = DEFAULT_SIZE;
		increment = DEFAULT_INCREMENT;
	}
	if (!increment)
		mode = 'f';
	switch (mode) {
	case 'a':
		bMode = ADDMODE;
		break;
	case 'f':
		bMode = FIXMODE;
		increment = 0;
		break;
	case 'm':
		if ((unsigned char)increment > MAXINCREMENT)
			return NULL;
		bMode = MULMODE;
		break;
	default:
		return NULL;
	}
	b = (bPointer)calloc(1, sizeof(bStructure));
	if (!b)
		return NULL;
	b->content = (char*)malloc(size);
	if (!b->content) {
		free(b);
		return NULL;
	}
	b->mode = bMode;
	b->size = size;
	b->increment = increment;
	b->flags = DEFAULT_FLAGS;
	return b;
}


/************************************************************
Function Name: bAddCh
Purpose: This function includes a character in the buffer.
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: realloc
Parameters: bPointer const pBuffer, char ch
Return value: Buffer *bPointer, NULL for failure
Algorithm:
1. Check if the buffer is not full
2. If full, test for which mode buffer operates in
3. The mode determines how much to increment buffer size
4. Reallocate character buffer size to new size
5. If enough space, add param ch to character buffer
**************************************************************/
bPointer bAddCh(bPointer const pBuffer, char ch) {
	char* tempbuf;
	short availSpace, newSize, newInc;
	if (!pBuffer)
		return NULL;
	pBuffer->flags = pBuffer->flags & RESET_R_FLAG;
	if (pBuffer->addCOffset * sizeof(char) == MAX_SIZE ||
		pBuffer->addCOffset * sizeof(char) == SHRT_MAX)
		return NULL;
	if (pBuffer->size == pBuffer->addCOffset * (int)sizeof(char)) {
		switch (pBuffer->mode) {
		case ADDMODE:
			newSize = pBuffer->size + (unsigned char)pBuffer->increment;
			if (newSize > 0)
				if (newSize == SHRT_MAX)
					newSize = MAX_SIZE;
			break;
		case FIXMODE:
			return NULL;
		case MULMODE:
			availSpace = MAX_SIZE - pBuffer->size;
			newInc = (short int)(availSpace * (pBuffer->increment / 100.0F));
			newSize = pBuffer->size + newInc;
			if (newSize <= pBuffer->size && pBuffer->size < MAX_SIZE)
				newSize = MAX_SIZE;
			break;
		default:
			return NULL;
		}
		tempbuf = (char*)realloc(pBuffer->content, newSize);
		if (!tempbuf)
			return NULL;
		if (tempbuf != pBuffer->content) {
			pBuffer->flags = pBuffer->flags | SET_R_FLAG;
			pBuffer->content = tempbuf;
		}
		pBuffer->size = newSize;
	}
	pBuffer->content[pBuffer->addCOffset++] = ch;
	return pBuffer;
}

/************************************************************
Function Name: bClean
Purpose: This function retains current buffer memory, and re-initializes all
buffer structure members
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: Buffer* bPointer
Return value: 0 for success, RT_FAIL_1 for failure
Algorithm:
**************************************************************/
int bClean(bPointer const pBuffer) {
	if (!pBuffer)
		return RT_FAIL_1;
	pBuffer->addCOffset = pBuffer->markOffset = pBuffer->getCOffset = 0;
	pBuffer->flags = pBuffer->flags & RESET_EOB;
	pBuffer->flags = pBuffer->flags & RESET_R_FLAG;
	return 0;
}

/************************************************************
Function Name: bFree
Purpose: This function frees the memory used be the character buffer and
the Buffer* bPointer
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: free
Parameters: Buffer* bPointer
Return value: int, or NULL for failure
Algorithm:
**************************************************************/
int bFree(bPointer const pBuffer) {
	/*Check if buffer head is not null, if not, fee the memory*/
	if (pBuffer) {
		free(pBuffer->content);
		free(pBuffer);
		return 1;
	}
	return 0;
}

/************************************************************
Function Name: bIsFull
Purpose: This function determines if the character buffer is full
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: Buffer* bPointer
Return value: 1 if buffer is full, 0 if not, RT_FAIL_1 for failure
Algorithm:
**************************************************************/
int bIsFull(bPointer const pBuffer) {

	if (!pBuffer)
		return RT_FAIL_1;
	return (pBuffer->size == pBuffer->addCOffset * (int)sizeof(char));
}

/************************************************************
Function Name: bGetAddChOffset
Purpose: This functions returns current addCOffset
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: Buffer* bPointer
Return value: short addCOffset, or RT_FAIL_1 for failure
Algorithm:
1.Check if buffer is working, return RT_FAIL_1 if not
**************************************************************/
short bGetAddChOffset(bPointer const pBuffer) {

	/*Check if buffer is null, if so, return NULL */
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->addCOffset;
}

/************************************************************
Function Name: bGetSize
Purpose: This function returns the current size of the character buffer
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: Buffer* bPointer
Return value: short size, or RT_FAIL_1 for failure
Algorithm:
1.Check if buffer is working, return RT_FAIL_1 if not
**************************************************************/
short bGetSize(bPointer const pBuffer) {

	/*Check if buffer is null, if so, return NULL */
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->size;
}

/************************************************************
Function Name: bGetMode
Purpose: This function returns the value of mode
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 202
Called functions: none
Parameters: Buffer* bPointer
Return value: int mode, or NULL for failure
Algorithm:
**************************************************************/
int bGetMode(bPointer const pBuffer) {

	/*Check if buffer is null, if so, return NULL */
	if (!pBuffer)
		return RT_FAIL_2;
	return pBuffer->mode;

}

/************************************************************
Function Name: bGetMarkOffset
Purpose: This function returns the value of markOffset
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 202
Called functions: none
Parameters: Buffer* bPointer
Return value: short markOffset, or RT_FAIL_1 for failure
Algorithm:
**************************************************************/
short bGetMarkOffset(bPointer const pBuffer) {

	/*Check if buffer is null, if so, return RT_FAIL_1 */
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->markOffset;
}

/************************************************************
Function Name: bSetMarkOffset
Purpose: This function sets parameter mark to markOffset
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 202
Called functions: none
Parameters: Buffer* bPointer, short mark (0 to addCOffset)
Return value: short markOffset, or RT_FAIL_1 for failure
Algorithm:
**************************************************************/
short bSetMarkOffset(bPointer const pBuffer, short mark) {

	/*Check if buffer and file is null, if so, return RT_FAIL_1 */
	if (!pBuffer || mark<0 || mark > pBuffer->addCOffset)
		return RT_FAIL_1;
	pBuffer->markOffset = mark;
	return 0;
}

/************************************************************
Function Name: bFinish
Purpose: This function shrinks or expandsd the character buffer to a
new size
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 202
Called functions: realloc
Parameters: Buffer* bPointer const pBuffer, char ch
Return value: Buffer* bPointer
Algorithm:
1. Set the new size to addCOffset plus 1
2. Reallocate to the new size
3. Put the ch parameter into the new character buffer
**************************************************************/
bPointer bFinish(bPointer const pBuffer, char ch) {

	char* tempbuf;
	short size;
	if (!pBuffer || pBuffer->addCOffset * sizeof(char) == SHRT_MAX)
		return NULL;
	pBuffer->flags = pBuffer->flags & RESET_R_FLAG;
	if (pBuffer->size - pBuffer->addCOffset * sizeof(char) == 1) {
		pBuffer->content[pBuffer->addCOffset++] = ch;
		return pBuffer;
	}
	size = (pBuffer->addCOffset + 1) * sizeof(char);
	tempbuf = (char*)realloc(pBuffer->content, size);
	if (!tempbuf)
		return NULL;
	if (pBuffer->content != tempbuf) {
		pBuffer->flags = pBuffer->flags | SET_R_FLAG;
		pBuffer->content = tempbuf;
	}
	pBuffer->size = size;
	pBuffer->content[pBuffer->addCOffset++] = ch;
	return pBuffer;
}

/************************************************************
Function Name: bDisplay
Purpose: This function will print the content of the the character buffer
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 202
Called functions: bGetCh, printf
Parameters: Buffer* bPointer const pBuffer, char nl
Return value: int, or RT_FAIL_1 for failure
Algorithm:
1. Get the character buffer
2. While not at the end of buffer, print out each character
3. At the end print a newline character
4. Return a count of the characters
**************************************************************/
int bDisplay(bPointer const pBuffer, char nl) {
	int cont = 0;
	char c;
	if (!pBuffer || !(pBuffer->content))
		return RT_FAIL_1;
	c = bGetCh(pBuffer);
	while (!(pBuffer->flags & CHECK_EOB)) {
		cont++;
		printf("%c", c);
		c = bGetCh(pBuffer);
	}
	if (nl)
		printf("\n");
	return cont;
}

/************************************************************
Function name: bLoad
Purpose: Load and open input file into buffer
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: fgetc(),bAddCh(), ungetc()
Parameters: bPointer const pBuffer, FILE* const fi
Return value: RT_FAIL_1, LOAD_FAIL, countChars
Algorithm:
	Check if pointer buffer and file is working, if not return RT_FAIL_1
	Opens FIle
**************************************************************/
int bLoad(bPointer const pBuffer, FILE* const fi) {
	int size = 0;
	char c;
	if (!fi || !pBuffer)
		return RT_FAIL_1;
	c = (char)fgetc(fi);
	while (!feof(fi)) {
		if (!bAddCh(pBuffer, c)) {
			ungetc(c, fi);
			return LOAD_FAIL;
		}
		c = (char)fgetc(fi);
		size++;
	}
	if (ferror(fi))
		return RT_FAIL_1;
	return size;
}

/************************************************************
Function name: bIsEmpty
Purpose: Check if addCOffset is empty
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions:none
Parameters: bPointer const pBuffer
Return value: RT_FAIL_1, 1, 0
Algorithm:
	Check if pointer buffer is working, if not return RT_FAIL_1
	Return 1 if the addCOffset is 0 else return 0
**************************************************************/
int bIsEmpty(bPointer const pBuffer) {

	/*Check the pointers of buffer if null*/
	if (!pBuffer)
		return RT_FAIL_1;
	return (pBuffer->addCOffset == 0);
}


/************************************************************
Function name: bGetCh
Purpose: This function reads the buffer
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: bPointer const pBuffer
Return value: pBuffer->content[pBuffer->getCOffset++], RT_FAIL_1, 0
Algorithm:
	Check if pointer buffer is working, if not return RT_FAIL_1
	Check if the eob has been achieved if so set it else reset (0)
	Retrurns the content
**************************************************************/
char bGetCh(bPointer const pBuffer) {

	if (!pBuffer)
		return RT_FAIL_1;
	if (pBuffer->getCOffset == pBuffer->addCOffset) {
		pBuffer->flags = pBuffer->flags | SET_EOB;
		return '\0';
	}
	pBuffer->flags = pBuffer->flags & RESET_EOB;
	return pBuffer->content[pBuffer->getCOffset++];
}


/************************************************************
Function name: bRewind
Purpose:  Set the getCOffset and markOffset to 0, so that the buffer can be re-read again
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: bPointer const pBuffer
Return value: RT_FAIL_1, 0
Algorithm:
	Check if pointer buffer is working, if not return RT_FAIL_1
	Set getCOffset and markOffset to 0
**************************************************************/
int bRewind(bPointer const pBuffer) {

	/*Check the pointers of buffer if null*/
	if (!pBuffer)
		return RT_FAIL_1;
	pBuffer->getCOffset = 0;
	pBuffer->markOffset = 0;
	return 0;
}


/************************************************************
Function name: bRetract
Purpose: Decrement getCOffset
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: bPointer const pBuffer
Return value: pBuffer->getCOffset, NULL
Algorithm:
	Check if pointer buffer is working, if not return NULL
	decrement getCOffset by 1
**************************************************************/
bPointer bRetract(bPointer const pBuffer) {

	if (!pBuffer || pBuffer->getCOffset == 0)
		return NULL;
	pBuffer->getCOffset--;
	return pBuffer;
}

/************************************************************
Function name: bRestore
Purpose: Sets the getCOffset equal to markOffset
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: bPointer const pBuffer
Return value: pBuffer->getCOffset, -1
Algorithm:
	Check if pointer buffer is working, if not return -1
	Set the getCOffset equal to markOffset
**************************************************************/
short bRestore(bPointer const pBuffer) {

	/*Check the pointers of buffer if null*/
	if (!pBuffer)
		return RT_FAIL_1;
	pBuffer->getCOffset = pBuffer->markOffset;
	return pBuffer->getCOffset;

}

/************************************************************
Function name: bGetChOffset
Purpose: Returns the getCOffset of the buffer field
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: bPointer const pBuffer
Return value: pBuffer->getCOffset, RT_FAIL_1
Algorithm:
	Check if pointer buffer is working, if not return RT_FAIL_1
**************************************************************/
short bGetChOffset(bPointer const pBuffer) {

	/*Check the pointers of buffer if null*/
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->getCOffset;
}


/************************************************************
Function name: bGetIncrement
Purpose: Returns the non-negative value of increment
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: bPointer const pBuffer
Return value: (unsigned char)(pBuffer->increment), RT_INC_FAIL
Algorithm:
	Check if pointer buffer is working, if not return RT_INC_FAIL
**************************************************************/
size_t bGetIncrement(bPointer const pBuffer) {

	/*Check the pointers of buffer if null*/
	if (!pBuffer)
		return RT_INC_FAIL;
	return (unsigned char)pBuffer->increment;
}


/************************************************************
Function name: bGetContent
Purpose: Return a pointer to the location of the character buffer
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: bPointer const pBuffer, short chPosition
Return value: pBuffer->content + chPosition, NULL
Algorithm:
	Check if pointer buffer is working and if the chPos met the range value, if not return NULL
**************************************************************/
char* bGetContent(bPointer const pBuffer, short chPosition) {

	/*Check if buffer is null, if chPosition is  less than 0 and if greater than buffer addCOffset */
	if (!pBuffer || chPosition < 0 || chPosition > pBuffer->addCOffset)
		return NULL;
	return pBuffer->content + chPosition;
}


/************************************************************
Function name: bufferAddCPosition
Purpose: Returns getCOffset to the calling functions
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: bStructure* const pBuffer
Return value: pBuffer->getCOffset or RT_FAIL_1
Algorithm:
	Checks if buffer is null
	return the value of buffer getCOffset
**************************************************************/
short bufferAddCPosition(bStructure* const pBuffer) {

	/*Check the pointers of buffer if null*/
	if (!pBuffer)
		return RT_FAIL_1;
	return pBuffer->addCOffset;
}


/************************************************************
Function name: bGetFalags
Purpose: Returns the flag field from buffer
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Feb 06 2021
Called functions: none
Parameters: bPointer const pBuffer
Return value: pBuffer->flags or RT_FAIL_1
Algorithm:
	Checks if buffer is null
	return the value of buffer flags
**************************************************************/
#define FLAGS_
#undef FLAGS_
#ifndef FLAGS_
unsigned short bGetFlags(bPointer const pBuffer) {

	if (!pBuffer)
		return (unsigned short)RT_FAIL_1;
	return pBuffer->flags;

}
#else
#define bGetFlags(pBuffer) ((pBuffer)?(pBuffer->flags):(RT_FAIL_1))
#endif
