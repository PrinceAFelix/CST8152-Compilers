/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Fall, 2020
* This Code is EXCLUSIVE for professors (must not be shared)
* Author: Svillen Ranev - Paulo Sousa - Abdulah.
*************************************************************
* File name: scanner.c
* Compiler: MS Visual Studio 2019
* Author: Mark Wright (040973261) and Prince Adrianne Felix (040933287)
* Course: CST 8152 – Compilers, Lab Section: 012
* Assignment: A3.
* Date: Mar 20 2021
* Professor: Paulo Sousa / Abdulah
* Purpose: This file is the main code for Scanner (A2)
* Function list: int startScanner(bPointer psc_buf), Token tokenizer(void), int nextState(int state, char c),
int nextClass(char c), Token funcAVID(char lexeme[]) , Token funcSVID(char lexeme[]), Token funcIL(char lexeme[]),
Token funcFPL(char lexeme[]), Token funcSL(char lexeme[]), Token funcErr(char lexeme[]), int isKeyword(char lexeme[])
*************************************************************/


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

 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern bPointer stringLiteralTable;		/* String literal table */
int line;								/* current line number of the source code */
extern int errorNumber;					/* defined in platy_st.c - run-time error number */

static char debugMode = 0;				/* optional for debugging */

/* Local(file) global objects - variables */
static bPointer lexemeBuffer;			/* pointer to temporary lexeme buffer */
static bPointer sourceBuffer;			/* pointer to input source buffer */
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int nextClass(char c);			/* character class function */
static int nextState(int, char);		/* state machine function */
static int isKeyword(char* kw_lexeme);	/* keywords lookup function */


 /************************************************************
 Function Name: startScanner
 Purpose: This function initializes the scanner using defensive programming.
 Author: Mark Wright and Prince Adrianne Felix
 History/Versions: Version 1.0 Mar 20, 2021
 Called functions: bIsEmpty() bRewind() bClean()
 Parameters: bPointer psc_buf
 Return value: EXIT_FAILURE if bPointer is empty, EXIT_SUCCESS if not
 Algorithm:
 1. Check if buffer is empty, return EXIT_FAILURE
 2. rewind the buffer, clean the stringLiteralTable, set Line to 1, set source buffer to current buffer
 3. breturn EXIT_SUCCESS
 **************************************************************/
int startScanner(bPointer psc_buf) {
	if (bIsEmpty(psc_buf))
		return EXIT_FAILURE; /*1*/
	/* in case the buffer has been read previously  */
	bRewind(psc_buf);
	bClean(stringLiteralTable);
	line = 1;
	sourceBuffer = psc_buf;
	return EXIT_SUCCESS; /*0*/
}


/************************************************************
Function Name: tokenizer
Purpose: Responsible to classify a char (or sequence of chars)
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Mar 20, 2021
Called functions: bGetCh() bRetract() bSetMarkOffset() bGetChOffset() bRestore() nextState() bCreate() bFree()
Parameters: void
Return value: CurrentToken
Algorithm:
1. Specific sequence is read from buffer
2. A regular expressin is recognize and the appropriate functions is called
**************************************************************/
Token tokenizer(void) {
	Token currentToken = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c;	/* input symbol */
	int state = 0;		/* initial state of the FSM */
	short lexStart;		/* start offset of a lexeme in the input char buffer (array) */
	short lexEnd;		/* end offset of a lexeme in the input char buffer (array)*/

	int lexLength;		/* token length */
	int i;				/* counter */
	unsigned char newc;	/* new char */

	while (1) { /* endless loop broken by token returns it will generate a warning */
		c = bGetCh(sourceBuffer);

		/* ------------------------------------------------------------------------
			Part 1: Implementation of token driven scanner.
			Every token is possessed by its own dedicated code
			-----------------------------------------------------------------------
		*/

		switch (c) {
			/* Comments and whitespace */
		case '%':
			newc = bGetCh(sourceBuffer);
			if (newc == '%') {
				/*continue reading until end of line*/
				while (newc != '\n') {
					newc = bGetCh(sourceBuffer);
					/*Test SEOF*/
					if (newc == CHARSEOF0 || newc == CHARSEOF255) {
						currentToken.code = SEOF_T;
						return currentToken;
					}
				}
				/* Increment line when there's a \n  */
				line++;
				break;
			}
			else {
				/*Raise an error (ERR_T) and retract the last char */
				bRetract(sourceBuffer);
				currentToken.code = ERR_T;
				currentToken.attribute.errLexeme[0] = c;
				currentToken.attribute.errLexeme[1] = CHARSEOF0;
			}
			return currentToken;

		case ' ':
			continue;
		case '\n':
			line++;
			break;
		case 9:
			continue;
		case 11:
			continue;
			/* SEOF */
		case (CHARSEOF0):
			currentToken.code = SEOF_T;
			return currentToken;
		case (CHARSEOF255):
			currentToken.code = SEOF_T;
			return currentToken;

			/*Seperators*/
		case '{':
			currentToken.code = LBR_T;
			return currentToken;
		case '}':
			currentToken.code = RBR_T;
			return currentToken;
		case ')':
			currentToken.code = RPR_T;
			return currentToken;
		case '(':
			currentToken.code = LPR_T;
			return currentToken;
		case ',':
			currentToken.code = COM_T;
			return currentToken;
		case ';':
			currentToken.code = EOS_T;
			return currentToken;

			/*Arithmethic operator / String Concatenation*/
		case '+':
			newc = bGetCh(sourceBuffer);
			if (newc == '+') {
				currentToken.code = SCC_OP_T;
				return currentToken;
			}
			else {
				bRetract(sourceBuffer);
				currentToken.code = ART_OP_T;
				currentToken.attribute.arithmeticOperator = ADD;
				return currentToken;
			}
		case '-':
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = SUB;
			return currentToken;
		case '*':
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = MUL;
			return currentToken;
		case '/':
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = DIV;
			return currentToken;


			/*Relational Operator / Assignment Operator*/
		case '=':
			newc = bGetCh(sourceBuffer);
			/* Class if */
			if (newc == '=') {
				currentToken.code = REL_OP_T;
				currentToken.attribute.relationalOperator = EQ;
				return currentToken;
			}
			else {
				bRetract(sourceBuffer);
				currentToken.code = ASS_OP_T;
				return currentToken;
			}
		case '!':
			newc = bGetCh(sourceBuffer);

			if (newc == '=') {
				currentToken.code = REL_OP_T;
				currentToken.attribute.relationalOperator = NE;
				return currentToken;
			}
			bRetract(sourceBuffer);
			currentToken.attribute.errLexeme[0] = c;
			currentToken.code = ERR_T;
			return currentToken;
		case '>':
			currentToken.code = REL_OP_T;
			currentToken.attribute.relationalOperator = GT;
			return currentToken;
		case '<':
			currentToken.code = REL_OP_T;
			currentToken.attribute.relationalOperator = LT;
			return currentToken;

			/*Logical Operator*/
		case '.':
			/* Test alls sequences of next chars to match with 3 possible situation */

			bSetMarkOffset(sourceBuffer, bGetChOffset(sourceBuffer));
			newc = bGetCh(sourceBuffer);
			/*AND OR NOT*/
			if (newc == 'A' && bGetCh(sourceBuffer) == 'N' && bGetCh(sourceBuffer) == 'D' && bGetCh(sourceBuffer) == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = AND;
				return currentToken;
			}
			else if (newc == 'O' && bGetCh(sourceBuffer) == 'R' && bGetCh(sourceBuffer) == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = OR;
				return currentToken;
			}
			else if (newc == 'N' && bGetCh(sourceBuffer) == 'O' && bGetCh(sourceBuffer) == 'T' && bGetCh(sourceBuffer) == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = NOT;
				return currentToken;
			}
			else {
				bRestore(sourceBuffer);
				currentToken.code = ERR_T;
				currentToken.attribute.errLexeme[0] = c;
				currentToken.attribute.errLexeme[1] = CHARSEOF0;

				return currentToken;

			}

			/* ------------------------------------------------------------------------
				Part 2: Implementation of Finite State Machine (DFA)
						   or Transition Table driven Scanner
						   Note: Part 2 must follow Part 1 to catch the illegal symbols
				-----------------------------------------------------------------------
			*/

		default: /*General case*/
			state = nextState(state, c);
			lexStart = bGetChOffset(sourceBuffer) - 1;
			bSetMarkOffset(sourceBuffer, lexStart);
			while (stateType[state] == NOAS) {
				c = bGetCh(sourceBuffer);
				state = nextState(state, c);
			}
			if (stateType[state] == ASWR)
				bRetract(sourceBuffer);
			lexEnd = bGetChOffset(sourceBuffer);
			lexLength = lexEnd - lexStart;
			lexemeBuffer = bCreate((short)lexLength + 2, 0, 'f');
			if (!lexemeBuffer) {
				fprintf(stderr, "Scanner error cannot create buffer\n");
				exit(1);
			}
			bRestore(sourceBuffer);
			for (i = 0; i < lexLength; i++)
				bAddCh(lexemeBuffer, bGetCh(sourceBuffer));
			bAddCh(lexemeBuffer, '\0');
			currentToken = (*finalStateTable[state])(bGetContent(lexemeBuffer, 0));
			bFree(lexemeBuffer);
			return currentToken;
		} /* switch */

	} /* while */

} /* tokenizer */


/* DO NOT MODIFY THE CODE / COMMENT OF THIS FUNCTION */
/*************************************************************
 * Get Next State
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	(*) assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:
	(*) Assertion failed: test, file filename, line linenum.
	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
 ************************************************************/

int nextState(int state, char c) {
	int col;
	int next;
	col = nextClass(c);
	next = transitionTable[state][col];
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
	return next;
}


/************************************************************
Function Name: nextClass
Purpose: Get the next token class
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Mar 20, 2021
Called functions: isAlpha() isDigit()
Parameters: char c
Return value: val
Algorithm:
1. set val to -1
2. check a char if Letter, Digit, Point, SVID terminator, Single quotes, etc..
3. Return a value assign to each of those case mention above
**************************************************************/
int nextClass(char c) {
	int val = -1;
	/*				[A-z](0),	[0-9](1),	.(2),	$(3),	'(4),	SEOF(5),	other(6) */
	switch (c) {
	case '.':
		val = 2;
		break;
	case '$':
		val = 3;
		break;
	case '\'':
		val = 4;
		break;
	case CHARSEOF0:
	case CHARSEOF255:
		val = 5;
		break;
	default:
		if (isalpha(c))
			val = 0;
		else if (isdigit(c))
			val = 1;
		else
			val = 6;
	}
	return val;
}

/************************************************************
Function Name: funcAVID
Purpose: To accept state function AVID
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Mar 20, 2021
Called functions: isKeyword() strncpy()
Parameters: char lexeme
Return value: curentToken
Algorithm:
1. Test if lexeme is a keyword
2. If not keyword, set currentToken code to KW_T
3. If keyword, set currentTOken code to AVID_T
**************************************************************/
Token funcAVID(char lexeme[]) {
	Token currentToken = { 0 };
	int kwindex = isKeyword(lexeme);
	/*Check if not keyword*/
	if (kwindex != -1) {
		/*Set the code to KW_T*/
		currentToken.code = KW_T;
		currentToken.attribute.keywordIndex = kwindex;
	}
	else {
		/*else set to AVID_T*/
		currentToken.code = AVID_T;
		strncpy(currentToken.attribute.vidLexeme, lexeme, VID_LEN);
		currentToken.attribute.vidLexeme[VID_LEN] = CHARSEOF0;
	}
	return currentToken;
}

/************************************************************
Function Name: funcSVID
Purpose: To accept state funtion SVID
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Mar 20, 2021
Called functions: strlen() strcnp()
Parameters: char lexeme
Return value: currentToken
Algorithm:
1. Check the limit, copy content if ok to vidLexeme
2. If not, set the corresponding attribute to vidLexeme
3. End with \0
**************************************************************/
Token funcSVID(char lexeme[]) {
	Token currentToken = { 0 };
	currentToken.code = SVID_T;
	int i;
	/*Check the limit - VID_LEN (trunc if necessary)*/
	if (strlen(lexeme) > VID_LEN) {
		/*if ok, copy content to vidlexeme*/
		for (i = 0; i < VID_LEN - 1; i++) {
			currentToken.attribute.vidLexeme[i] = lexeme[i];
		}
		currentToken.attribute.vidLexeme[VID_LEN - 1] = '$';
		/*end with \0*/
		currentToken.attribute.vidLexeme[strlen(lexeme)] = CHARSEOF0;

	}
	else {
		strncpy(currentToken.attribute.vidLexeme, lexeme, strlen(lexeme));
		/*end with \0*/
		currentToken.attribute.vidLexeme[strlen(lexeme)] = CHARSEOF0;
	}

	return currentToken;
}



/************************************************************
Function Name:funcIL
Purpose: To accept state function IL
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Mar 20, 2021
Called functions: atol()
Parameters: char lexeme
Return value: currentToken
Algorithm:
1. Test boundary condition, returned error if larger
2.	Transform leseme to long
**************************************************************/
Token funcIL(char lexeme[]) {
	Token currentToken = { 0 };
	/*1. Test boundary condition ( null string or length beyond): call ES*/
	/*2. If ok, transform lexeme to long (not int) to test SHRT_MAX*/
	long testLex = atol(lexeme);
	if (testLex > SHRT_MAX || testLex < 0 || !lexeme || !lexeme[0])
	{
		currentToken = finalStateTable[ES](lexeme);
	}
	/*3. if ok, set intValue to cast converion for int*/
	else
	{
		int intVal = testLex;
		currentToken.code = INL_T;
		currentToken.attribute.intValue = intVal;
	}
	return currentToken;
}

/************************************************************
Function Name: funcFPL
Purpose: To accept state function FPL
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Mar 20, 2021
Called functions: funcErr()
Parameters: char lexeme
Return value: currentToken
Algorithm:
1. Test the boundary
2. if ok, adjust the attribute.floatValue
**************************************************************/
Token funcFPL(char lexeme[]) {
	Token currentToken = { 0 };

	/*Convert to double not float*/
	double num = atof(lexeme);
	/*Test boundary conditions*/
	if (num > FLT_MAX || num < FLT_MIN && num != 0)
	{

		currentToken = finalStateTable[ES](lexeme);
		return currentToken;
	}
	else
	{
		currentToken.code = FPL_T;
		currentToken.attribute.floatValue = (float)num;
	}
	/*if ok, adjust the attribute.floatValue*/


	return currentToken;
}



/************************************************************
Function Name: funcSL
Purpose: To accept state function SL
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Mar 20, 2021
Called functions: strlen() bAddCh()
Parameters: char lexeme
Return value: currentToken
Algorithm:
1. For loop, check if lexeme is single quote, if not add to stringLiteralTable, Increment line if found a newline
2. Separate lexeme with \0
**************************************************************/
Token funcSL(char lexeme[]) {
	Token currentToken = { 0 };

	int i;
	unsigned int len = strlen(lexeme);

	currentToken.code = STR_T;
	currentToken.attribute.contentString = bGetAddChOffset(stringLiteralTable);

	/*For loops*/
	for (i = 0; i < len; i++) {
		if (lexeme[i] == '\'') /*Checks if one lexeme is a ', ignore if so*/
			continue;
		else
			bAddCh(stringLiteralTable, lexeme[i]); /*Else add to stringLiteralTable*/

		if (lexeme[i] == '\n') /*Increment line if found a newline*/
			line++;

	}

	/*Separate the lexeme*/
	bAddCh(stringLiteralTable, CHARSEOF0);

	currentToken.code = STR_T;

	return currentToken;
}


/************************************************************
Function Name: funcErr
Purpose: To accept state function error
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Mar 20, 2021
Called functions: strlen() strcpy()
Parameters: char lexeme
Return value: currentToken
Algorithm:
1. Set the current token to ERR_T
2. Check if char array is longer than err len, if so replace the last three with a period
3. Check for a new line, increment line if so
**************************************************************/
Token funcErr(char lexeme[]) {
	/*Token*/
	Token currentToken = { 0 };

	int i;

	currentToken.code = ERR_T;

	/*If longer, then replace the last three with a period*/
	if (strlen(lexeme) > ERR_LEN) {
		lexeme[ERR_LEN - 1] = '.';
		lexeme[ERR_LEN - 2] = '.';
		lexeme[ERR_LEN - 3] = '.';
		lexeme[ERR_LEN] = CHARSEOF0;

	}

	/*Check for new line*/
	for (i = 0; i < strlen(lexeme); i++) {
		if (lexeme[i] == '\n')
			line++;
	}
	/*if lexeme is not longer thant ERR_LEN*/
	strcpy(currentToken.attribute.errLexeme, lexeme);
	return currentToken;
}

/************************************************************
Function Name: isKeyword
Purpose: This function checks if one specific lexeme is a keyword
Author: Mark Wright and Prince Adrianne Felix
History/Versions: Version 1.0 Mar 20, 2021
Called functions: strcmp()
Parameters: char lexeme
Return value: -1 if no match, index i if match
Algorithm:
1. Loop to the keyword size
2. Check if the same
**************************************************************/
int isKeyword(char lexeme[]) {
	/*Varibale i for looping*/
	int i = -1;

	/*For loops*/
	for (i = 0; i < KWT_SIZE; i++) {
		/*check if the one specific lexeme is euqal to the keyword*/
		if (strcmp(lexeme, keywordTable[i]) == 0) {
			return i;
		}
	}
	return -1;
}
