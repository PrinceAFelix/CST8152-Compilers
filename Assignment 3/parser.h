/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
*************************************************************
* File name: parser.h
* Compiler: MS Visual Studio 2019
* Author: Mark Wright (040973261) and Prince Adrianne Felix (040933287)
* Course: CST 8152 – Compilers, Lab Section: 012
* Assignment: A3.
* Date: April 18, 2021
* Purpose: This file is the main header for Parser (.h)
* Function list: startParser(), matchToken(), syncErrorHandler(), printError(), program(), optionalStatements(), inputStatement(), statements()
				 statement(), statements_prime(), assignmentStatement(), assignmentExpression(), selectionStatement(), iterationStatement(), preCondition()
				 variableList(), variableListPprime(), variableIdentifier(), outputStatement(), output_statement_prime(), optVariableList(), outputList()
				 arithmeticExpression(), unary_arithmetic_expression(), additive_arithmetic_expression(), additive_arithmetic_expression_prime(), multiplicative_arithmetic_expression()
				 multiplicative_arithmetic_expression_prime(), primary_arithmetic_expression(), stringExpression(), stringExpression_prime(), primary_string_expression()
				 conditionalExpression(), logical_OR_expression(), logical_OR_expression_prime(), logical_AND_expression(), logical_AND_expression(), logical_AND_expression_prime(), 
				 logical_NOT_expression(), relationalExpression(), relational_a_expression(), relational_a_expression_prime(), relational_s_expression(), relational_s_expression_prime(), 
				 primary_a_relational_expression(), primary_s_relational_expression().
*************************************************************/

/* Inclusion section */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "token.h"
#include "buffer.h"

/* Global vars */
static Token lookahead;
int syntaxErrorNumber = 0;
extern bStructure* stringLiteralTable;
extern int line;
extern Token tokenizer();
extern char* keywordTable[];

/* Constants */
#define	NO_ATTR	(-1)
#define MAIN 	0
#define IF		1
#define THEN	2
#define	ELSE	3
#define WHILE	4
#define DO		5
#define READ	6
#define WRITE	7
#define TRUE	8
#define FALSE	9

/* Function definitions */
void startParser(void);
void matchToken(int, int);
void syncErrorHandler(int);
void printError();
//void printMessage(char*);

/* PLATYPUS Syntactic Specification */
void program(void);

void optionalStatements(void);
/* statement(s)*/
void statement(void);
void statements(void);
void statements_prime(void);

/* assignment */
void assignmentStatement(void);
void assignmentExpression(void);

/* statement types */
void selectionStatement(void);
void iterationStatement(void);
void inputStatement(void);
void preCondition(void);

/* varibale list/identifier */
void variableList(void);
void optVariableList(void);
void variableListPprime(void);
void variableIdentifier(void);

/* output */
void outputStatement(void);
void output_statement_prime(void);
void outputList(void);

/* arithmetic */
void arithmeticExpression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void primary_arithmetic_expression(void);

/* string */
void stringExpression(void);
void stringExpression_prime(void);
void primary_string_expression(void);

/* conditional */
void conditionalExpression(void);
void logical_OR_expression(void);
void logical_OR_expression_prime(void);
void logical_AND_expression(void);
void logical_AND_expression_prime(void);
void logical_NOT_expression(void);

/* relational */
void relationalExpression(void);
void relational_a_expression(void);
void relational_a_expression_prime(void);
void primary_a_relational_expression(void);
void relational_s_expression(void);
void relational_s_expression_prime(void);
void primary_s_relational_expression(void);

