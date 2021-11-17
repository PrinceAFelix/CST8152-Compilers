/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
*************************************************************
* File name: parser.c
* Compiler: MS Visual Studio 2019
* Author: Mark Wright (040973261) and Prince Adrianne Felix (040933287)
* Course: CST 8152 – Compilers, Lab Section: 012
* Assignment: A3.
* Date: April 18, 2021
* Purpose: This file contains the logic for the PLATYPUS Parser
* Function list: startParser(), matchToken(), syncErrorHandler(), printError(), program(), optionalStatements(), inputStatement(), statements()
				 statement(), statements_prime(), assignmentStatement(), assignmentExpression(), selectionStatement(), iterationStatement(), preCondition()
				 variableList(), variableListPprime(), variableIdentifier(), outputStatement(), output_statement_prime(), optVariableList(), outputList()
				 arithmeticExpression(), unary_arithmetic_expression(), additive_arithmetic_expression(), additive_arithmetic_expression_prime(), multiplicative_arithmetic_expression()
				 multiplicative_arithmetic_expression_prime(), primary_arithmetic_expression(), stringExpression(), stringExpression_prime(), primary_string_expression()
				 conditionalExpression(), logical_OR_expression(), logical_OR_expression_prime(), logical_AND_expression(), logical_AND_expression(), logical_AND_expression_prime(), 
				 logical_NOT_expression(), relationalExpression(), relational_a_expression(), relational_a_expression_prime(), relational_s_expression(), relational_s_expression_prime(), 
				 primary_a_relational_expression(), primary_s_relational_expression().
*************************************************************/

#include "parser.h"

/*************************************************************
 * Process Parser
 ************************************************************/
void startParser(void) {
	lookahead = tokenizer();
	program();
	matchToken(SEOF_T, NO_ATTR);
	printf("%s\n", "PLATY: Source file parsed");
}

/*************************************************************
 * Match Token 
 ************************************************************/
void matchToken(int tokenCode, int tokenAttribute) {
	int matchFlag = 1;
	switch (lookahead.code) {
	/* For KW_T, REL_OP_T,  ART_OP_T, LOG_OP_T, we must check that the lookahead attribute code type matches expected */
	case KW_T:
	case REL_OP_T:
	case ART_OP_T:
	case LOG_OP_T:
		if (lookahead.code == tokenCode && lookahead.attribute.codeType != tokenAttribute)
			matchFlag = 0;
	/* For other codes, we do only need to match the token code */
	default:
		if (lookahead.code != tokenCode)
			matchFlag = 0;
	}
	/* if we have a match, and that token is an SEOF_T, we return as we are at the end of file */
	if (matchFlag && (lookahead.code == SEOF_T))
		return;
	/* otherwise, if we match the token we advance lookahead by calling tokenizer */
	if (matchFlag) {
		lookahead = tokenizer();
		/* if the next token is an Error token, we call printError() and advance the tokenizer */
		if (lookahead.code == ERR_T) {
			printError();
			lookahead = tokenizer();
			syntaxErrorNumber++;

		}
	}
	/* if matchFlag == 0, we have no match, we call syncErrorHandler() */
	else {
		syncErrorHandler(tokenCode);
		return;
	}
}

/*************************************************************
 * Syncronize Error Handler
 ************************************************************/
void syncErrorHandler(int syncTokenCode) {
	printError();
	syntaxErrorNumber++;
	/* keep advancing lookahead token until we get the code we need (syncmTokenCode, which is matchTokens tokenCode param */
	while (lookahead.code != syncTokenCode) {
		if (lookahead.code == SEOF_T)
			exit(syntaxErrorNumber);

		lookahead = tokenizer();
	}
	if (lookahead.code != SEOF_T)
		lookahead = tokenizer();
	return;
}

/*************************************************************
 * Print Error
 ************************************************************/
void printError() {
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case ERR_T:
		printf("%s\n", t.attribute.errLexeme);
		break;
	case SEOF_T:
		printf("SEOF_T\t\t%d\t\n", t.attribute.seofType);
		break;
	case AVID_T:
	case SVID_T:
		printf("%s\n", t.attribute.vidLexeme);
		break;
	case FPL_T:
		printf("%5.1f\n", t.attribute.floatValue);
		break;
	case INL_T:
		printf("%d\n", t.attribute.codeType);
		break;
	case STR_T:
		bSetMarkOffset(stringLiteralTable, t.attribute.contentString);
		printf("%s\n", bGetContent(stringLiteralTable, 0));
		break;
	case SCC_OP_T:
		printf("NA\n");
		break;
	case ASS_OP_T:
		printf("NA\n");
		break;
	case ART_OP_T:
		printf("%d\n", t.attribute.codeType);
		break;
	case REL_OP_T:
		printf("%d\n", t.attribute.codeType);
		break;
	case LOG_OP_T:
		printf("%d\n", t.attribute.codeType);
		break;
	case LPR_T:
		printf("NA\n");
		break;
	case RPR_T:
		printf("NA\n");
		break;
	case LBR_T:
		printf("NA\n");
		break;
	case RBR_T:
		printf("NA\n");
		break;
	case KW_T:
		printf("%s\n", keywordTable[t.attribute.codeType]);
		break;
	case COM_T:
		printf("NA\n");
		break;
	case EOS_T:
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}
}


/*************************************************************
 * Program statement
 * BNF: <program> -> PLATYPUS { <opt_statements> }
 * FIRST(<program>)= {KW_T (MAIN)}.
 ************************************************************/
void program(void) {
	/* Match Terminal symbols */
	matchToken(KW_T, MAIN);
	matchToken(LBR_T, NO_ATTR);
	/* Call non-terminal symbol */
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	printf("%s\n", "PLATY: Program parsed");
}

/*************************************************************
 * Optional statement
 * <opt_statements> -> <statements> | e
 * FIRST= {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}
 ************************************************************/
void optionalStatements(void) {
	/* Use first set to check if we can advance to non-terminal symbol */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		if (lookahead.attribute.codeType == IF
			|| lookahead.attribute.codeType == WHILE
			|| lookahead.attribute.codeType == READ
			|| lookahead.attribute.codeType == WRITE) {
			statements();
			break;
		}
		break;
	}
	printf("%s\n", "PLATY: Optional statements parsed");
}

/*************************************************************
 * Input Statement
 * <input statement> -> READ (<variable list>);
 * FIRST = {KW_T(READ)}
 ************************************************************/
void inputStatement(void) {
	/* Match Terminal symbols */
	matchToken(KW_T, READ);
	matchToken(LPR_T, NO_ATTR);
	/* Call non-terminal symbol */
	variableList();
	matchToken(RPR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Input statement parsed");
}

/*************************************************************
 * Statements
 * <statements> -> <statement> <statementsPrime>
 * FIRST = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
 ************************************************************/
void statements(void) {
	/* Call non-terminal symbols */
	statement();
	statements_prime();
	printf("%s\n", "PLATY: Statements parsed");
}

/*************************************************************
 * Statement
 * BNF:<statement> ->  <assignment statement>
					   | <selection statement>
					   | <iteration statement>
					   | <input statement>
					   | <output statement>
 * FIRST = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
 ************************************************************/
void statement(void) {
	/* Use first set to check if we can advance to non-terminal symbol */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assignmentStatement();
		break;
	case KW_T:
		switch (lookahead.attribute.codeType) {
		case IF:
			selectionStatement();
			break;
		case WHILE:
			iterationStatement();
			break;
		case READ:
			inputStatement();
			break;
		case WRITE:
			outputStatement();
			break;
		}
		break;
	/* A right brace would be an error */
	case RBR_T:
		printError();
	}
	printf("%s\n", "PLATY: Statement parsed");
}

/*************************************************************
 * Statements Prime
 * BNF: <statementsPrime> -> <statement><statementsPrime> | e
 * FIRST = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e }
 ************************************************************/
void statements_prime(void) {
	/* Use first set to check if we can advance to non-terminal symbols */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statement();
		statements_prime();
		break;
	case KW_T:
		if (lookahead.attribute.codeType == IF
			|| lookahead.attribute.codeType == WHILE
			|| lookahead.attribute.codeType == READ
			|| lookahead.attribute.codeType == WRITE) {
			statement();
			statements_prime();
		}
		break;
	}
}

/*************************************************************
 * Assignment Statement
 * BNF: <assignment statement> -> <assignment expression>;
 * FIRST = {AVID_T, SVID_T}
 ************************************************************/
void assignmentStatement(void) {
	/* advance to non-terminal symbols */
	assignmentExpression();
	/* match end of statement token */
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Assignment statement parsed");
}

/*************************************************************
 * Assignment Expression
 * BNF: <assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>
 * FIRST = {AVID_T, SVID_T}
 ************************************************************/
void assignmentExpression(void) {
	/* Use first set to check if we can advance to match expected terminal symbols, and advance to non-terminal symbols */
	switch (lookahead.code) {
	case AVID_T:
		matchToken(AVID_T, NO_ATTR);
		matchToken(ASS_OP_T, NO_ATTR);
		arithmeticExpression();
		printf("%s\n", "PLATY: Assignment expression parsed");
		break;
	case SVID_T:
		matchToken(SVID_T, NO_ATTR);
		matchToken(ASS_OP_T, NO_ATTR);
		stringExpression();
		printf("%s\n", "PLATY: Assignment expression parsed");
		break;
	}
}

/*************************************************************
 * Selection Statement
 * BNF:<selection statement> ->  IF <pre-condition>  (<conditional expression>)
		THEN { <opt_statements> }
		ELSE { <opt_statements> } ;
 * FIRST
 ************************************************************/
void selectionStatement(void) {
	/* match terminal symbols and advance to non-terminal symbols in correct order */
	matchToken(KW_T, IF);
	preCondition();
	matchToken(LPR_T, NO_ATTR);
	conditionalExpression();
	matchToken(RPR_T, NO_ATTR);
	matchToken(KW_T, THEN);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	matchToken(KW_T, ELSE);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);

	printf("%s\n", "PLATY: Selection statement parsed");
}

/*************************************************************
 * Iteration Statement
 * BNF: <iteration statement> -> WHILE <pre-condition> (<conditional expression>) DO { <statements>};
 * FIRST = {KW_T(WHILE)}
 ************************************************************/
void iterationStatement(void) {
	/* match terminal symbols and advance to non-terminal symbols in correct order */
	matchToken(KW_T, WHILE);
	preCondition();
	matchToken(LPR_T, NO_ATTR);
	conditionalExpression();
	matchToken(RPR_T, NO_ATTR);
	matchToken(KW_T, DO);
	matchToken(LBR_T, NO_ATTR);
	statements();
	matchToken(RBR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);

	printf("%s\n", "PLATY: Iteration statement parsed");
}


/*************************************************************
 * Pre Condition
 * BNF: <pre-condition> -> TRUE | FALSE
 * FIRST = {KW_T(TRUE), KW_T(FALSE)}
 ************************************************************/
void preCondition(void) {
	/* Use first set to check if we can advance to match expected terminal symbols */
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.codeType) {
		case TRUE:
			matchToken(KW_T, TRUE);
			break;
		case FALSE:
			matchToken(KW_T, FALSE);
			break;
		}
		break;

	}
	printf("%s\n", "PLATY: Pre-condition parsed");
}

/*************************************************************
 * Variable List
 * BNF: <variable list> -> <variable identifier><variable listPrime>
 * FIRST = {AVID_T, SVID_T}
 ************************************************************/
void variableList(void) {
	/* advance to non-terminal symbols in correct order */
	variableIdentifier();
	variableListPprime();
	printf("%s\n", "PLATY: Variable list parsed");
}

/*************************************************************
 * Variable List Prime
 * BNF: <variable listPrime> -> ,<variable identifier><variable listPrime> | e
 * FIRST = {COM_T, e }
 ************************************************************/
void variableListPprime(void) {
	switch (lookahead.code) {
	case COM_T: /*If comma*/
		matchToken(COM_T, NO_ATTR); /*Match the token*/
		variableIdentifier();
		variableListPprime();
		break;
	}
	/* epsilon allows for no error */
}

/*************************************************************
 * Variable Identifier
 * BNF: <variable identifier> -> AVID_T | SVID_T
 * FIRST = {AVID_T, SVID_T}
 ************************************************************/
void variableIdentifier(void) {
	switch (lookahead.code) {
	case AVID_T:
		matchToken(AVID_T, NO_ATTR); /*Match the token*/
		break;
	case SVID_T:
		matchToken(SVID_T, NO_ATTR);  /*Match the token*/
		break;
	default:
		printError();
	}
	printf("%s\n", "PLATY: Variable identifier parsed");
}

/*************************************************************
 * Output Statement
 * BNF: <output statement> -> WRITE (<output statementPrime>);
 * FIRST = {KW_T(WRITE)}
 ************************************************************/
void outputStatement(void) {
	matchToken(KW_T, WRITE);
	matchToken(LPR_T, NO_ATTR);
	output_statement_prime();
	matchToken(RPR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);

	printf("%s\n", "PLATY: Output statement parsed");
}

/*************************************************************
 * Output Statement Prime
 * BNF: <output statementPrime> -> <opt_variable list> | STR_T
 * FIRST = {AVID_T,SVID_T, STR_T, e}
 ************************************************************/
void output_statement_prime(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		optVariableList();  /*call variable list on AVID_T and SVID_T*/
		break;
	case STR_T:
		matchToken(STR_T, NO_ATTR); /*Match the token on String*/
		break;
	}
	printf("%s\n", "PLATY: Output variable list parsed");
}

/*************************************************************
 * Optional Variable List
 * BNF: <opt_variable list> -> <variable list> | e
 * FIRST = {AVID_T,SVID_T, e}
 ************************************************************/
void optVariableList(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		variableList(); /*call variable list on AVID_T and SVID_T*/
		break;
	default:
		printf("%s\n", "PLATY: Output variable list parsed");
		break;
	}
}

/*************************************************************
 * Output List
 * BNF: <output_list> -> <opt_variable list> | STR_T
 * FIRST = {AVID_T,SVID_T, STR_T, e}
 ************************************************************/
void outputList(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		optVariableList(); /*call optional variable list on AVID_T and SVID_T*/
		break;
	case STR_T:
		matchToken(STR_T, NO_ATTR); /*Match token if String*/
		break;
	}
	printf("%s\n", "PLATY: Output list parsed");
}

/*************************************************************
 * Arithmetic Expression
 * BNF: <arithmetic expression> ->  <unary arithmetic expression> | <additive arithmetic expression>
 * FIRST = {ART_OP_T(SUB),ART_OP_T(ADD), AVID_T, FPL_T, INL_T, LPR_T}
 ************************************************************/
void arithmeticExpression() {
	switch (lookahead.code) {
	case ART_OP_T: /*If Arithmetic operator*/
		switch (lookahead.attribute.codeType) {
		case SUB:
		case ADD: /*If SUB and ADD*/
			unary_arithmetic_expression(); /*Call unary arithmetic expression function*/
			break;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression(); /*Call additive arithmetic expression function*/
		break;
	default:
		printError(); /*Print error on default*/
		break;
	}
	printf("%s\n", "PLATY: Arithmetic expression parsed");


}

/*************************************************************
 * Unary Arithmetic Expression
 * BNF: <unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression>
 * FIRST = {ART_OP_T(SUB),ART_OP_T(ADD)}
 ************************************************************/
void unary_arithmetic_expression(void) {
	switch (lookahead.code) {
	case ART_OP_T: /*If Arithmetic operator*/
		switch (lookahead.attribute.codeType) {/*Check the code type*/
		case SUB:/*If SUB*/
			matchToken(ART_OP_T, SUB); /*Match the token*/
			primary_arithmetic_expression(); /*Call primary arithmetic expression function*/
			break;
		case ADD: /*If ADD*/
			matchToken(ART_OP_T, ADD); /*Match the token*/
			primary_arithmetic_expression(); /*Call primary arithmetic expression function*/
			break;
		}
	}
}

/*************************************************************
 * Additive Arithmetic Expression
 * BNF: <additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expressionPrime>
 * FIRST = {AVID_T, FPL_T, INL_T, LPR_T}
 ************************************************************/
void additive_arithmetic_expression(void) {
	/*Calls the first set*/
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();
	printf("%s\n", "PLATY: Additive arithmetic expression parsed");

}

/*************************************************************
 * Additive Arithmetic Expression Prime
 * BNF: <additive arithmetic expressionPrime> ->
	+ <multiplicative arithmetic expression><additive arithmetic expressionPrime>
	| - <multiplicative arithmetic expression><additive arithmetic expressionPrime>
	| e
 * FIRST prime = { ART_OP_T(SUB),ART_OP_T(ADD), e }
 ************************************************************/
void additive_arithmetic_expression_prime(void) {
	switch (lookahead.code) {
	case ART_OP_T: /*If lookahead code is Arithmetic operation*/
		switch (lookahead.attribute.codeType) {
		case SUB:
		case ADD:/*if SUB and ADD*/
			matchToken(ART_OP_T, lookahead.attribute.codeType); /*Match the code by calling the matchTOken*/
			multiplicative_arithmetic_expression();  /*Call multiplicative arithmetic expression function*/
			additive_arithmetic_expression_prime(); /*Call additive arithmetic expression function*/
			break;
		}
	}


}

/*************************************************************
 * Multiplicative Arithmetic Expression
 * BNF: <multiplicative arithmetic expression> ->
	<primary arithmetic expression><multiplicative arithmetic expressionPrime>
 * FIRST = {AVID_T, FPL_T, INL_T, LPR_T}
 ************************************************************/
void multiplicative_arithmetic_expression(void) {
	/*Call the first set*/
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();
	printf("%s\n", "PLATY: Multiplicative arithmetic expression parsed");

}

/*************************************************************
 * Multiplicative Arithmetic Expression Prime
 * BNF:	<multiplicative arithmetic expressionPrime> ->
				* <primary arithmetic expression><multiplicative arithmetic expressionPrime>
				| / <primary arithmetic expression><multiplicative arithmetic expressionPrime>
				| e

 * FIRST = {ART_OP(MUL), ART_OP_T(DIV), e}
 ************************************************************/
void multiplicative_arithmetic_expression_prime(void) {
	switch (lookahead.code) { /*Check the lookahead code cases*/
	case ART_OP_T: /*If  Arithmetic operator*/
		switch (lookahead.attribute.codeType) {
		case MUL:
		case DIV: /*If MUL and DIV*/
			matchToken(ART_OP_T, lookahead.attribute.codeType); /*Match the code by calling the matchTOken*/
			primary_arithmetic_expression(); /*Call primary arithmetic expression function*/
			multiplicative_arithmetic_expression_prime(); /*Call multiplicative arithmetic expression function*/
			break;
		}
		break;

	}
}

/*************************************************************
 * Primary Arithmetic Expression
 * BNF: <primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
 * FIRST = {AVID_T, FPL_T, INL_T, LPR_T}
 ************************************************************/
void primary_arithmetic_expression(void) {
	switch (lookahead.code) { /*Check the lookahead code cases*/
	case AVID_T:
	case FPL_T:
	case INL_T: 
		matchToken(lookahead.code, NO_ATTR); /*Match the code if AVID_T, FPL_T, or INL_T*/
		break;
	case LPR_T:
		matchToken(LPR_T, NO_ATTR);/*Match the code if LPR_T*/
		arithmeticExpression(); /*Calls arithmetic expression function*/
		matchToken(RPR_T, NO_ATTR);/*Match the code if RPR_T*/
		break;
	}

	printf("%s\n", "PLATY: Primary arithmetic expression parsed");

}


/*************************************************************
 * String Expression
 * BNF: <string expression> -> <primary string expression><string expressionPrime>
 * FIRST = {SVID_T, STR_T}
 ************************************************************/
void stringExpression(void) {
	primary_string_expression();
	stringExpression_prime();
	printf("%s\n", "PLATY: String expression parsed");
}

/*************************************************************
 * String Expression Prime
 * BNF: <string expressionPrime> -> ++ <primary string expression><string expressionPrime | e
 * FIRST = { SCC_OP_T(++), e }
 ************************************************************/
void stringExpression_prime(void) {
	switch (lookahead.code) {
	case SCC_OP_T:
		matchToken(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		stringExpression_prime();
		break;
	}
}

/*************************************************************
 * Primary String Expression
 * BNF: <primary string expression> -> SVID_T  |  STR_T
 * FIRST = {SVID_T, STR_T}
 ************************************************************/
void primary_string_expression(void) {
	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		matchToken(lookahead.code, NO_ATTR);
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Primary string expression parsed");

}

/*************************************************************
 * Conditional Expression
 * BNF: <conditional expression> -> <logical OR  expression>
 * FIRST = { LOG_OP_T(.NOT.), AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void conditionalExpression(void) {
	/*Call the first set*/
	logical_OR_expression();
	printf("%s\n", "PLATY: Conditional expression parsed");
}

/*************************************************************
 * Logical OR Expression
 * BNF: <logical OR expression> -> <logical AND expression><logical OR expressionPrime>
 * FIRST = { LOG_OP_T(.NOT.), AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void logical_OR_expression(void) {
	/*Call the first set*/
	logical_AND_expression();
	logical_OR_expression_prime();
	printf("%s\n", "PLATY: Logical Or Expression parsed");

}

/*************************************************************
 * Logical OR Expression Prime
 * BNF: <logical OR expressionPrime> -> .OR. <logical AND expression><logical OR expressionPrime> | e
 * FIRST = { LOG_OP_T(.OR.), e }
 ************************************************************/
void logical_OR_expression_prime(void) {
	switch (lookahead.code) {
	case LOG_OP_T: /*If lookahead is LOG_OP_T and codetype is OR*/
		if (lookahead.attribute.codeType == OR) {
			matchToken(LOG_OP_T, OR); /*match the Token*/
			logical_AND_expression();
			logical_OR_expression_prime();
			printf("%s\n", "PLATY: Logical OR expression parsed");
		}
	default:
		return;

	}
}

/*************************************************************
 * Logical AND Expression
 * BNF: <logical AND expression> -> <logical NOT expression><logical AND expressionPrime>
 * FIRST = { LOG_OP_T(.NOT.), AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void logical_AND_expression(void) {
	/*Call the first set*/
	logical_NOT_expression();
	logical_AND_expression_prime();
	printf("%s\n", "PLATY: Logical And Expression parsed");

}

/*************************************************************
 * Logical AND Expression Prime
 * BNF: <logical AND expressionPrime> -> .AND. <logical NOT expression><logical AND expressionPrime> | e
 * FIRST =  { LOG_OP_T(.AND.), e  }
 ************************************************************/
void logical_AND_expression_prime(void) {
	switch (lookahead.code) {
	case LOG_OP_T: /*If lookahead is LOG_OP_T and codetype is AND*/
		if (lookahead.attribute.codeType == AND) {
			matchToken(LOG_OP_T, AND); /*match the Token*/
			logical_NOT_expression(); /*Call logical Not function*/
			logical_AND_expression_prime(); /*Call logical And function*/
			printf("%s\n", "PLATY: Logical AND expression parsed");
		}
	default:
		return;
	}
}

/*************************************************************
 * Logical NOT Expression
 * BNF: <logical NOT expression> -> .NOT. <relational expression>
										| <relational expression>
 * FIRST = { LOG_OP_T(.NOT.), AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void logical_NOT_expression(void) {
	switch (lookahead.code) { /*If lookahead is LOG_OP_T and attribute is NOT, match the token b calling the matchToken*/
	case LOG_OP_T:
		matchToken(LOG_OP_T, NOT);
		break;
	}
	relationalExpression(); //Call the first set
	printf("%s\n", "PLATY: Logical Not Expression parsed");

}


/*************************************************************
 * Relational Expression
 * BNF: <primary s relational expression><relational s expressionPrime>
 * FIRST = { AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 ************************************************************/
void relationalExpression(void) {
	switch (lookahead.code) { /*If lookahead code is AVID_T, FPL_T, and INL_T call the relational_a_expression function, else call the relational_a_expression */
	case AVID_T:
	case FPL_T:
	case INL_T:
		relational_a_expression();
		break;
	case SVID_T:
	case STR_T:
		relational_s_expression();
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Relational expression parsed");
}

/*************************************************************
 * Relational a Expression
 * BNF: <relational a expression> -> <primary a relational expression><relational a expressionPrime>
 * FIRST = { AVID_T, FPL_T, INL_T}
 ************************************************************/
void relational_a_expression(void) {
	/*Call the first set*/
	primary_a_relational_expression();
	relational_a_expression_prime();
	printf("%s\n", "PLATY: Relational arithmetic expression parsed");
}

/*************************************************************
 * Relational a Expression prime
 * BNF:<relational a expressionPrime> -> == <primary a relational expression> |
										 != <primary a relational expression> |
										 >  <primary a relational expression> |
										 <  <primary a relational expression
 * FIRST = {REL_OP_T(==, !=, >, <) }
 ************************************************************/
void relational_a_expression_prime(void) {
	/*Switch case that check the lookahead code then call the matchToken to match the code*/
	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.codeType) {/*Check for REL_OP_T code type, then call the matchTOken to match the code*/
		case EQ:
		case NE:
		case GT:
		case LT:
			matchToken(REL_OP_T, lookahead.attribute.codeType);
			primary_a_relational_expression();
			break;
		}
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Relational arithmetic operator parsed");
}



/*************************************************************
 * Relational s Expression
 * BNF: <primary s relational expression><relational s expressionPrime>
 * FIRST = {SVID_T, STR_T}
 ************************************************************/
void relational_s_expression(void) {
	/*Call the first set*/
	primary_s_relational_expression();
	relational_s_expression_prime();
	printf("%s\n", "PLATY: Relational string expression parsed");
}

/*************************************************************
 * Relational s Expression prime
 * BNF:<relational s expressionPrime> -> == <primary s relational expression> |
										 != <primary s relational expression> |
										 >  <primary s relational expression> |
										 <  <primary s relational expression>
 * FIRST = { REL_OP_T(==, !=, >, <) }
 ************************************************************/
void relational_s_expression_prime(void) {
	/*Switch case that check the lookahead code then call the matchToken to match the code*/
	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.codeType) { /*Check for REL_OP_T code type, then call the matchTOken to match the code*/
		case EQ:
		case NE:
		case GT:
		case LT:
			matchToken(REL_OP_T, lookahead.attribute.codeType);
			primary_s_relational_expression();
			break;
		}
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Relational string operator parsed");

}

/*************************************************************
 * Primary a Relational Expression
 * BNF: <primary a_relational expression> ->  AVID_T  | FPL_T  | INL_T
 * FIRST = {AVID_T, FPL_T, INL_T}
 ************************************************************/
void primary_a_relational_expression(void) {
	
	/*Switch case that check the lookahead code then call the matchToken to match the code*/
	switch (lookahead.code) {
	case AVID_T:
		matchToken(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		matchToken(FPL_T, NO_ATTR);
		break;
	case INL_T:
		matchToken(INL_T, NO_ATTR);
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Primary relational arithmetic expression parsed");
}

/*************************************************************
 * Primary s Relational Expression
 * BNF: <primary s_relational expression> -> <primary string expression>
 * FIRST = {SVID_T, STR_T}
 ************************************************************/
void primary_s_relational_expression(void) {
	primary_string_expression(); /*Call the first set*/
	printf("%s\n", "PLATY: Primary relational string expression parsed");
}