%{
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
// #define YYPRINT(file, type, value)   yyprint (file, type, value)
// static void
// yyprint (file, type, value)
//      FILE *file;
//      int type;
//      YYSTYPE value;
// {
//   if (type == VAR)
//     fprintf (file, " %s", value.tptr->name);
//   else if (type == NUM)
//     fprintf (file, " %d", value.val);
// }
#define YYDEBUG 1
%}

%start PROGRAM

%union { int a;
        }

%token STORAGE_CLASS_SPECIFIER TYPE_SPECIFIER TYPE_QUALIFIER STRUCT_OR_UNION ENUM CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN SIZEOF DEFINE UNDEF INCLUDE LINE ERROR PRAGMA IFDEF IFNDEF ELIF IDENTIFIER STRING CHARACTER_CONSTANT INTEGER_CONSTANT FLOATING_CONSTANT FILENAME TOKEN_SEQUENCE

%token MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN  ELLIPSIS RIGHT_OP LEFT_OP INC_OP DEC_OP PTR_OP AND_OP OR_OP LE_OP GE_OP EQ_OP NE_OP


%%

PROGRAM: CONTROL TRANSLATION__UNIT { printf("PASS\n"); };

TRANSLATION__UNIT: EXTERNAL__DECLARATION
                  |
                  TRANSLATION__UNIT EXTERNAL__DECLARATION
                  ;

EXTERNAL__DECLARATION: FUNCTION__DEFINITION
                      |
                      DECLARATION
                      ;

FUNCTION__DEFINITION:  DECLARATION__SPECIFIERS DECLARATOR DECLARATION__LIST COMPOUND__STATEMENT
                       |
                       DECLARATOR DECLARATION__LIST COMPOUND__STATEMENT
                       |
                       DECLARATION__SPECIFIERS DECLARATOR COMPOUND__STATEMENT
                       |
                      DECLARATOR COMPOUND__STATEMENT
                      ;

DECLARATION:  DECLARATION__SPECIFIERS INIT__DECLARATOR__LIST
              |
              DECLARATION__SPECIFIERS 
              ;

DECLARATION__LIST: DECLARATION
                  |
                  DECLARATION__LIST DECLARATION
                  ;

DECLARATION__SPECIFIERS: STORAGE__CLASS__SPECIFIER DECLARATION__SPECIFIERS 
                        |
                        STORAGE__CLASS__SPECIFIER
                        |
                        TYPE__SPECIFIER DECLARATION__SPECIFIERS
                        |
                        TYPE__SPECIFIER
                        |                          
                        TYPE__QUALIFIER DECLARATION__SPECIFIERS
                        |
                        TYPE__QUALIFIER
                        ;

STORAGE__CLASS__SPECIFIER: STORAGE_CLASS_SPECIFIER 
                         ;

TYPE__SPECIFIER: TYPE_SPECIFIER
                |
                STRUCT__OR__UNION__SPECIFIER
                |
                ENUM__SPECIFIER
                ;
/*                |
                TYPEDEF__NAME*/

TYPE__QUALIFIER: TYPE_QUALIFIER
                ;

STRUCT__OR__UNION__SPECIFIER: STRUCT__OR__UNION IDENTIFIER '{' STRUCT__DECLARATION__LIST '}' ';'
                           |
                           STRUCT__OR__UNION '{' STRUCT__DECLARATION__LIST '}' ';'
                           |
                           STRUCT__OR__UNION IDENTIFIER
                           ;

STRUCT__OR__UNION: STRUCT_OR_UNION 
                 ;

STRUCT__DECLARATION__LIST: STRUCT__DECLARATION
                         |
                         STRUCT__DECLARATION__LIST STRUCT__DECLARATION
                         ;

INIT__DECLARATOR__LIST: INIT__DECLARATOR
                      |
                      INIT__DECLARATOR__LIST ',' INIT__DECLARATOR
                      ;

INIT__DECLARATOR: DECLARATOR
                 |
                 DECLARATOR '=' INITIALIZER
                 ;

STRUCT__DECLARATION: SPECIFIER__QUALIFIER__LIST STRUCT__DECLARATOR__LIST ';'
                    ;

SPECIFIER__QUALIFIER__LIST: TYPE__SPECIFIER SPECIFIER__QUALIFIER__LIST
                          |
                          TYPE__SPECIFIER
                          |
                          TYPE__QUALIFIER SPECIFIER__QUALIFIER__LIST
                          |
                          TYPE__QUALIFIER
                          ;

STRUCT__DECLARATOR__LIST: STRUCT__DECLARATOR
                        |
                        STRUCT__DECLARATOR__LIST ',' STRUCT__DECLARATOR
                        ;

STRUCT__DECLARATOR:  DECLARATOR
                    |
                    DECLARATOR ':' CONSTANT__EXPRESSION
                    |
                    ':' CONSTANT__EXPRESSION
                    ;

ENUM__SPECIFIER: ENUM IDENTIFIER '{' ENUMERATOR__LIST '}' 
                |
                ENUM '{' ENUMERATOR__LIST '}'
                |
                ENUM IDENTIFIER
                ;

ENUMERATOR__LIST: ENUMERATOR
                 |
                 ENUMERATOR__LIST ',' ENUMERATOR
                 ;

ENUMERATOR: IDENTIFIER
            |
            IDENTIFIER '=' CONSTANT__EXPRESSION
            ;

DECLARATOR: POINTER DIRECT__DECLARATOR
            |
            DIRECT__DECLARATOR
            ;

DIRECT__DECLARATOR: IDENTIFIER 
                   |
                  '(' DECLARATOR ')'
                   |
                   DIRECT__DECLARATOR '[' CONSTANT__EXPRESSION ']'
                   |
                   DIRECT__DECLARATOR '[' ']'
                   |
                   DIRECT__DECLARATOR '(' PARAMETER__TYPE__LIST ')'
                   |
                   DIRECT__DECLARATOR '(' IDENTIFIER__LIST ')'
                   |
                   DIRECT__DECLARATOR '(' ')'
                   ;

POINTER: '*' TYPE__QUALIFIER__LIST
          |
          '*' TYPE__QUALIFIER__LIST POINTER
          |
          '*' POINTER
          |
          '*'
          ;

TYPE__QUALIFIER__LIST:  TYPE__QUALIFIER
                      |
                      TYPE__QUALIFIER__LIST TYPE__QUALIFIER
                      ;

PARAMETER__TYPE__LIST:  PARAMETER__LIST
                      |
                      PARAMETER__LIST ',' ELLIPSIS
                      ;

PARAMETER__LIST: PARAMETER__DECLARATION
                |
                PARAMETER__LIST ',' PARAMETER__DECLARATION
                ;

PARAMETER__DECLARATION: DECLARATION__SPECIFIERS DECLARATOR
                        |
                        DECLARATION__SPECIFIERS ABSTRACT__DECLARATOR
                        |
                        DECLARATION__SPECIFIERS
                        ;

IDENTIFIER__LIST: IDENTIFIER
                  |
                  IDENTIFIER__LIST ',' IDENTIFIER
                  ;

INITIALIZER: ASSIGNMENT__EXPRESSION
              |
              '{' INITIALIZER__LIST '}'
              |
              '{' INITIALIZER__LIST ',' '}'
              ;

INITIALIZER__LIST: INITIALIZER
                  |
                  INITIALIZER__LIST ',' INITIALIZER
                  ;

TYPE__NAME: SPECIFIER__QUALIFIER__LIST ABSTRACT__DECLARATOR
			| SPECIFIER__QUALIFIER__LIST
          ;

ABSTRACT__DECLARATOR: POINTER
                      |
                      POINTER DIRECT__ABSTRACT__DECLARATOR
                      |
                      DIRECT__ABSTRACT__DECLARATOR
                      ;

DIRECT__ABSTRACT__DECLARATOR: '(' ABSTRACT__DECLARATOR ')'
                            |
                            DIRECT__ABSTRACT__DECLARATOR '[' CONSTANT__EXPRESSION ']'
                            |
                            DIRECT__ABSTRACT__DECLARATOR '[' ']'
                            |
                            '[' CONSTANT__EXPRESSION ']'
                            |
                            '[' ']'
                            |
                            DIRECT__ABSTRACT__DECLARATOR '(' PARAMETER__TYPE__LIST ')'
                            |
                            DIRECT__ABSTRACT__DECLARATOR '(' ')'
                            |
                            '(' PARAMETER__TYPE__LIST ')'
                            |
                            '(' ')'
                            ;

/*TYPEDEF__NAME: IDENTIFIER
              ;*/

STATEMENT:  LABELED__STATEMENT
            |
            EXPRESSION__STATEMENT
            |
            COMPOUND__STATEMENT
            |
            SELECTION__STATEMENT
            |
            ITERATION__STATEMENT
            |
            JUMP__STATEMENT
            ;

LABELED__STATEMENT:  IDENTIFIER ':' STATEMENT
                    |
                    CASE CONSTANT__EXPRESSION ':' STATEMENT
                    |
                    DEFAULT ':' STATEMENT
                    ;

EXPRESSION__STATEMENT: EXPRESSION__OPT ';'
                      ;

COMPOUND__STATEMENT: '{' BLOCK_ITEM_LIST '}'
					|
					'{' '}'
                    ;

BLOCK_ITEM_LIST: BLOCK_ITEM
                  |
                  BLOCK_ITEM_LIST BLOCK_ITEM
                  ;

BLOCK_ITEM:   DECLARATION
              |
              STATEMENT
              ;                                      


SELECTION__STATEMENT:  IF '(' EXPRESSION ')' STATEMENT
                      |
                      IF '(' EXPRESSION ')' STATEMENT ELSE STATEMENT
                      |
                      SWITCH '(' EXPRESSION ')' STATEMENT
                      ;

ITERATION__STATEMENT:  WHILE '(' EXPRESSION ')' STATEMENT
                      |
                      DO STATEMENT WHILE '(' EXPRESSION ')' ';'
                      |
                      FOR '(' EXPRESSION__OPT ';' EXPRESSION__OPT ';' EXPRESSION__OPT ')' STATEMENT
                      ;

JUMP__STATEMENT: GOTO IDENTIFIER';'
                |
                CONTINUE';'
                |
                BREAK';'
                |
                RETURN EXPRESSION__OPT ';'
                ;

EXPRESSION__OPT: EXPRESSION
				|
				;

EXPRESSION: ASSIGNMENT__EXPRESSION
            |
            EXPRESSION ',' ASSIGNMENT__EXPRESSION
            ;

ASSIGNMENT__EXPRESSION:  CONDITIONAL__EXPRESSION
                        |
                        UNARY__EXPRESSION ASSIGNMENT__OPERATOR ASSIGNMENT__EXPRESSION
                        ;

ASSIGNMENT__OPERATOR: '='
                      |
                      MUL_ASSIGN
                      |
                       DIV_ASSIGN
                      |
                       MOD_ASSIGN
                      |
                       ADD_ASSIGN
                      |
                       SUB_ASSIGN
                      |
                       LEFT_ASSIGN
                      |
                       RIGHT_ASSIGN
                      |
                       AND_ASSIGN
                      |
                       XOR_ASSIGN
                      |
                       OR_ASSIGN
                      ;

CONDITIONAL__EXPRESSION: LOGICAL__OR__EXPRESSION
                        |
                        LOGICAL__OR__EXPRESSION '?' EXPRESSION ':' CONDITIONAL__EXPRESSION
                        ;

CONSTANT__EXPRESSION: CONDITIONAL__EXPRESSION
                     ;

LOGICAL__OR__EXPRESSION:  LOGICAL__AND__EXPRESSION
                        |
                        LOGICAL__OR__EXPRESSION OR_OP LOGICAL__AND__EXPRESSION
                        ;

LOGICAL__AND__EXPRESSION: INCLUSIVE__OR__EXPRESSION
						|
						LOGICAL__AND__EXPRESSION AND_OP INCLUSIVE__OR__EXPRESSION
						;

INCLUSIVE__OR__EXPRESSION: EXCLUSIVE__OR__EXPRESSION
						|
						INCLUSIVE__OR__EXPRESSION '|' EXCLUSIVE__OR__EXPRESSION
						;

EXCLUSIVE__OR__EXPRESSION: AND__EXPRESSION
						|
						EXCLUSIVE__OR__EXPRESSION '^' AND__EXPRESSION
						;

AND__EXPRESSION: EQUALITY__EXPRESSION
				|
				AND__EXPRESSION '&' EQUALITY__EXPRESSION
				;

EQUALITY__EXPRESSION: RELATIONAL__EXPRESSION
					|
					EQUALITY__EXPRESSION EQ_OP RELATIONAL__EXPRESSION
					|
					EQUALITY__EXPRESSION NE_OP RELATIONAL__EXPRESSION
					;

RELATIONAL__EXPRESSION: SHIFT__EXPRESSION
					|
					RELATIONAL__EXPRESSION '<' SHIFT__EXPRESSION
					|
					RELATIONAL__EXPRESSION '>' SHIFT__EXPRESSION
					|															
					RELATIONAL__EXPRESSION LE_OP SHIFT__EXPRESSION
					|
					RELATIONAL__EXPRESSION GE_OP SHIFT__EXPRESSION
					;

SHIFT__EXPRESSION: ADDITIVE__EXPRESSION
				|
				SHIFT__EXPRESSION LEFT_OP ADDITIVE__EXPRESSION
				|					
				SHIFT__EXPRESSION RIGHT_OP ADDITIVE__EXPRESSION
				;

ADDITIVE__EXPRESSION: MULTIPLICATIVE__EXPRESSION
					|
					ADDITIVE__EXPRESSION '+' MULTIPLICATIVE__EXPRESSION
					|
					ADDITIVE__EXPRESSION '-' MULTIPLICATIVE__EXPRESSION
					;

MULTIPLICATIVE__EXPRESSION:CAST__EXPRESSION
						|
						MULTIPLICATIVE__EXPRESSION '*' CAST__EXPRESSION
						|
						MULTIPLICATIVE__EXPRESSION '/' CAST__EXPRESSION
						|
						MULTIPLICATIVE__EXPRESSION '%' CAST__EXPRESSION
						;

CAST__EXPRESSION: UNARY__EXPRESSION
				|
				'(' TYPE__NAME ')' CAST__EXPRESSION
				;

UNARY__EXPRESSION: POSTFIX__EXPRESSION
				|
				INC_OP UNARY__EXPRESSION
				|
				DEC_OP UNARY__EXPRESSION
				|
				UNARY__OPERATOR CAST__EXPRESSION
				|
				SIZEOF UNARY__EXPRESSION
				|
				SIZEOF '(' TYPE__NAME ')'
				;

UNARY__OPERATOR : '&'
				|
				'*'
				|
				'+'
				|
				'-'
				|
				'~'
				|
				'!'
				;

POSTFIX__EXPRESSION: PRIMARY__EXPRESSION
					|
					POSTFIX__EXPRESSION '[' EXPRESSION ']'
					|
					POSTFIX__EXPRESSION '(' ARGUMENT__EXPRESSION__LIST ')'
					|
					POSTFIX__EXPRESSION '(' ')'
					|
					POSTFIX__EXPRESSION '.' IDENTIFIER
					|
					POSTFIX__EXPRESSION PTR_OP IDENTIFIER
					|
					POSTFIX__EXPRESSION INC_OP
					|
					POSTFIX__EXPRESSION DEC_OP
					;

PRIMARY__EXPRESSION: IDENTIFIER
					|
					CONSTANT
					|
					STRING
					|
					'(' EXPRESSION ')'
					;

ARGUMENT__EXPRESSION__LIST: ASSIGNMENT__EXPRESSION
						|
						ARGUMENT__EXPRESSION__LIST ',' ASSIGNMENT__EXPRESSION
						;

CONSTANT: INTEGER_CONSTANT
		|
		CHARACTER_CONSTANT
		|
		FLOATING_CONSTANT
		;

CONTROL: CONTROL__LINE CONTROL
		|
		CONTROL__LINE
		;

CONTROL__LINE: '#' DEFINE IDENTIFIER TOKEN_SEQUENCE
			|
			'#' UNDEF IDENTIFIER
			|
			'#' INCLUDE '<' FILENAME '>'
			|
			'#' INCLUDE '"' FILENAME '"'
			|
			'#' LINE CONSTANT '"' FILENAME '"'
			|
			'#' LINE CONSTANT
			|
			'#' ERROR TOKEN_SEQUENCE
			|
			'#' ERROR
			|
			'#' PRAGMA TOKEN_SEQUENCE
			|
			'#' PRAGMA
			|
			'#'
			|
			PREPROCESSOR__CONDITIONAL
			;

PREPROCESSOR__CONDITIONAL: IF__LINE TEXT ELIF__PARTS ELSE__PART '#' "ENDIF"
							| IF__LINE TEXT ELIF__PARTS '#' "ENDIF";

IF__LINE: '#' "IF" CONSTANT__EXPRESSION
		|
		'#' "IFDEF" IDENTIFIER
		|
		'#' "IFNDEF" IDENTIFIER
		;

ELIF__PARTS: ELIF__LINE TEXT
			;

ELIF__LINE : '#' "ELIF" CONSTANT__EXPRESSION;

ELSE__PART: ELSE__LINE TEXT;

ELSE__LINE: '#' "ELSE";

TEXT: TRANSLATION__UNIT;

%%

int main()
{
//	 extern int yy_flex_debug;
	// yy_flex_debug= 1;	
	//yydebug = 1;
 return(yyparse());
}

yyerror(s)
char *s;
{
	extern int yylineno;
  extern char *yytext;
   fprintf(stderr, "FAIL\n",s);
	printf("Line Number: %d Parse Error %s\n",yylineno, yytext );
 
}

yywrap()
{
  return(1);
}
