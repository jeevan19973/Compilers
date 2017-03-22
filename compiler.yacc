%{
#include<stdio.h>
#include<math.h>
#include<stdlib.h>
%}

%start program

%union { int a;
        }

%token STORAGE_CLASS_SPECIFIER TYPE_SPECIFIER TYPE_QUALIFIER STRUCT_OR_UNION ENUM_SPECIFIER CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%type <a>  program a b c str


%%

TRANSLATION-UNIT: EXTERNAL-DECLARATION
                  |
                  TRANSLATION-UNIT EXTERNAL-DECLARATION
                  ;

EXTERNAL-DECLARATION: FUNCTION-DEFINITION
                      |
                      DECLARATION
                      ;

FUNCTION-DEFINITION:  DECLARATION-SPECIFIERS 
                      |
                      DECLARATOR DECLARATION-LIST
                      |
                      COMPOUND-STATEMENT
                      ;

DECLARATION:  DECLARATION-SPECIFIERS INIT-DECLARATOR-LIST
              |
              ;

DECLARATION-LIST: DECLARATION
                  |
                  DECLARATION-LIST DECLARATION
                  ;

DECLARATION-SPECIFIERS: STORAGE-CLASS-SPECIFIER DECLARATION-SPECIFIERS 
                        |
                        TYPE-SPECIFIER DECLARATION-SPECIFIERS
                        |                          
                        TYPE-QUALIFIER DECLARATION-SPECIFIERS
                        ;

STORAGE-CLASS-SPECIFIER: STORAGE_CLASS_SPECIFIER 
                         ;

TYPE-SPECIFIER: TYPE_SPECIFIER
                |
                STRUCT-OR-UNION-SPECIFIER
                |
                ENUM-SPECIFIER
                |
                TYPEDEF-NAME
                ;

TYPE-QUALIFIER: TYPE_QUALIFIER
                ;

STRUCT-OR-UNION-SPECIFIER: STRUCT-OR-UNION IDENTIFIER '{' STRUCT-DECLARATION-LIST '}'
                           |
                           STRUCT-OR-UNION IDENTIFIER
                           ;

STRUCT-OR-UNION: STRUCT_OR_UNION 
                 ;

STRUCT-DECLARATION-LIST: STRUCT DECLARATION
                         |
                         STRUCT-DECLARATION-LIST STRUCT DECLARATION
                         ;

INIT-DECLARATOR-LIST: INIT-DECLARATOR
                      |
                      INIT-DECLARATOR-LIST ',' INIT-DECLARATOR
                      ;

INIT-DECLARATOR: DECLARATOR
                 |
                 DECLARATOR '=' INITIALIZER
                 ;

STRUCT-DECLARATION: SPECIFIER-QUALIFIER-LIST STRUCT-DECLARATOR-LIST
                    ;

SPECIFIER-QUALIFIER-LIST: TYPE-SPECIFIER SPECIFIER-QUALIFIER-LIST
                          |
                          TYPE-QUALIFIER SPECIFIER-QUALIFIER-LIST
                          |
                          ;

STRUCT-DECLARATOR-LIST: STRUCT-DECLARATOR
                        |
                        STRUCT-DECLARATOR-LIST ',' STRUCT-DECLARATOR
                        ;

STRUCT-DECLARATOR:  DECLARATOR
                    |
                    DECLARATOR ':' CONSTANT-EXPRESSION
                    ;

ENUM-SPECIFIER: ENUM IDENTIFIER '{' ENUMERATOR-LIST '}' 
                |
                ENUM IDENTIFIER
                ;

ENUMERATOR-LIST: ENUMERATOR
                 |
                 ENUMERATOR-LIST ',' ENUMERATOR
                 ;

ENUMERATOR: IDENTIFIER
            |
            IDENTIFIER '=' CONSTANT-EXPRESSION
            ;

DECLARATOR: POINTER
            |
            DIRECT-DECLARATOR
            ;

DIRECT-DECLARATOR: IDENTIFIER 
                   |
                  '(' DECLARATOR ')'
                   |
                   DIRECT-DECLARATOR '[' CONSTANT-EXPRESSION ']'
                   |
                   DIRECT-DECLARATOR '(' PARAMETER-TYPE-LIST ')'
                   |
                   DIRECT-DECLARATOR '(' IDENTIFIER-LIST OPT ')'
                   ;

POINTER: '*' TYPE-QUALIFIER-LIST OPT
          |
          '*' TYPE-QUALIFIER-LIST OPT POINTER
          ;

TYPE-QUALIFIER-LIST:  TYPE-QUALIFIER
                      |
                      TYPE-QUALIFIER-LIST TYPE-QUALIFIER
                      ;

PARAMETER-TYPE-LIST:  PARAMETER-LIST
                      |
                      PARAMETER-LIST ',' PARAMETER-TYPE-LIST
                      ;

PARAMETER-LIST: PARAMETER-DECLARATION
                |
                PARAMETER-LIST ',' PARAMETER-DECLARATION
                ;

PARAMETER-DECLARATION: DECLARATION-SPECIFIERS DECLARATOR
                        |
                        DECLARATION-SPECIFIERS ABSTRACT-DECLARATOR OPT
                        ;

IDENTIFIER-LIST: IDENTIFIER
                  |
                  IDENTIFIER-LIST ',' IDENTIFIER
                  ;

INITIALIZER: ASSIGNMENT-EXPRESSION
              |
              '{' INITIALIZER-LIST '}'
              |
              '{' INITIALIZER-LIST ',' '}'
              ;

INITIALIZER-LIST: INITIALIZER
                  |
                  INITIALIZER-LIST ',' INITIALIZER
                  ;

TYPE-NAME: SPECIFIER-QUALIFIER-LIST ABSTRACT-DECLARATOR OPT
          ;

ABSTRACT-DECLARATOR: POINTER
                      |
                      POINTER OPT DIRECT-ABSTRACT-DECLARATOR
                      ;

DIRECT-ABSTRACT-DECLARATOR: '(' ABSTRACT-DECLARATOR ')'
                            |
                            DIRECT-ABSTRACT-DECLARATOR OPT '[' CONSTANT-EXPRESSION OPT ']'
                            |
                            DIRECT-ABSTRACT-DECLARATOR OPT '(' PARAMETER-TYPE-LIST OPT ')'
                            ;

TYPEDEF-NAME: IDENTIFIER
              ;

STATEMENT:  LABELED-STATEMENT
            |
            EXPRESSION-STATEMENT
            |
            COMPOUND-STATEMENT
            |
            SELECTION-STATEMENT
            |
            ITERATION-STATEMENT
            |
            JUMP-STATEMENT
            ;

LABELED-STATEMENT:  IDENTIFIER ':' STATEMENT
                    |
                    CASE CONSTANT-EXPRESSION ':' STATEMENT
                    |
                    DEFAULT ':' STATEMENT
                    ;

EXPRESSION-STATEMENT: EXPRESSION OPT ';'
                      ;

COMPOUND-STATEMENT: '{' DECLARATION-LIST OPT STATEMENT-LIST OPT '}'
                    ;

STATEMENT-LIST: STATEMENT
                |
                STATEMENT-LIST STATEMENT
                ;

SELECTION-STATEMENT:  IF '(' EXPRESSION ')' STATEMENT
                      |
                      IF '(' EXPRESSION ')' STATEMENT ELSE STATEMENT
                      |
                      SWITCH '(' EXPRESSION ')' STATEMENT
                      ;

ITERATION-STATEMENT:  WHILE '(' EXPRESSION ')' STATEMENT
                      |
                      DO STATEMENT WHILE '(' EXPRESSION ')' ';'
                      |
                      FOR '(' EXPRESSION OPT ';' EXPRESSION OPT ';' EXPRESSION OPT ')' STATEMENT
                      ;

JUMP-STATEMENT: GOTO IDENTIFIER';'
                |
                CONTINUE';'
                |
                BREAK';'
                |
                RETURN EXPRESSION OPT ';'
                ;

EXPRESSION: ASSIGNMENT-EXPRESSION
            |
            EXPRESSION ',' ASSIGNMENT-EXPRESSION
            ;

ASSIGNMENT-EXPRESSION:  CONDITIONAL-EXPRESSION
                        |
                        UNARY-EXPRESSION ASSIGNMENT-OPERATOR ASSIGNMENT-EXPRESSION
                        ;

ASSIGNMENT-OPERATOR:  '=' 
                      |
                      '*''=' 
                      |
                      '/''=' 
                      |
                      '%''=' 
                      |
                      '+''=' 
                      |
                      '-''=' 
                      |
                      '<''<''='
                      |
                      '>''>''=' 
                      |
                      '&''=' 
                      |
                      '^''=' 
                      |
                      '|''='
                      ;

CONDITIONAL-EXPRESSION: LOGICAL-OR-EXPRESSION
                        |
                        LOGICAL-OR-EXPRESSION '?' EXPRESSION ':' CONDITIONAL-EXPRESSION
                        ;

CONSTANT-EXPRESSION: CONDITIONAL-EXPRESSION
                     ;

LOGICAL-OR-EXPRESSION:  LOGICAL-AND-EXPRESSION
                        |
                        LOGICAL-OR-EXPRESSION '|''|' LOGICAL-AND-EXPRESSION
                        ;


%%
int main()
{
 return(yyparse());
}

yyerror(s)
char *s;
{
  fprintf(stderr, "The string is not of the required form\n",s);
}

yywrap()
{
  return(1);
}
