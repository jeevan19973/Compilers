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
