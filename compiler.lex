%{
	#include "y.tab.h"
	// int yylineno=1;
%}
%%
[\n]   { yylineno = yylineno + 1;}
"/*"			{ comment(); }
"//"[^\n]*              { /* consume //-comment */ }


[ \t] {}

"fname" { return FILENAME;}

"auto"|"register"|"static"|"extern"|"typedef" { return STORAGE_CLASS_SPECIFIER;}

"void"|"char"|"short"|"int"|"long"|"float"|"double"|"signed"|"unsigned" { return TYPE_SPECIFIER;}

"const"|"volatile" { return TYPE_QUALIFIER;}

"struct"|"union" { return STRUCT_OR_UNION;}

"enum" { return ENUM;}

"case" { return CASE;}
"default" { return DEFAULT;}

"if" { return IF;}
"else" { return ELSE;}

"switch" { return SWITCH;}

"while" { return WHILE;}
"do" { return DO;}

"for" { return FOR;}

"goto" { return GOTO;}

"continue" { return CONTINUE;}
"break" { return BREAK;}
"return" { return RETURN;}

"||"	{return OR_OP;}
"&&"	{return AND_OP;}

"..."	{ return ELLIPSIS; }
">>"	{ return RIGHT_OP; }
"<<"	{ return LEFT_OP; }

"++"	{ return INC_OP; }
"--"	{ return DEC_OP; }
"->"	{ return PTR_OP; }

"=="    {return EQ_OP;}
"<="	{ return LE_OP; }
">="	{ return GE_OP; }
"!="	{ return NE_OP; } 

%Assignment
"*="      { return MUL_ASSIGN; }
"/="      { return DIV_ASSIGN; }
"%="      { return MOD_ASSIGN; }
"+="      { return ADD_ASSIGN; }
"-="      { return SUB_ASSIGN; }
"<<="     { return LEFT_ASSIGN; }
">>="     { return RIGHT_ASSIGN; }
"&="      { return AND_ASSIGN; }
"^="      { return XOR_ASSIGN; }
"|="      { return OR_ASSIGN; }

[,{}()[\]#=*+-/<>!|&%~^;:] { return yytext[0];}

"sizeof" { return SIZEOF;}
"define" { return DEFINE;}
"undef" { return UNDEF;}
"include" { return INCLUDE;}
"line" { return LINE;}
"error" { return ERROR;}
"pragma" { return PRAGMA;}
"ifdef" { return IFDEF;}
"ifndef" { return IFNDEF;}
"elif" { return ELIF;} 


[_a-zA-Z][_a-zA-Z0-9]* { return IDENTIFIER;}
[+-]?[0-9]+ { return INTEGER_CONSTANT;}
[+-]?([0-9]*[.])?[0-9]+ { return FLOATING_CONSTANT;}
\"(\\.|[^\\"])*\" { return STRING;}
\'(\\.|[^\\'])\' { return CHARACTER_CONSTANT;}

%[^\n \t]+ { return TOKEN_SEQUENCE;}


%%

void comment(void)
{
	char c, prev = 0;
  
	while ((c = input()) != 0)      /* (EOF maps to 0) */
	{
		if( c == '\n') { yylineno += 1;}
		if (c == '/' && prev == '*')
			return;
		prev = c;
	}
	error("unterminated comment");
}