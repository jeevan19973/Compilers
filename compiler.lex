%{

%}
%%
"auto"|"register"|"static"|"extern"|"typedef"|"return" return STORAGE_CLASS_SPECIFIER;

"void"|"char"|"short"|"int"|"long"|"float"|"double"|"signed"|"unsigned" return TYPE_SPECIFIER;

"const"|"volatile" return TYPE_QUALIFIER;

"struct"|"union" return STRUCT_OR_UNION;

"enum" return ENUM_SPECIFIER;

"case" return CASE;
"default" return DEFAULT;

"if" return IF;
"else" return ELSE;

"switch" return SWITCH;

"while" return WHILE;
"do" return DO;

"for" return FOR;

"goto" return GOTO;

"continue" return CONTINUE;
"break" return BREAK;
"return" return RETURN;

"," | "{" | "}" | "(" | ")" | "[" | "]" "#" | "=" | "*" | "+" | "-" | "/" | "<" | ">" | "!" | " |" | "&" | "%" | "~" | "^" return yytext[0];

"sizeof" return SIZEOF;
"define" return DEFINE;
"undef" return UNDEF;
"include" return INCLUDE;
"line" return LINE;
"error" return ERROR;
"pragma" return PRAGMA;
"ifdef" return IFDEF;
"ifndef" return IFNDEF;
"elif" return ELIF; 

%%