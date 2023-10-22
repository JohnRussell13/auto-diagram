%option noyywrap yylineno
%{
    #include "definitions.h"
    #include "parser.tab.h"
%}

%%

    /* SKIP SPACES AND COMMENTS */
[ \t\n]+            { /* skip */ }
"//".*\n            { /* skip */ }
"/*"(.|\n)*"*/"     { /* skip */ }  // WORKS



    /* BUILT-IN FUNCTIONS */
"if"        { return _IF; }
"else"      { return _ELSE; }
"switch"    { return _SWITCH; }
"case"      { return _CASE; }
"default"   { return _DEFAULT;}
"continue"  { return _CONTINUE; }
"break"     { return _BREAK; }
"while"     { return _WHILE; }
"do"        { return _DO; }
"for"       { return _FOR; }
"return"    { return _RETURN; }

    /* SPECIAL KEYWORDS */
"#define"|"#DEFINE"     { return _DEF; }
"const"                 { return _CONST;}
"NULL"|"null"           { return _NULL;}

    /* DATA TYPES */
"int"       { yylval.i = INT;  return _TYPE; }
"unsigned"  { yylval.i = UINT; return _TYPE; }
"void"      { yylval.i = VOID; return _TYPE; }

    /* SPECIAL SYMBOLS */
"("         { return _LPAREN; }
")"         { return _RPAREN; }
"["         { return _LSQBRACK; }
"]"         { return _RSQBRACK; }
"{"         { return _LBRACKET; }
"}"         { return _RBRACKET; }
";"         { return _SEMICOLON; }
","         { return _COMMA; }
":"         { return _COLON; }
"="         { return _ASSIGN; }

    /* ARITHMETIC OPERATIONS */
"+"         { return _PLUS; }
"-"         { return _MINUS; }
"/"         { return _DIV; }
"%"         { return _MOD; }
">>"        { return _SR; }
"<<"        { return _SL; }
"|"         { return _BOR; }
"^"         { return _BXOR; } /* NOT IS MISSING */
"&&"        { return _AND; }
"||"        { return _OR; }

"*"         { return _STAR; } /* STAR INSTEAD OF MUL SINCE IT CAN DENOTE A POINTER */
"&"         { return _AMP; } /* CAN DENOTE ADDRES AS WELL */

"++"        { yylval.i = INC;  return _ITER; }
"--"        { yylval.i = DEC;  return _ITER; }

    /* RELATION OPERATIONS */
"<"         { yylval.i = LT;  return _RELOP; }
"<="        { yylval.i = LEQ; return _RELOP; }
">"         { yylval.i = GT;  return _RELOP; }
">="        { yylval.i = GEQ; return _RELOP; }
"=="        { yylval.i = EQ;  return _RELOP; }
"!="        { yylval.i = NEQ; return _RELOP; }

    /* NAMES AND VALUES */
[a-zA-Z][a-zA-Z0-9_]*   { yylval.s = strdup(yytext); return _ID; } // WHY FTN? WHY??? ..... 
[0-9]{1,10}             { yylval.s = strdup(yytext); return _INT_NUMBER; } /* CAREFULL WITH x = -5; vs 3-5; */
[0-9]{1,10}[uU]         { yylval.s = strdup(yytext); return _UINT_NUMBER; }
0[xX][0-9a-fA-F]{1,4}   { yylval.s = strdup(yytext); return _HEX_NUMBER;}


    /* OTHER */
.           { printf("line %d: LEXICAL ERROR on char %c\n", yylineno, *yytext); }

%%
