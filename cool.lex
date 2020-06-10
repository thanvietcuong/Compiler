/* recognize tokens for the calculator and print them out */
%{  
    #include <string.h>
    #include <stdlib.h>
    #include <unistd.h> 
	#include <fcntl.h> 
    #include <ctype.h>

    char* content; //The read content
    int string_len = 0; //Length of the string
    int cur_line = 1; //Current number of line
    int start_err = 0; //The number of error line (if there exist)
    int nest_num = 0;
    long long val = 0;

    enum yytokentype {
        ERROR = 1,
        AT,
        CASE,
        CLASS,
        COLON,
        COMMA,
        DIVIDE,
        DOT,
        ELSE,
        EQUALS,
        ESAC,
        FALSE,
        FI,
        IDENTIFIER,
        IF,
        IN,
        INHERITS,
        INTEGER,
        ISVOID,
        LARROW,
        LBRACE,
        LE,
        LET,
        LOOP,
        LPAREN,
        LT,
        MINUS,
        NEW,
        NOT,
        OF,
        PLUS,
        POOL,
        RARROW,
        RBRACE,
        RPAREN,
        SEMI,
        STRING,
        THEN,
        TILDE,
        TIMES,
        TRUE,
        TYPE,
        WHILE,
        EOL,
        CMT,
        CONST_STRING
   };

   int yylval;
   char *res;

   	//Add the output string to the result file
    void concatResult(char *add){
        char *temp = (char *)malloc(strlen(res) + 1);
        strcpy(temp, res);
        free(res);
        res = (char *)malloc(strlen(temp) + strlen(add) + 2);
        strcpy(res, temp);
        free(temp);
        strcat(res, add);
    }

    //Output string for the current token
    void printString(int tok){
        char addString[10001];
        addString[0] = '\0';
        if(tok == WHILE || tok == IF || tok == ELSE || tok == ELSE || tok == FI || tok == CLASS || tok == ISVOID || tok == CASE || tok == ESAC || tok == LOOP || tok == POOL || tok == INHERITS || tok == FALSE || tok == TRUE || tok == NEW || tok == LET || tok == IN || tok == THEN || tok == OF || tok == NOT){
            //Change string to lower case
            strcpy(content, yytext);
            string_len = strlen(content);
            int i = 0;
            for(i = 0; i < string_len; ++i) content[i] = tolower(content[i]);
            sprintf(addString, "%d\n%s\n", cur_line, content);
        }
        else{
            switch(tok){
                case EOL: return;
                case PLUS:  sprintf(addString, "%d\nplus\n", cur_line);
                        break;
                case MINUS:  sprintf(addString, "%d\nminus\n", cur_line);
                        break;
                case AT:    sprintf(addString, "%d\nat\n", cur_line);
                        break;
                case DIVIDE:  sprintf(addString, "%d\ndivide\n", cur_line);
                        break;
                case LARROW:  sprintf(addString, "%d\nlarrow\n", cur_line);
                        break;
                case LE:  sprintf(addString, "%d\nle\n", cur_line);
                        break;
                case LBRACE:  sprintf(addString, "%d\nlbrace\n", cur_line);
                        break;
                case RBRACE:  sprintf(addString, "%d\nrbrace\n", cur_line);
                        break;
                case LPAREN:  sprintf(addString, "%d\nlparen\n", cur_line);
                        break;
                case RPAREN:  sprintf(addString, "%d\nrparen\n", cur_line);
                        break;
                case TIMES:  sprintf(addString, "%d\ntimes\n", cur_line);
                        break;
                case RARROW:  sprintf(addString, "%d\nrarrow\n", cur_line);
                        break;
                case LT:  sprintf(addString, "%d\nlt\n", cur_line);
                        break;
                case COMMA:  sprintf(addString, "%d\ncomma\n", cur_line);
                        break;
                case COLON:  sprintf(addString, "%d\ncolon\n", cur_line);
                        break;
                case DOT:  sprintf(addString, "%d\ndot\n", cur_line);
                        break;
                case TILDE:  sprintf(addString, "%d\ntilde\n", cur_line);
                        break;
                case SEMI:  sprintf(addString, "%d\nsemi\n", cur_line);
                        break;
                case EQUALS:  sprintf(addString, "%d\nequals\n", cur_line);
                        break;
                case INTEGER: sprintf(addString, "%d\ninteger\n%d\n", cur_line, yylval);
                        break;
                case IDENTIFIER: sprintf(addString, "%d\nidentifier\n%s\n", cur_line, yytext);
                        break;
                case TYPE: sprintf(addString, "%d\ntype\n%s\n", cur_line, yytext);
                        break;
                case CONST_STRING: sprintf(addString, "%d\nstring\n%s\n", cur_line, content);
                        break;
                default: break;
            }
        }
        concatResult(addString);
    }

%}
%x STRING
%x COMMENT

LETTER      [a-zA-Z_]
DIGIT       [0-9]
NEWLINE     (\r\n|\n)+
WHITESPACE  [ \t]*
DASHCOMMENT --.*\n

NSCLASS     (?i:class)
NSNOT       (?i:not)
NSIN        (?i:in)
NSLET       (?i:let)
NSLOOP      (?i:loop)
NSPOOL      (?i:pool)
NSCASE      (?i:case)
NSESAC      (?i:esac)
NSIF        (?i:if)
NSFI        (?i:fi)
NSELSE      (?i:else)
NSINHERITS  (?i:inherits)
NSISVOID    (?i:isvoid)
NSWHILE     (?i:while)
NSNEW       (?i:new)
NSTHEN      (?i:then)
NSOF        (?i:of)

TYPEID      [A-Z]({DIGIT}|{LETTER})*
OBJECTID    [a-z]({DIGIT}|{LETTER})*
INT_CONST   {DIGIT}+

%%
"(*" {
	nest_num = 1;
    BEGIN(COMMENT);
}
<COMMENT><<EOF>> {
    BEGIN(INITIAL);
    printf("ERROR: %d: Lexer: EOF in (* comment *)\n", cur_line);
    return ERROR;
}
<COMMENT>"(*" { nest_num++; }
<COMMENT>\n { cur_line++; }
<COMMENT>. { }
<COMMENT>"*)" {
	nest_num--;
	if(nest_num == 0){
		BEGIN(INITIAL);
    	return CMT;
	} 
}

{DASHCOMMENT} { cur_line++; }
\" {
    string_len = 0;
    start_err = cur_line;
    BEGIN(STRING);
}

<STRING>\\\" {
	string_len += 2;
	content[string_len - 2] = '\\';
	content[string_len - 1] = '\"';
}

<STRING>\0 {
	BEGIN(INITIAL);
    printf("ERROR: %d: Lexer: invalid character: NULL\n", cur_line);
    return ERROR;
}

<STRING>\" {
    BEGIN(INITIAL);
    content[string_len] = '\0';
    return CONST_STRING;
}

<STRING>\n { 
	printf("ERROR: %d: Lexer: invalid character: \"\n", start_err);
	return ERROR; }
<STRING>. { content[string_len++] = yytext[0];}
<STRING><<EOF>> {
    BEGIN(INITIAL);
    printf("ERROR: %d: Lexer: invalid character: \"\n", start_err);
    return ERROR;
}

{NSNOT} 	{ return NOT; }
{NSIN}	{ return IN; }
{NSLET} 	{ return LET; }
{NSCLASS} { return CLASS; }
{NSLOOP} 	{ return LOOP; }
{NSPOOL} 	{return POOL; }
{NSCASE} 	{ return CASE; }
{NSIF} 	{ return IF; }
{NSFI} 	{ return FI; }
{NSELSE} 	{return ELSE; }
{NSINHERITS} 	{return INHERITS; }
{NSISVOID}	{return ISVOID; }
{NSESAC} 	{ return ESAC; }
{NSWHILE} { return WHILE; }
"true" 	{return TRUE; }
"false" {return FALSE; }
{NSNEW} 	{return NEW; }
{NSTHEN} 	{ return THEN; }
{NSOF}	{ return OF; }

"=>"	{ return RARROW; }
"<-"    { return LARROW; }
"<="    { return LE; }
"<"     { return LT; }
"+"    	{ return PLUS; }
"-"    	{ return MINUS; }
"*"    	{ return TIMES; }
"/"    	{ return DIVIDE; }
"@"    	{ return AT; }
"="     { return EQUALS; }
"~"     { return TILDE; }
"{"     { return LBRACE; }
"}"     { return RBRACE; }
"("     { return LPAREN; }
")"     { return RPAREN; }
":"     { return COLON; }
";"     { return SEMI; }
","     { return COMMA; }
"."     { return DOT; }
[0-9]+ 	{   strcpy(content, yytext);
            while(*content == '0') content++;
            if(*content == '\0') content--;
            if(strlen(content) > 10){
                printf("ERROR: %d: Lexer: not a non-negative 32-bit signed integer: %s\n", cur_line, content);
                return ERROR;
            }
            else{
                sscanf(content, "%lli", &val);
                if(val > 2147483647){
                    printf("ERROR: %d: Lexer: not a non-negative 32-bit signed integer: %s\n", cur_line, content);
                    return ERROR;
                }
            }
			yylval = atoi(yytext); 
			return INTEGER; }
[\n]    { cur_line++; return EOL; }
[ \t\r] { }
{OBJECTID} 	{ 
				strcpy(content, yytext); 
				return IDENTIFIER; }
{TYPEID}    { 
				strcpy(content, yytext); 
				return TYPE; }
.      { 
			printf("ERROR: %d: Lexer: invalid character %c\n", cur_line, *yytext);
		 	return ERROR;	}
%%
int main(int argc, char **argv)
{
  	int tok;
    content = (char *)malloc(10000);
  	if(argc == 1){
  		perror("No file input\n");
  		exit(1);
  	}

  	//Open the input file
  	int inFile = open(argv[1], O_RDONLY);
    if (dup2(inFile, 0) != 0) {
        perror("Input does not exist\n");
        exit(1); 
    }
    close(inFile);

  	res = (char *)malloc(100);
  	res[0] = '\0';
  	//Scan tokens
  	while(tok = yylex()) {
  		if(tok == ERROR) exit(1);
    	if(tok != EOL) printString(tok);
  	}

  	int len = strlen(argv[1]);

  	//Create a file name <input>-lex
  	char *fileName, lex[5] = "-lex";
  	fileName = (char *)malloc(len + 10);
  	strcpy(fileName, argv[1]);
  	strcat(fileName, lex);
  	
  	//Open output file
  	int outFile = open(fileName, O_TRUNC|O_CREAT|O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  	if (dup2(outFile, 1) != 1){
  		perror("Cannot create lex file\n");
        exit(1); 
  	}
    close(outFile);

    //Print the result
  	printf("%s", res);
  	return 0;
}