/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr = NULL;

#define APPEND_WITH_RANGE_CHECK(ch) \
    /*printf("buffer len:%d, appending <%d>\n", string_buf_ptr - string_buf, ch);*/\
    if (string_buf_ptr - string_buf + 1 < MAX_STR_CONST){\
        *(string_buf_ptr++) = ch; \
    }else{\
        string_buf_ptr = string_buf;\
        cool_yylval.error_msg = strdup("String constant too long");\
        BEGIN(expect_closing_or_new_line);\
        return ERROR;\
    }

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
static int comments_depth = 0;

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>

%x inside_string expect_closing_or_new_line 
%x within_comments within_comments_ex
%%


 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

(?i:class)    return (CLASS);
(?i:else)     return (ELSE);
(?i:fi)       return (FI);
(?i:if)       return (IF);
(?i:in)       return (IN);
(?i:inherits) return (INHERITS);
(?i:let)      return (LET);
(?i:loop)     return (LOOP);
(?i:pool)     return (POOL);
(?i:then)     return (THEN);
(?i:while)    return (WHILE);
(?i:case)     return (CASE);
(?i:esac)     return (ESAC);
(?i:of)       return (OF);
(?i:new)      return (NEW);
(?i:isvoid)   return (ISVOID);
(?i:not)      return (NOT);

 /* boolean constants */
t(?i:rue)     {
    cool_yylval.boolean = 1;
    return BOOL_CONST;
}

f(?i:alse)    {
    cool_yylval.boolean = 0;
    return BOOL_CONST;
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\" {
    string_buf_ptr = string_buf;
    BEGIN(inside_string);
}

<inside_string>{
    /* ordinary pieces */
    [^\\\n\"\0]+ {
        /* ordinary string pieces*/
        //printf("@@@ yytex <%s> len:%d, buf len:%d\n", yytext, yyleng, string_buf_ptr - string_buf);
        if (string_buf_ptr - string_buf + yyleng > MAX_STR_CONST - 1){
            cool_yylval.error_msg = strdup("String constant too long");
            BEGIN(expect_closing_or_new_line);
            return ERROR;
        }else{
            memcpy(string_buf_ptr, yytext, yyleng);
            string_buf_ptr += yyleng;
        }
    }

    \"   {
        /* end of string */
        cool_yylval.symbol = inttable.add_string(string_buf, string_buf_ptr - string_buf);
        //printf("@@ end of string. buf:<%s>\n", string_buf);
        BEGIN(INITIAL);
        return STR_CONST;
    }

    <<EOF>>     {
        cool_yylval.error_msg = strdup("EOF in string constant");
        BEGIN(INITIAL);
        return ERROR;
    }

    \\\b |
    \\b {APPEND_WITH_RANGE_CHECK('\b');}
    \\\f |
    \\f {APPEND_WITH_RANGE_CHECK('\f');}
    \\n {APPEND_WITH_RANGE_CHECK('\n');}
    \\\t |
    \\t {APPEND_WITH_RANGE_CHECK('\t');}

    \\\n {
        /* save as \n for continution */
        curr_lineno++;
        APPEND_WITH_RANGE_CHECK('\n');
    }

    \\\0  {
        cool_yylval.error_msg = strdup("String contains null character");
        BEGIN(expect_closing_or_new_line);
        return ERROR;
    }


    \\[^\b\f\n\t\0] { /* other normal characters like \c*/
        APPEND_WITH_RANGE_CHECK(yytext[1])
    } 

    \n {
        /* no continution, error */
        string_buf_ptr = string_buf;
        cool_yylval.error_msg = strdup("Unterminated string constant");
        BEGIN(INITIAL);
        curr_lineno++;
        return ERROR;
    }
}

<expect_closing_or_new_line>{
    \n      BEGIN(INITIAL);
    \"       BEGIN(INITIAL);
}

 /*
  * lineno count
  */
<*>\n { 
    curr_lineno++; 
    //printf("@@newline id:%d\n", curr_lineno);
}

 /*
  * Int constant
  */

[0-9]+  {
    cool_yylval.symbol = inttable.add_string(yytext, yyleng);
    return INT_CONST;
}

 /*
  * Identifiers
  */
[A-Z]([a-zA-Z0-9_])*   {
    cool_yylval.symbol = inttable.add_string(yytext, yyleng);
    return TYPEID; 
}

[a-z]([a-zA-Z0-9_])*   {
    cool_yylval.symbol = inttable.add_string(yytext, yyleng);
    return OBJECTID; 
}



 /*
  * spaces
  */
[ \f\r\t\v]+  {
    /*skip*/
}

 /*
  * LE and assign
  */
\<-   return ASSIGN;
\<=   return LE;

 /*
  * Special notations
  */
\+|-|\*|\/|~|=|\<           |
\(|\)|\{|\}|,|;|:|\.|\@     return (char)yytext[0];

. {
    //unrecognized character
    //printf("@@@ Unrecognized character <%c>\n", yytext[0]);
    cool_yylval.error_msg = strdup(yytext);
    return ERROR;
}

 /*
  *  Nested comments
  */
--$     /*skip*/
--      BEGIN(within_comments);
<within_comments>{
    <<EOF>> BEGIN(INITIAL);
    [^\n]*  /*skip*/
    [^\n]*\n      {
        BEGIN(INITIAL);
        curr_lineno++;
    }

}


\(\*    {
    BEGIN(within_comments_ex);
    //printf("@@@current line no is :<%d>\n", curr_lineno);
    comments_depth = 1;
    //printf("External, comment depth is:%d, line:%d\n", comments_depth, curr_lineno);
}

<within_comments_ex>{
    \(+\*     {
        comments_depth++;    
        //printf("Nested more, comment depth is:%d, line:%d\n", comments_depth, curr_lineno);
    }

    \([^*\n]?     //printf("ignoring pattern1 <%s>\n", yytext);

    \*+\)     {
        comments_depth--;
        //printf("Nested less, comment depth is:%d, line:%d\n", comments_depth, curr_lineno);
        if (comments_depth == 0){
            BEGIN(INITIAL);
        }
    }

    \*[^\)\n]?\)?    //printf("ignoring pattern2 <%s>\n", yytext);


    <<EOF>> {
        cool_yylval.error_msg = strdup("EOF in comment");
        BEGIN(INITIAL);
        return ERROR;
    }

    [^\(\)\*\n]+\)?   //printf("ignoring pattern3 <%s>\n", yytext);

    [\*\(\)]\n     curr_lineno++;
}

\*\)    {
        cool_yylval.error_msg = strdup("Unmatched *)");
        return ERROR;
}

%%
