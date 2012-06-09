
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stddef.h>

//#include "tk_defs.h" // we used to get the following 'defines' from tk_defs. but shouldn't lemon be putting them in sql_parse.h ??
#define TK_ISNOT                          146
#define TK_ILLEGAL                        148
#define TK_SPACE                          149
#define TK_COLUMN                         152
#define TK_CONST_FUNC                     155
#define TK_UMINUS                         156
#define TK_UPLUS                          157

#include "sqliteLimit.h"
#include "sql_parse.h"
#include "sql_parse.c"

#include "my_callbacks.c"
#include "my_callbacks2.c"

#include "run_parser_function.c"

/*
  Constant tokens for values 0 and 1.
  Without this, we get a linker error:   In function `yy_reduce':
*/
const Token sqlite3IntTokens[] = {
   { "0", 1 },
   { "1", 1 }
};


int parse_one_string( const char *zSql )
{
    Parse *pParse;            /* Parsing context */
    char *zErrMsg = 0;        /* Error message */
    int rc = 0;               /* Result code */

    pParse = (Parse *) malloc(sizeof(Parse));
    if( ! pParse )
    {
        // this should virtually NEVER happen
        rc = -1;
    }
    else
    {
        printf("parsing: %s\n", zSql );

        memset(pParse, 0, sizeof(*pParse));

        pParse->nQueryLoop = (double)1;

        sqlite3RunParser(pParse, zSql, &zErrMsg);

        assert( 1==(int)pParse->nQueryLoop );

        rc = pParse->rc;

        if ( rc == 0 )
        {
            printf("\tresult: happy parse\n");
        }
        else
        {
            printf("\tresult: oops\n");
        }
    }

    return rc;
}



int main()
{
    const char *one = "select * from lic.LoggableEvent;";
    const char *two = "selectx * from lic.LoggableEvent;";
    const char *three = "select * from; lic.LoggableEvent;";

    parse_one_string( one );
    parse_one_string( two );
    parse_one_string( three );
}
