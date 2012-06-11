
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
#include "lemon_sql_parse.h"
#include "lemon_sql_parse.c"

#include "my_callbacks.c"
#include "hodgepodge_borrowed_needs_reorganization.c"

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
    Parse *pParse = 0;        /* Parsing context */
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
        printf("\nparsing: %s\n", zSql );
        memset(pParse, 0, sizeof(*pParse));

        sqlite3RunParser(pParse, zSql, &zErrMsg);

        rc = pParse->rc;

        if ( rc == 0 )
        {
            printf("result: success\n");
        }
        else
        {
            printf("result: oops\n");
        }
    }

    return rc;
}



int main()
{
    /*
      So far, it seems to be the case that for each string that we pass to
      sqlite3RunParser, we _ALWAYS_ get one (or more) call(s) to
      'sqlite3BeginParse'. (We get one per semicolon-delimited statement.)

      Then, depending on whether we SUCCEED at PARSING A VALID statement, we
      _either_ get a call to 'sqlite3FinishCoding' _or_ a call to
      sqlite3BeginParse.

      (at some point, clearly, i will need certainty about these supposed
      guarantees)
    */

    const char *one = "select * from lic.LoggableEvent;";

    // for each semicolon-delimited statement, you get a call to
    // 'sqlite3BeginParse' and 'sqlite3FinishCoding' (all inside the single call
    // to sqlite3RunParser)
    const char *two = "select * from lic.LoggableEvent;select * from lic.LoggableEvent;";

    const char *three = "select * from; lic.LoggableEvent;";
    const char *four = "select 1!!2 from lic.LoggableEvent;";
    const char *five = "selectx * from lic.LoggableEvent;";

    parse_one_string( one );
    parse_one_string( two );
    parse_one_string( three );
    parse_one_string( four );
    parse_one_string( five );

    printf( "\n" );
}
