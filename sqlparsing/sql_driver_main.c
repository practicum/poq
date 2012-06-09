
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stddef.h>


#include "sqliteLimit.h"
#include "tk_defs.h"
#include <assert.h>
#include "sql_parse.c"

#include "my_callbacks.c"
#include "my_callbacks2.c"

#include "parser_driver_logic.c"

/*
** Constant tokens for values 0 and 1.
*/
const Token sqlite3IntTokens[] = {
   { "0", 1 },
   { "1", 1 }
};


int main()
{
    /*
    // sqlite3ParserAlloc is one of the first things done in sqlite3RunParser
      
    // sql_driver_main.c:7:19: warning: initialization makes pointer from integer without a cast [enabled by default]
    void* pParser = sqlite3ParserAlloc (malloc);
    printf("did alloc\n");

    sqlite3ParserFree(pParser, free );
    printf("did free\n");

    */

    const char *zSql = "select * from lic.LoggableEvent;";

    Parse *pParse;            /* Parsing context */
    char *zErrMsg = 0;        /* Error message */
    int rc = 0;//SQLITE_OK;       /* Result code */
    int i;                    /* Loop counter */


    //  pParse = sqlite3StackAllocZero(db, sizeof(*pParse));
  
    pParse = (Parse *) malloc(sizeof(Parse));
    if( pParse ){
        memset(pParse, 0, sizeof(*pParse));
    }

  
    if( pParse==0 )
    {
        rc = -1;
    }
    else
    {
        pParse->nQueryLoop = (double)1;

        /*
          if( nBytes>=0 && (nBytes==0 || zSql[nBytes-1]!=0) ){
          char *zSqlCopy;
          int mxLen = db->aLimit[SQLITE_LIMIT_SQL_LENGTH];
          testcase( nBytes==mxLen );
          testcase( nBytes==mxLen+1 );
          if( nBytes>mxLen ){
          sqlite3Error(db, SQLITE_TOOBIG, "statement too long");
          rc = sqlite3ApiExit(db, SQLITE_TOOBIG);
          goto end_prepare;
          }
          zSqlCopy = sqlite3DbStrNDup(db, zSql, nBytes);
          if( zSqlCopy ){
          sqlite3RunParser(pParse, zSqlCopy, &zErrMsg);
          sqlite3DbFree(db, zSqlCopy);
          pParse->zTail = &zSql[pParse->zTail-zSqlCopy];
          }else{
          pParse->zTail = &zSql[nBytes];
          }
          }*/

        //else{
        sqlite3RunParser(pParse, zSql, &zErrMsg);
        //}
    
        //assert( 1==(int)pParse->nQueryLoop );

        rc = pParse->rc;
    }

    return rc;
}
