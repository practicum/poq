#include "tokenize.c"


/*
** Run the parser on the given SQL string.  The parser structure is
** passed in.  An SQLITE_ status code is returned.  If an error occurs
** then an and attempt is made to write an error message into
** memory obtained from sqlite3_malloc() and to make *pzErrMsg point to that
** error message.
*/
int sqlite3RunParser(Parse *pParse, const char *zSql, char **pzErrMsg)
{
    int nErr = 0;                   /* Number of errors encountered */
    int i = 0;                      /* Loop counter */
    void *pEngine = 0;              /* The LEMON-generated LALR(1) parser */
    int tokenType = 0;              /* type of the next token */
    int lastTokenParsed = -1;       /* type of the previous token */
    const u8 enableLookaside = 0;   /* Saved value of db->lookaside.bEnabled */
    const int mxSqlLen = 1000000;   /* Max length of an SQL string */

    // The maximum number of bytes in the text of an SQL statement is limited to SQLITE_MAX_SQL_LENGTH which defaults to 1000000.

    pParse->rc = 0; //SQLITE_OK;
    pParse->zTail = zSql;
    i = 0;
    assert( pzErrMsg!=0 );
    pEngine = sqlite3ParserAlloc(malloc);

    if( pEngine==0 )
    {
        return 8;//SQLITE_NOMEM;
    }
    assert( pParse->pNewTable==0 );
    assert( pParse->pNewTrigger==0 );
    assert( pParse->nVar==0 );
    assert( pParse->nzVar==0 );
    assert( pParse->azVar==0 );

    while( zSql[i]!=0 )
    {
        assert( i>=0 );
        pParse->sLastToken.z = &zSql[i]; // the entire string 'tail' from current position onward
        pParse->sLastToken.n = // the size (character count) of whichever token just got found
            sqlite3GetToken( (unsigned char*)&zSql[i], &tokenType );
        i += pParse->sLastToken.n;

        if( i>mxSqlLen )
        {
            pParse->rc = 1;// SQLITE_TOOBIG;
            break;
        }

        switch( tokenType )
        {
        case TK_SPACE:
            {
                /*
                  if( db->u1.isInterrupted ){
                  sqlite3ErrorMsg(pParse, "interrupt");
                  pParse->rc = 1;// SQLITE_INTERRUPT;
                  goto abort_parse;
                  }*/
                break; // break from SWITCH, not the while loop...
            }
        case TK_ILLEGAL:
            {
                //sqlite3DbFree(db, *pzErrMsg);
                //*pzErrMsg = sqlite3MPrintf(db, "unrecognized token: \"%T\"",
                //              &pParse->sLastToken);
                nErr++;
                goto abort_parse;
            }
        case TK_SEMI:
            {
                pParse->zTail = &zSql[i];
                /* Fall thru into the default case */
            }
        default:
            {
                sqlite3Parser(pEngine, tokenType, pParse->sLastToken, pParse);
                lastTokenParsed = tokenType;

                //if( pParse->rc!=SQLITE_OK ){
                //  goto abort_parse;
                //}

                break; // break from SWITCH, not the while loop...
            }

        }// end switch statement

    }// end while loop that consumes the statement string

 abort_parse:
    /*
      if( zSql[i]==0 && nErr==0 && pParse->rc==SQLITE_OK ){
      if( lastTokenParsed!=TK_SEMI ){
      sqlite3Parser(pEngine, TK_SEMI, pParse->sLastToken, pParse);
      pParse->zTail = &zSql[i];
      }
      sqlite3Parser(pEngine, 0, pParse->sLastToken, pParse);
      }
      #ifdef YYTRACKMAXSTACKDEPTH
      sqlite3StatusSet(SQLITE_STATUS_PARSER_STACK,
      sqlite3ParserStackPeak(pEngine)
      );
      #endif / * YYDEBUG * /
      sqlite3ParserFree(pEngine, sqlite3_free);
      db->lookaside.bEnabled = enableLookaside;
      if( db->mallocFailed ){
      pParse->rc = SQLITE_NOMEM;
      }
      if( pParse->rc!=SQLITE_OK && pParse->rc!=SQLITE_DONE && pParse->zErrMsg==0 ){
      sqlite3SetString(&pParse->zErrMsg, db, "%s", sqlite3ErrStr(pParse->rc));
      }
      assert( pzErrMsg!=0 );
      if( pParse->zErrMsg ){
      *pzErrMsg = pParse->zErrMsg;
      sqlite3_log(pParse->rc, "%s", *pzErrMsg);
      pParse->zErrMsg = 0;
      nErr++;
      }
      if( pParse->pVdbe && pParse->nErr>0 && pParse->nested==0 ){
      sqlite3VdbeDelete(pParse->pVdbe);
      pParse->pVdbe = 0;
      }
      #ifndef SQLITE_OMIT_SHARED_CACHE
      if( pParse->nested==0 ){
      sqlite3DbFree(db, pParse->aTableLock);
      pParse->aTableLock = 0;
      pParse->nTableLock = 0;
      }
      #endif
      #ifndef SQLITE_OMIT_VIRTUALTABLE
      sqlite3_free(pParse->apVtabLock);
      #endif

      if( !IN_DECLARE_VTAB ){
      / * If the pParse->declareVtab flag is set, do not delete any table
      ** structure built up in pParse->pNewTable. The calling code (see vtab.c)
      ** will take responsibility for freeing the Table structure.
      * /
      sqlite3DeleteTable(db, pParse->pNewTable);
      }

      sqlite3DeleteTrigger(db, pParse->pNewTrigger);
      for(i=pParse->nzVar-1; i>=0; i--) sqlite3DbFree(db, pParse->azVar[i]);
      sqlite3DbFree(db, pParse->azVar);
      sqlite3DbFree(db, pParse->aAlias);
      while( pParse->pAinc ){
      AutoincInfo *p = pParse->pAinc;
      pParse->pAinc = p->pNext;
      sqlite3DbFree(db, p);
      }
      while( pParse->pZombieTab ){
      Table *p = pParse->pZombieTab;
      pParse->pZombieTab = p->pNextZombie;
      sqlite3DeleteTable(db, p);
      }
      if( nErr>0 && pParse->rc==SQLITE_OK ){
      pParse->rc = SQLITE_ERROR;
      }
    */
    sqlite3ParserFree(pEngine, free );

    return nErr;
}


