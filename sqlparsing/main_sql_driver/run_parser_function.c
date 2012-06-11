#include "run_parser_function.h"
#include "tokenize.h"
#include "generated_parser/extra_tokens.h"
#include "generated_parser/lemon_sql_parse.h"
#include "callbacks.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

/*
** Run the parser on the given SQL string.  The parser structure is passed in.
** An SQLITE_ status code is returned.  If an error occurs then an attempt is
** made to write an error message into memory obtained from sqlite3_malloc() and
** to make *pzErrMsg point to that error message.
*/
int sqlite3RunParser(Parse *pParse, const char *zSql, char **pzErrMsg)
{
    int nErr = 0;                   /* Number of errors encountered */
    int positionInStr = 0;          /* position in sql statement string */
    void *pEngine = 0;              /* The LEMON-generated LALR(1) parser */
    int tokenType = 0;              /* type of the next token */
    int lastTokenTypeParsed = -1;   /* type of the previous token */
    const int mxSqlLen = 1000000;   /* Max length of an SQL string */

    // The maximum number of bytes in the text of an SQL statement is limited to SQLITE_MAX_SQL_LENGTH which defaults to 1000000.

    assert( pzErrMsg!=0 );
    pParse->rc = 0; //SQLITE_OK;

    // per the description of 'struct Parse', 'zTail' always holds all remaining text after the last known semicolon
    pParse->zTail = zSql;

    pEngine = sqlite3ParserAlloc(malloc);

    if( pEngine==0 )
    {
        pParse->rc = 8;
        return 8;//SQLITE_NOMEM;
    }
    assert( pParse->pNewTable==0 );
    assert( pParse->pNewTrigger==0 );
    assert( pParse->nVar==0 );
    assert( pParse->nzVar==0 );
    assert( pParse->azVar==0 );

    while( zSql[positionInStr] != 0 )
    {
        assert( positionInStr>=0 );
        pParse->sLastToken.z = &zSql[positionInStr]; // the entire string 'tail' from current position onward
        pParse->sLastToken.n = // the size (character count) of whichever token just got found
            sqlite3GetToken( (unsigned char*)&zSql[positionInStr], &tokenType );
        positionInStr += pParse->sLastToken.n;

        if( positionInStr > mxSqlLen )
        {
            pParse->rc = 1;// SQLITE_TOOBIG;
            break;
        }

        switch( tokenType )
        {
        case TK_SPACE:
            {
                // every time the tokenizer code finds whitespace, we get here. we just ignore it.
                break; // break from SWITCH, not the while loop...
            }
        case TK_ILLEGAL:
            {
                // this will happen if you use invalid tokens. for example: this is ok --> "!=" but this is not --> "!!"
                pParse->rc = -1;
                printf("  unrecognized token: %s\n", pParse->sLastToken.z );
                nErr++;
                goto abort_parse;
            }
        case TK_SEMI:
            {
                // found a SEMICOLON (;)

                // Per the description of 'struct Parse', 'zTail' always holds all remaining text after the last known semicolon
                pParse->zTail = &zSql[positionInStr];
                /* Fall thru into the default case */
            }
        default:
            {
                // normal "happy path" ... just accept this token and continue parsing
                sqlite3Parser(pEngine, tokenType, pParse->sLastToken, pParse);
                lastTokenTypeParsed = tokenType;

                if( pParse->rc != 0 )
                {
                    goto abort_parse;
                }

                break; // break from SWITCH, not the while loop...
            }

        }// end switch statement

    }// end while loop that consumes the statement string

 abort_parse:

    if( zSql[positionInStr]==0   // found null terminator
        && nErr==0               // no counted errors
        && pParse->rc==0         // no parser error state
    )
    {
        // we finished with no errors, but without a ';', so just add a ';' now
        if( lastTokenTypeParsed != TK_SEMI )
        {
            sqlite3Parser(pEngine, TK_SEMI, pParse->sLastToken, pParse);
            pParse->zTail = &zSql[positionInStr];
        }
        sqlite3Parser(pEngine, 0, pParse->sLastToken, pParse);
    }

    // if we somehow counted an error yet pParse shows 'OK' state, then force pParse to show error state
    if( nErr>0 && pParse->rc==0 )
    {
        pParse->rc = 1;//SQLITE_ERROR;
    }

    sqlite3ParserFree(pEngine, free );

    return nErr;
}



