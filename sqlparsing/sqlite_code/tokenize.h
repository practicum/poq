#ifndef BORROWED_SQLITE_CODE_SQLITE_TOKENIZER
#define BORROWED_SQLITE_CODE_SQLITE_TOKENIZER

/*
** 2001 September 15
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** An tokenizer for SQL
**
** This file contains C code that splits an SQL input string up into
** individual tokens and sends those tokens one-by-one over to the
** parser for analysis.
*/




/*
** Return the length of the token that begins at z[0].
** Store the token type in *tokenType before returning.
*/
int sqlite3GetToken(const unsigned char *z, int *tokenType);

#endif //#ifndef BORROWED_SQLITE_CODE_SQLITE_TOKENIZER
