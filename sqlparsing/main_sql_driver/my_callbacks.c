#include "my_callbacks.h"
#include <stdarg.h> // for va_start
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h> // for NULL
#include "callbacks.h"
#include "walker_helpers.h"
#include "generated_parser/extra_tokens.h"
#include "generated_parser/lemon_sql_parse.h"
#include <assert.h>

#include "keywordhash.h"

#define ArraySize(X)    ((int)(sizeof(X)/sizeof(X[0])))

// forward declared prototype (implemented in hodgepodge_borrowed_needs_reorganization.c)
Expr *sqlite3ExprAlloc(
  sqlite3 *db,            /* Handle for sqlite3DbMallocZero() (may be null) */
  int op,                 /* Expression opcode */
  const Token *pToken,    /* Token argument.  Might be NULL */
  int dequote             /* True to dequote */
);

// forward declared prototype
char *sqlite3NameFromToken(sqlite3 *db, Token *pName);// implemented in hodgepodge_borrowed_needs_reorganization.c




// this gets called and we just do nothing
void sqlite3ExplainBegin(Vdbe*v)
{}

// this gets called and we just do nothing
void sqlite3ExplainSelect(Vdbe*v, Select*s)
{}

// this gets called and we just do nothing
void sqlite3ExplainFinish(Vdbe*v)
{}



void *sqlite3DbMallocZero(sqlite3* db,int n)
{
    void *p = malloc(n);
    if( p )
    {
        memset(p, 0, n);
    }

    return p;
}



/*
** Resize the block of memory pointed to by p to n bytes. If the
** resize fails, set the mallocFailed flag in the connection object.
*/
void *sqlite3DbRealloc(sqlite3 *db, void *p, int n)
{
    void *pNew = 0;

    if( p==0 )
    {
        return malloc(n);
    }

    pNew = realloc(p, n);

    return pNew;
}


void sqlite3ErrorMsg(Parse* p, const char* str, ...)
{
    p->rc = 1;
    printf( "  sqlite3ErrorMsg called!\n");
    va_list vl;
    va_start(vl, str);

    // if there is no '%' (percent char) in 'str', then there is no va arg!!
    // otherwise, there should be a max of one '%' and one token va arg.
    int i;
    for (i=0;i<   1   ;i++)
    {
        Token* val=va_arg(vl,Token*);
        printf( "  sqlite3ErrorMsg problem token: \"%s\"\n", val->z );
    }
    va_end(vl);
}

/// the integer is a sqlite flag we do not need.
/// a new SQL statement is beginning to be parsed. initialize structs as needed.
void sqlite3BeginParse(Parse* p,int i1)
{
    p->nVar = 0;
    p->explain = 0;
    RaiseBreakpointSignalOnlyWhenDebuggerExists();
}

// as long as there are NO PARSE ERRORS, then for each semicolon-delimited
// statement, you get a call to 'sqlite3BeginParse' and 'sqlite3FinishCoding'
// (all inside the single call to sqlite3RunParser)
void sqlite3FinishCoding(Parse* p)
{
    RaiseBreakpointSignalOnlyWhenDebuggerExists();
}



/// there are other callbacks beginning with 'sqlite3Expr', but this is the one with the shortest name.
/// "allocate a new expression node from a zero-terminated token that has already been dequoted."
Expr *sqlite3Expr
(
 sqlite3*db,
 int op, // expression opcode
 const char* zToken // possibly NULL token
)
{
    Token x;
    x.z = zToken;
    x.n = zToken ? sqlite3Strlen30(zToken) : 0;
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();
    return sqlite3ExprAlloc(db, op, &x, 0);
}



void sqlite3ExprDelete(sqlite3* db, Expr* e)
{
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();// this function is to avoid memory leaks. that's the least of my concern right now.
}

void sqlite3SelectDelete(sqlite3* db, Select* sel)
{
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();// this function is to avoid memory leaks. that's the least of my concern right now.
}

void sqlite3ExprListDelete(sqlite3* db, ExprList* elist)
{
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();// this function is to avoid memory leaks. that's the least of my concern right now.
}

void sqlite3SrcListDelete(sqlite3* db, SrcList* slist)
{
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();// this function is to avoid memory leaks. that's the least of my concern right now.
}

void sqlite3IdListDelete(sqlite3* db, IdList*idlist)
{
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();// this function is to avoid memory leaks. that's the least of my concern right now.
}


///////////// BEGIN SECTION: "Uncertainty Zone" -- i don't know whether i will _NEED_ to implement these or not...

void sqlite3ExprListSetName(Parse* p,ExprList* elist,Token* tk,int i1)
{
    RaiseBreakpointSignalOnlyWhenDebuggerExists();
}

void sqlite3ExprListSetSpan(Parse* p,ExprList* elist,ExprSpan* espan)
{
    RaiseBreakpointSignalOnlyWhenDebuggerExists(); // this does get called quite often
}

void sqlite3ExprListCheckLength(Parse* p, ExprList* elist, const char* str)
{
    RaiseBreakpointSignalOnlyWhenDebuggerExists();
}

void sqlite3ExprAssignVarNumber(Parse* p, Expr* e)
{
    RaiseBreakpointSignalOnlyWhenDebuggerExists();
}

void sqlite3ExprSetHeight(Parse *pParse, Expr *p)
{
    RaiseBreakpointSignalOnlyWhenDebuggerExists();
}

///////////// END   SECTION: "Uncertainty Zone" -- i don't know whether i will _NEED_ to implement these or not...


/*
** Attach subtrees pLeft and pRight to the Expr node pRoot.
**
** If pRoot==NULL that means that a memory allocation error has occurred.
** In that case, delete the subtrees pLeft and pRight.
*/
void sqlite3ExprAttachSubtrees
(
 sqlite3 *db,
 Expr *pRoot,
 Expr *pLeft,
 Expr *pRight
)
{
    if( pRoot==0 )
    {
        assert( ! "root is NULL and that should not happen here");
    }
    else
    {
        if( pRight )
        {
            pRoot->pRight = pRight;
            if( pRight->flags & EP_ExpCollate )
            {
                pRoot->flags |= EP_ExpCollate;
                pRoot->pColl = pRight->pColl;
            }
        }
        if( pLeft )
        {
            pRoot->pLeft = pLeft;
            if( pLeft->flags & EP_ExpCollate )
            {
                pRoot->flags |= EP_ExpCollate;
                pRoot->pColl = pLeft->pColl;
            }
        }

        // we won't worry about enforcing any maximum height for now
        //exprSetHeight(pRoot);
    }
}


/*
** Allocate a Expr node which joins as many as two subtrees.
**
** One or both of the subtrees can be NULL.  Return a pointer to the new
** Expr node.  Or, if an OOM error occurs, set pParse->db->mallocFailed,
** free the subtrees and return NULL.
*/
Expr *sqlite3PExpr
(
 Parse *pParse,          /* Parsing context */
 int op,                 /* Expression opcode */
 Expr *pLeft,            /* Left operand */
 Expr *pRight,           /* Right operand */
 const Token *pToken     /* Argument token */
)
{
    Expr *p = sqlite3ExprAlloc(pParse->db, op, pToken, 1);
    sqlite3ExprAttachSubtrees(pParse->db, p, pLeft, pRight);

    if( p )
    {
        // we won't worry about enforcing any maximum height for now
        //sqlite3ExprCheckHeight(pParse, p->nHeight);
    }
    return p;
}




/*
** Add a new element to the end of an expression list.  If pList is
** initially NULL, then create a new expression list.
**
** If a memory allocation error occurs, the entire list is freed and
** NULL is returned.  If non-NULL is returned, then it is guaranteed
** that the new entry was successfully appended.
*/
ExprList *sqlite3ExprListAppend(
  Parse *pParse,          /* Parsing context */
  ExprList *pList,        /* List to which to append. Might be NULL */
  Expr *pExpr             /* Expression to be appended. Might be NULL */
)
{
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();
    sqlite3 *db = 0;

    // we want to append to pList, but pList is null so create it
    if( pList==0 )
    {
        pList = sqlite3DbMallocZero(db, sizeof(ExprList) );
        if( pList==0 ){
            goto no_mem;
        }
        assert( pList->nAlloc==0 );
    }

    if( pList->nAlloc <= pList->nExpr )
    {
        struct ExprList_item *a;
        int n = pList->nAlloc*2 + 4;

        //** Resize the block of memory pointed to by p to n bytes. If the
        //** resize fails, set the mallocFailed flag in the connection object.

        a = sqlite3DbRealloc(db, pList->a, n*sizeof(pList->a[0]));
        if( a==0 ){
            goto no_mem;
        }
        pList->a = a;
        pList->nAlloc = n;// /sizeof(a[0]);  // <<------------- FIX
    }

    assert( pList->a!=0 );
    if( 1 )
    {
        struct ExprList_item *pItem = &pList->a[pList->nExpr++];
        memset(pItem, 0, sizeof(*pItem));
        pItem->pExpr = pExpr;
    }

    return pList;

 no_mem:
    /* Avoid leaking memory if malloc has failed. */
    /////// sqlite3ExprDelete(db, pExpr);
    /////// sqlite3ExprListDelete(db, pList);
    return 0;
}




/*
** If zNum represents an integer that will fit in 32-bits, then set
** *pValue to that integer and return true.  Otherwise return false.
**
** Any non-numeric characters that following zNum are ignored.
** This is different from sqlite3Atoi64() which requires the
** input number to be zero-terminated.
*/
int sqlite3GetInt32(const char *zNum, int *pValue)
{
    sqlite_int64 v = 0;
    int i, c;
    int neg = 0;
    if( zNum[0]=='-' ){
        neg = 1;
        zNum++;
    }else if( zNum[0]=='+' ){
        zNum++;
    }
    while( zNum[0]=='0' ) zNum++;
    for(i=0; i<11 && (c = zNum[i] - '0')>=0 && c<=9; i++){
        v = v*10 + c;
    }

    /* The longest decimal representation of a 32 bit integer is 10 digits:
    **
    **             1234567890
    **     2^31 -> 2147483648
    */

    if( i>10 ){
        return 0;
    }

    if( v-neg>2147483647 ){
        return 0;
    }
    if( neg ){
        v = -v;
    }
    *pValue = (int)v;
    return 1;
}




/*
** Construct a new expression node for a function with multiple
** arguments.
*/
Expr *sqlite3ExprFunction(Parse *pParse, ExprList *pList, Token *pToken)
{
    Expr *pNew;

    assert( pToken );
    pNew = sqlite3ExprAlloc(0 /*db*/, TK_FUNCTION, pToken, 1);
    if( pNew==0 )
    {
        sqlite3ExprListDelete(0 /*db*/, pList); /* Avoid memory leak when malloc fails */
        return 0;
    }
    pNew->x.pList = pList;
    assert( !ExprHasProperty(pNew, EP_xIsSelect) );

    // we don't care about height for now
    //sqlite3ExprSetHeight(pParse, pNew);
    return pNew;
}





Select *sqlite3SelectNew(
  Parse *pParse,        /* Parsing context */
  ExprList *pEList,     /* which columns to include in the result */
  SrcList *pSrc,        /* the FROM clause -- which tables to scan */
  Expr *pWhere,         /* the WHERE clause */
  ExprList *pGroupBy,   /* the GROUP BY clause */
  Expr *pHaving,        /* the HAVING clause */
  ExprList *pOrderBy,   /* the ORDER BY clause */
  int isDistinct,       /* true if the DISTINCT keyword is present */
  Expr *pLimit,         /* LIMIT value.  NULL means not used */
  Expr *pOffset         /* OFFSET value.  NULL means no offset */
)
{
    RaiseBreakpointSignalOnlyWhenDebuggerExists();

    assert( !pOffset || pLimit ); /* OFFSET implies LIMIT */
    assert( pOffset==0 || pLimit!=0 );

    // i'm not yet sure WHEN this would happen, but the sqlite code plans for it, so i should too...?
    if( pEList==0 )
    {
        pEList = sqlite3ExprListAppend(pParse, 0, sqlite3Expr(/*db*/NULL,TK_ALL,0));
    }

    // so what the sqlite code does is to create:
    // Select *pNew;

    // most things get copied directly from the incoming parameters to pNew.
    // some things get 'tweaked' on pNew. they are shown here:

    //pNew->selFlags = isDistinct ? SF_Distinct : 0;
    //pNew->op = TK_SELECT;

    walk_sqlite3SelectNew
        ( pParse,
          pEList,
          pSrc,
          pWhere,
          pGroupBy,
          pHaving,
          pOrderBy,
          isDistinct,
          pLimit,
          pOffset );


    return NULL;
}


/*
** Expand the space allocated for the given SrcList object by
** creating nExtra new slots beginning at iStart.  iStart is zero based.
** New slots are zeroed.
**
** For example, suppose a SrcList initially contains two entries: A,B.
** To append 3 new entries onto the end, do this:
**
**    sqlite3SrcListEnlarge(db, pSrclist, 3, 2);
**
** After the call above it would contain:  A, B, nil, nil, nil.
** If the iStart argument had been 1 instead of 2, then the result
** would have been:  A, nil, nil, nil, B.  To prepend the new slots,
** the iStart value would be 0.  The result then would
** be: nil, nil, nil, A, B.
**
** If a memory allocation fails the SrcList is unchanged.  The
** db->mallocFailed flag will be set to true.
*/
SrcList *sqlite3SrcListEnlarge
(
  sqlite3 * unused,  /* Database connection to notify of OOM errors */
  SrcList *pSrc,     /* The SrcList to be enlarged */
  int nExtra,        /* Number of new slots to add to pSrc->a[] */
  int iStart         /* Index in pSrc->a[] of first new slot */
)
{
    int i;

    /* Sanity checking on calling parameters */
    assert( iStart>=0 );
    assert( nExtra>=1 );
    assert( pSrc!=0 );
    assert( iStart<=pSrc->nSrc );

    /* Allocate additional space if needed */
    if( pSrc->nSrc+nExtra > pSrc->nAlloc )
    {
        SrcList *pNew;
        int nAlloc = pSrc->nSrc+nExtra;
        int nGot;

        const int alloc_size = sizeof(*pSrc) + (nAlloc-1)*sizeof(pSrc->a[0]);
        pNew = sqlite3DbRealloc(0, pSrc, alloc_size );

        if( pNew==0 )
        {
            assert( ! "malloc failed" );
            //            assert( 0->mallocFailed ); // 0 was db
            return pSrc;
        }
        pSrc = pNew;
        nGot = alloc_size/sizeof(pSrc->a[0]);
        pSrc->nAlloc = (u16)nGot;
    }

    /* Move existing slots that come after the newly inserted slots
    ** out of the way */
    for(i=pSrc->nSrc-1; i>=iStart; i--)
    {
        pSrc->a[i+nExtra] = pSrc->a[i];
    }
    pSrc->nSrc += (i16)nExtra;

    /* Zero the newly allocated slots */
    memset(&pSrc->a[iStart], 0, sizeof(pSrc->a[0])*nExtra);
    for(i=iStart; i<iStart+nExtra; i++)
    {
        pSrc->a[i].iCursor = -1;
    }

    /* Return a pointer to the enlarged SrcList */
    return pSrc;
}


/*
** Append a new table name to the given SrcList.  Create a new SrcList if
** need be.  A new entry is created in the SrcList even if pTable is NULL.
**
** A SrcList is returned, or NULL if there is an OOM error.  The returned
** SrcList might be the same as the SrcList that was input or it might be
** a new one.  If an OOM error does occurs, then the prior value of pList
** that is input to this routine is automatically freed.
**
** If pDatabase is not null, it means that the table has an optional
** database name prefix.  Like this:  "database.table".  The pDatabase
** points to the table name and the pTable points to the database name.
** The SrcList.a[].zName field is filled with the table name which might
** come from pTable (if pDatabase is NULL) or from pDatabase.
** SrcList.a[].zDatabase is filled with the database name from pTable,
** or with NULL if no database is specified.
**
** In other words, if call like this:
**
**         sqlite3SrcListAppend(D,A,B,0);
**
** Then B is a table name and the database name is unspecified.  If called
** like this:
**
**         sqlite3SrcListAppend(D,A,B,C);
**
** Then C is the table name and B is the database name.  If C is defined
** then so is B.  In other words, we never have a case where:
**
**         sqlite3SrcListAppend(D,A,0,C);
**
** Both pTable and pDatabase are assumed to be quoted.  They are dequoted
** before being added to the SrcList.
*/
SrcList *sqlite3SrcListAppend
(
  sqlite3 * unused,        /* Connection to notify of malloc failures */
  SrcList *pList,     /* Append to this SrcList. NULL creates a new SrcList */
  Token *pTable,      /* Table to append */
  Token *pDatabase    /* Database of the table */
)
{
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();
    struct SrcList_item *pItem;
    assert( pDatabase==0 || pTable!=0 );  /* Cannot have C without B */

    if( pList==0 )
    {
        pList = sqlite3DbMallocZero(0, sizeof(SrcList) );
        if( pList==0 ) return 0;
        pList->nAlloc = 1;
    }

    pList = sqlite3SrcListEnlarge(0, pList, 1, pList->nSrc);

    /* if( db->mallocFailed ) */
    /* { */
    /*     //sqlite3SrcListDelete(0, pList); */
    /*     return 0; */
    /* } */

    pItem = &pList->a[pList->nSrc-1];

    if( pDatabase && pDatabase->z==0 )
    {
        pDatabase = 0;
    }

    if( pDatabase )
    {
        Token *pTemp = pDatabase;
        pDatabase = pTable;
        pTable = pTemp;
    }

    pItem->zName = sqlite3NameFromToken(0, pTable);
    pItem->zDatabase = sqlite3NameFromToken(0, pDatabase);
    return pList;
}


/*
** This routine is called by the parser to add a new term to the
** end of a growing FROM clause.  The "p" parameter is the part of
** the FROM clause that has already been constructed.  "p" is NULL
** if this is the first term of the FROM clause.  pTable and pDatabase
** are the name of the table and database named in the FROM clause term.
** pDatabase is NULL if the database name qualifier is missing - the
** usual case.  If the term has a alias, then pAlias points to the
** alias token.  If the term is a subquery, then pSubquery is the
** SELECT statement that the subquery encodes.  The pTable and
** pDatabase parameters are NULL for subqueries.  The pOn and pUsing
** parameters are the content of the ON and USING clauses.
**
** Return a new SrcList which encodes is the FROM with the new
** term added.
*/
SrcList *sqlite3SrcListAppendFromTerm
(
 Parse *pParse,          /* Parsing context */
 SrcList *p,             /* The left part of the FROM clause already seen */
 // NOTE: pTable and pDatabase actually start out reversed when both are being used. they get swapped.
 Token *pTable,          /* Name of the table to add to the FROM clause */
 // NOTE: pTable and pDatabase actually start out reversed when both are being used. they get swapped.
 Token *pDatabase,       /* Name of the database containing pTable */
 Token *pAlias,          /* The right-hand side of the AS subexpression */
 Select *pSubquery,      /* A subquery used in place of a table name */
 Expr *pOn,              /* The ON clause of a join */
 IdList *pUsing          /* The USING clause of a join */
)
{
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();
    struct SrcList_item *pItem;

    if( !p && (pOn || pUsing) )
    {
        printf("  a JOIN clause is required before %s", (pOn ? "ON" : "USING") );
        assert( ! "disallowed ON/USING" );
        goto append_from_error;
    }

    p = sqlite3SrcListAppend(NULL, p, pTable, pDatabase);

    if( p==0 || NEVER(p->nSrc==0) )
    {
        assert( ! "refer to source code" );
        goto append_from_error;
    }

    pItem = &p->a[p->nSrc-1];
    assert( pAlias!=0 );

    if( pAlias->n )
    {
        pItem->zAlias = sqlite3NameFromToken(0, pAlias);
    }

    pItem->pSelect = pSubquery;
    pItem->pOn = pOn;
    pItem->pUsing = pUsing;

    return p;

 append_from_error:
    /* assert( p==0 ); */
    /* sqlite3ExprDelete(db, pOn); */
    /* sqlite3IdListDelete(db, pUsing); */
    /* sqlite3SelectDelete(db, pSubquery); */
    return 0;
}



/*
sqlite uses this function in order to generate code for the SELECT statement
given in the p argument.

We do not actually want to generate any vdbe byte code, so it seems appropriate
to leave this callback empty for our purposes.

The only thing that might be of interest to us is the value of pDest->eDest:

The results are distributed in various ways depending on the
contents of the SelectDest structure pointed to by argument pDest
as follows:

    pDest->eDest    Result
    ------------    -------------------------------------------
    SRT_Output      Generate a row of output (using the OP_ResultRow
                    opcode) for each row in the result set.

    SRT_Mem         Only valid if the result is a single column.
                    Store the first column of the first result row
                    in register pDest->iParm then abandon the rest
                    of the query.  This destination implies "LIMIT 1".

    SRT_Set         The result must be a single column.  Store each
                    row of result as the key in table pDest->iParm.
                    Apply the affinity pDest->affinity before storing
                    results.  Used to implement "IN (SELECT ...)".

    SRT_Union       Store results as a key in a temporary table pDest->iParm.

    SRT_Except      Remove results from the temporary table pDest->iParm.

    SRT_Table       Store results in temporary table pDest->iParm.
                    This is like SRT_EphemTab except that the table
                    is assumed to already be open.

    SRT_EphemTab    Create an temporary table pDest->iParm and store
                    the result there. The cursor is left open after
                    returning.  This is like SRT_Table except that
                    this destination uses OP_OpenEphemeral to create
                    the table first.

    SRT_Coroutine   Generate a co-routine that returns a new row of
                    results each time it is invoked.  The entry point
                    of the co-routine is stored in register pDest->iParm.

    SRT_Exists      Store a 1 in memory cell pDest->iParm if the result
                    set is not empty.

    SRT_Discard     Throw the results away.  This is used by SELECT
                    statements within triggers whose only purpose is
                    the side-effects of functions.

This routine returns the number of errors.


*/
int sqlite3Select
(
  Parse *pParse,         /* The parser context */
  Select *p,             /* The SELECT statement being coded. */
  SelectDest *pDest      /* What to do with the query results */
)
{
    //RaiseBreakpointSignalOnlyWhenDebuggerExists();
    return 0; // this function is supposed to return the number of errors.
}


/// we probably do not need this??  sqlite does this shifting for some reason. in the 'proof' project we probably don't care...
/*
** When building up a FROM clause in the parser, the join operator
** is initially attached to the left operand.  But the code generator
** expects the join operator to be on the right operand.  This routine
** Shifts all join operators from left to right for an entire FROM
** clause.
**
** Example: Suppose the join is like this:
**
**           A natural cross join B
**
** The operator is "natural cross join".  The A and B operands are stored
** in p->a[0] and p->a[1], respectively.  The parser initially stores the
** operator with A.  This routine shifts that operator over to B.
*/
void sqlite3SrcListShiftJoinType(SrcList *p)
{
    if( p )
    {
        int i;
        assert( p->a || p->nSrc==0 );
        for(i=p->nSrc-1; i>0; i--)
        {
            p->a[i].jointype = p->a[i-1].jointype;
        }
        p->a[0].jointype = 0;
    }
}


/*
** Given 1 to 3 identifiers preceeding the JOIN keyword, determine the
** type of join.  Return an integer constant that expresses that type
** in terms of the following bit values:
**
**     JT_INNER
**     JT_CROSS
**     JT_OUTER
**     JT_NATURAL
**     JT_LEFT
**     JT_RIGHT
**
** A full outer join is the combination of JT_LEFT and JT_RIGHT.
**
** If an illegal or unsupported join type is seen, then still return
** a join type, but put an error in the pParse structure.
*/
int sqlite3JoinType(Parse *pParse, Token *pA, Token *pB, Token *pC)
{
    int jointype = 0;
    Token *apAll[3];
    Token *p;
    /*   0123456789 123456789 123456789 123 */
    static const char zKeyText[] = "naturaleftouterightfullinnercross";
    static const struct {
        u8 i;        /* Beginning of keyword text in zKeyText[] */
        u8 nChar;    /* Length of the keyword in characters */
        u8 code;     /* Join type mask */
    } aKeyword[] = {
        /* natural */ { 0,  7, JT_NATURAL                },
        /* left    */ { 6,  4, JT_LEFT|JT_OUTER          },
        /* outer   */ { 10, 5, JT_OUTER                  },
        /* right   */ { 14, 5, JT_RIGHT|JT_OUTER         },
        /* full    */ { 19, 4, JT_LEFT|JT_RIGHT|JT_OUTER },
        /* inner   */ { 23, 5, JT_INNER                  },
        /* cross   */ { 28, 5, JT_INNER|JT_CROSS         },
    };

    int i, j;
    apAll[0] = pA;
    apAll[1] = pB;
    apAll[2] = pC;

    for(i=0; i<3 && apAll[i]; i++)
    {
        p = apAll[i];
        for(j=0; j<ArraySize(aKeyword); j++){
            if( p->n==aKeyword[j].nChar
                && sqlite3StrNICmp((char*)p->z, &zKeyText[aKeyword[j].i], p->n)==0 ){
                jointype |= aKeyword[j].code;
                break;
            }
        }

        if( j>=ArraySize(aKeyword) ){
            jointype |= JT_ERROR;
            break;
        }
    }

    if(
       (jointype & (JT_INNER|JT_OUTER))==(JT_INNER|JT_OUTER) ||
       (jointype & JT_ERROR)!=0
       )
    {
        const char *zSp = " ";
        assert( pB!=0 );
        if( pC==0 ){ zSp++; }

        sqlite3ErrorMsg(pParse, "unknown or unsupported join type");

        jointype = JT_INNER;
    }
    else if( (jointype & JT_OUTER)!=0
              && (jointype & (JT_LEFT|JT_RIGHT))!=JT_LEFT )
    {
        sqlite3ErrorMsg(pParse, "RIGHT and FULL OUTER JOINs are not currently supported");
        jointype = JT_INNER;
    }
    return jointype;
}


