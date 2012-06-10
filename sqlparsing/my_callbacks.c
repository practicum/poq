#include <stdarg.h> // for va_start
#include  <signal.h>      // for SIGTRAP
#include "callbacks.h"
#include "walker_helpers.h"

// forward declared prototype
Expr *sqlite3ExprAlloc(
  sqlite3 *db,            /* Handle for sqlite3DbMallocZero() (may be null) */
  int op,                 /* Expression opcode */
  const Token *pToken,    /* Token argument.  Might be NULL */
  int dequote             /* True to dequote */
);

// forward declared prototype
char *sqlite3NameFromToken(sqlite3 *db, Token *pName);

// forward declared prototype
Expr *sqlite3Expr
(
 sqlite3*db,
 int op, // expression opcode
 const char* zToken // possibly NULL token
 );


void DebugHelperSilliness()
{
    raise(SIGTRAP);
}



void sqlite3ExprDelete(sqlite3* db, Expr* e)
{
    DebugHelperSilliness();
}


void sqlite3SelectDelete(sqlite3* db, Select* sel)
{
    DebugHelperSilliness();
}


void sqlite3ExprListDelete(sqlite3* db, ExprList* elist)
{
    DebugHelperSilliness();
}

void sqlite3SrcListDelete(sqlite3* db, SrcList* slist)
{
    DebugHelperSilliness();
}


void sqlite3IdListDelete(sqlite3* db, IdList*idlist)
{
    DebugHelperSilliness();
}

void sqlite3DeleteTriggerStep(sqlite3* db, TriggerStep* trigstep)
{
    DebugHelperSilliness();
}


void sqlite3ErrorMsg(Parse* p, const char* str, ...)
{
    va_list vl;
    va_start(vl, str);

    // if there is no '%' (percent char) in 'str', then there is no va arg!!
    // otherwise, there should be a max of one '%' and one token va arg.
    int i;
    for (i=0;i<   1   ;i++)
    {
        Token* val=va_arg(vl,Token*);
        DebugHelperSilliness();
    }
    va_end(vl);

    DebugHelperSilliness();
}

/// the integer is a sqlite flag we do not need.
/// a new SQL statement is beginning to be parsed. initialize structs as needed.
void sqlite3BeginParse(Parse* p,int i1)
{
    p->nVar = 0;
    p->explain = 0;
    DebugHelperSilliness();
}

void sqlite3FinishCoding(Parse* p)
{
    DebugHelperSilliness();
}

void sqlite3BeginTransaction(Parse* p,int i1)
{
    DebugHelperSilliness();
}

void sqlite3CommitTransaction(Parse* p)
{
    DebugHelperSilliness();
}

void sqlite3RollbackTransaction(Parse* p)
{
    DebugHelperSilliness();
}


void sqlite3StartTable(Parse* p,Token* tk,Token* tk2,int i1,int i2,int i3,int i4)
{
    DebugHelperSilliness();
}



void sqlite3EndTable(Parse* p,Token* tk,Token* tk2,Select* sel)
{
    DebugHelperSilliness();
}



void sqlite3AddColumn(Parse* p,Token* tk)
{
    DebugHelperSilliness();
}

void sqlite3AddColumnType(Parse* p,Token* tk)
{
    DebugHelperSilliness();
}



void sqlite3AddDefaultValue(Parse* p,ExprSpan* espan)
{
    DebugHelperSilliness();
}

void sqlite3AddNotNull(Parse* p,int i1)
{
    DebugHelperSilliness();
}

void sqlite3AddPrimaryKey(Parse* p, ExprList* elist,int i1,int i2,int i3)
{
    DebugHelperSilliness();
}


void sqlite3AddCheckConstraint(Parse* p, Expr* e)
{
    DebugHelperSilliness();
}

void sqlite3CreateForeignKey(Parse* p, ExprList* elist, Token* tk, ExprList* elist2,int i1)
{
    DebugHelperSilliness();
}

void sqlite3DeferForeignKey(Parse* p,int i1)
{
    DebugHelperSilliness();
}

void sqlite3AddCollateType(Parse* p, Token* tk)
{
    DebugHelperSilliness();
}




void sqlite3DropTable(Parse* p, SrcList* slist,int i1,int i2)
{
    DebugHelperSilliness();
}

void sqlite3CreateView(Parse* p,Token* tk,Token* tk2,Token* tk3,Select* sel,int i1,int i2)
{
    DebugHelperSilliness();
}


void sqlite3ExprListSetName(Parse* p,ExprList* elist,Token* tk,int i1)
{
    DebugHelperSilliness();
}

void sqlite3ExprListSetSpan(Parse* p,ExprList* elist,ExprSpan* espan)
{
    DebugHelperSilliness();
}



void *sqlite3DbMallocZero(sqlite3* db,int n)
{
    void *p = malloc(n);
    if( p ){
        memset(p, 0, n);
    }

    //DebugHelperSilliness();
    return p;
}




void sqlite3DeleteFrom(Parse* p, SrcList* slist, Expr* e)
{
    DebugHelperSilliness();
}

/// we would only need to implement this if we want to support INDEXED BY or NOT INDEXED
void sqlite3SrcListIndexedBy(Parse *p, SrcList *slist, Token *tk)
{
    if ( tk->z != 0 )
    {
        assert( ! "we do not support the INDEXED keyword");
        DebugHelperSilliness();// verify that tk->z is NULL, otherwise raise!
    }
}

void sqlite3ExprListCheckLength(Parse* p, ExprList* elist, const char* str)
{
    DebugHelperSilliness();
}



void sqlite3Insert(Parse* p, SrcList* slist, ExprList* elist, Select* sel, IdList* idlist,int i1)
{
    DebugHelperSilliness();
}


void sqlite3ExprAssignVarNumber(Parse* p, Expr* e)
{
    DebugHelperSilliness();
}


void sqlite3ExprSetHeight(Parse *pParse, Expr *p)
{
    DebugHelperSilliness();
}


void sqlite3DropIndex(Parse* p, SrcList* slist,int i1)
{
    DebugHelperSilliness();
}

void sqlite3Vacuum(Parse* p)
{
    DebugHelperSilliness();
}

void sqlite3Pragma(Parse* p,Token* tk,Token* tk2,Token* tk3,int i1)
{
    DebugHelperSilliness();
}


void sqlite3FinishTrigger(Parse* p, TriggerStep* trigstep, Token* tk)
{
    DebugHelperSilliness();
}


void sqlite3DropTrigger(Parse* p, SrcList* slist,int i1)
{
    DebugHelperSilliness();
}

void sqlite3Attach(Parse* p, Expr* e, Expr* e2, Expr* e3)
{
    DebugHelperSilliness();
}

void sqlite3Detach(Parse* p, Expr* e)
{
    DebugHelperSilliness();
}



void sqlite3Reindex(Parse* p, Token* tk, Token* tk2)
{
    DebugHelperSilliness();
}



void sqlite3Analyze(Parse* p, Token* tk, Token* tk2)
{
    DebugHelperSilliness();
}

void sqlite3AlterRenameTable(Parse* p, SrcList* slist, Token* tk)
{
    DebugHelperSilliness();
}

void sqlite3AlterFinishAddColumn(Parse *p, Token *tk)
{
    DebugHelperSilliness();
}

void sqlite3AlterBeginAddColumn(Parse *p, SrcList *slist)
{
    DebugHelperSilliness();
}

void sqlite3VtabFinishParse(Parse* p, Token* tk)
{
    DebugHelperSilliness();
}



void sqlite3VtabBeginParse(Parse* p, Token* tk, Token* tk2, Token* tk3)
{
    DebugHelperSilliness();
}

void sqlite3VtabArgInit(Parse* p)
{
    DebugHelperSilliness();
}

void sqlite3VtabArgExtend(Parse* p, Token* tk)
{
    DebugHelperSilliness();
}




void sqlite3ExplainBegin(Vdbe*v)
{
    DebugHelperSilliness();
}

void sqlite3ExplainSelect(Vdbe*v, Select*s)
{
    DebugHelperSilliness();
}

void sqlite3ExplainFinish(Vdbe*v)
{
    DebugHelperSilliness();
}




TriggerStep *sqlite3TriggerUpdateStep(sqlite3* db,Token* tk,ExprList* elist, Expr* e, u8 val)
{
    DebugHelperSilliness();
    return NULL;
}



TriggerStep *sqlite3TriggerDeleteStep(sqlite3* db,Token* tk, Expr* e)
{
    DebugHelperSilliness();
    return NULL;
}

TriggerStep *sqlite3TriggerSelectStep(sqlite3* db,Select* s)
{
    DebugHelperSilliness();
    return NULL;
}

Expr *sqlite3PExpr(Parse* p,int i1, Expr* e, Expr* e2, const Token* tk)
{
    DebugHelperSilliness();
    return NULL;
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
    DebugHelperSilliness();
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



IdList *sqlite3IdListAppend(sqlite3* db, IdList* idlist, Token* tk)
{
    DebugHelperSilliness();
    return NULL;
}






int sqlite3GetInt32(const char *str,int *ip)
{
    DebugHelperSilliness();
    return -1;
}


Expr *sqlite3ExprSetCollByToken(Parse *pParse, Expr* e, Token* tk)
{
    DebugHelperSilliness();
    return NULL;
}


Expr *sqlite3ExprFunction(Parse* p,ExprList* elist, Token* tk)
{
    DebugHelperSilliness();
    return NULL;
}







int sqlite3JoinType(Parse* p, Token* tk, Token* tk2, Token* tk3)
{
    DebugHelperSilliness();
    return -1;
}





Index *sqlite3CreateIndex(Parse* p,Token* tk,Token* tk2,SrcList* slist,ExprList* elist,int i1,Token* tk3,Token* tk4, int i2, int i3)
{
    DebugHelperSilliness();
    return NULL;
}


void printExpressionList( ExprList* pEList, Parse *pParse /*for iAlias*/ )
{
    printf( "\t\t printExpressionList:\n");

    int i = 0;
    for( i = 0; i < pEList->nExpr; i++ )
    {
        /*
          struct ExprList_item {
          Expr *pExpr;           // The list of expressions
          char *zName;           // Token associated with this expression
          char *zSpan;           // Original text of the expression
          u8 sortOrder;          // 1 for DESC or 0 for ASC
          u8 done;               // A flag to indicate when processing is finished
          u16 iOrderByCol;       // For ORDER BY, column number in result set
          u16 iAlias;            // Index into Parse.aAlias[] for zName
          }
         */
        struct ExprList_item* item = (&( pEList->a[i] ));

        printf("\t\t\t original text: %s\n", item->zSpan );
        printf("\t\t\t    token text: %s\n", item->zSpan );
    }

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
    DebugHelperSilliness();

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

    // initial "test printer" to see if i was anywhere near obtaining info that i want. so far so good!
    printExpressionList( pEList, pParse );

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
            raise(SIGTRAP);
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
    DebugHelperSilliness();
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
    DebugHelperSilliness();
    struct SrcList_item *pItem;

    if( !p && (pOn || pUsing) )
    {
        printf("a JOIN clause is required before %s", (pOn ? "ON" : "USING") );
        raise(SIGTRAP);
        goto append_from_error;
    }

    p = sqlite3SrcListAppend(NULL, p, pTable, pDatabase);

    if( p==0 || NEVER(p->nSrc==0) )
    {
        raise(SIGTRAP);
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




TriggerStep *sqlite3TriggerInsertStep(sqlite3* db,Token* tk, IdList* idlist, ExprList*elist,Select*s,u8 val)
{
    DebugHelperSilliness();
    return NULL;
}


void sqlite3BeginTrigger(Parse*p, Token*tk1,Token*tk2,int i1,int i2,IdList* idlist,SrcList* slist, Expr*e,int i3, int i4)
{
    DebugHelperSilliness();
}

void sqlite3Savepoint(Parse*p, int i1, Token* tk)
{
    DebugHelperSilliness();
}

int sqlite3Select(Parse*p, Select*s, SelectDest*sd)
{
    DebugHelperSilliness();
    return -1;
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
    DebugHelperSilliness();
    return sqlite3ExprAlloc(db, op, &x, 0);
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


void sqlite3Update(Parse*p, SrcList*slist, ExprList*elist, Expr*e, int i1)
{
    DebugHelperSilliness();
}



