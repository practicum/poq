
#include "walker_helpers.h"

#include "utils/debugging_helpers.h"
#include "callbacks.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#define WRC_Continue    0   /* Continue down into children */
#define WRC_Prune       1   /* Omit children but continue walking siblings */
#define WRC_Abort       2   /* Abandon the tree walk */



void printExpression( Expr* pExpr, Parse *pParse /*for iAlias*/ )
{
    printf( "\t\t printExpression:\n");

    if( pExpr->flags & EP_IntValue )
    {
        printf( "\t\t\t expression: %d\n", (int) pExpr->u.iValue );
    }
    else
    {
        printf( "\t\t\t expression: %s\n", pExpr->u.zToken );
    }
}

void printSrcList( SrcList* pList )
{
    /*
struct SrcList {
  i16 nSrc;        // Number of tables or subqueries in the FROM clause
  i16 nAlloc;      // Number of entries allocated in a[] below

  struct SrcList_item {
    char *zDatabase;  // Name of database holding this table
    char *zName;      // Name of the table
    char *zAlias;     // The "B" part of a "A AS B" phrase.  zName is the "A"
    Table *pTab;      // An SQL table corresponding to zName
    Select *pSelect;  // A SELECT statement used in place of a table name
    int addrFillSub;  // Address of subroutine to manifest a subquery
    int regReturn;    // Register holding return address of addrFillSub
    u8 jointype;      // Type of join between this table and the previous
    u8 notIndexed;    // True if there is a NOT INDEXED clause
    u8 isCorrelated;  // True if sub-query is correlated
#ifndef SQLITE_OMIT_EXPLAIN
    u8 iSelectId;     // If pSelect!=0, the id of the sub-select in EQP
#endif
    int iCursor;      // The VDBE cursor number used to access this table
    Expr *pOn;        // The ON clause of a join
    IdList *pUsing;   // The USING clause of a join
    Bitmask colUsed;  // Bit N (1<<N) set if column N of pTab is used
    char *zIndex;     // Identifier from "INDEXED BY <zIndex>" clause
    Index *pIndex;    // Index structure corresponding to zIndex, if any
  } a[1];             // One entry for each identifier on the list
};
     */

    printf( "\t\t Begin printSrcList:\n");

    int i = 0;

    for( i = 0; i < pList->nSrc; i++ )
    {
        struct SrcList_item* item = (&( pList->a[i] ));

        if ( item->pSelect )
        {
            RaiseBreakpointSignalOnlyWhenDebuggerExists();
        }
        else
        {
            /*
              #define JT_INNER     0x0001    // Any kind of inner or cross join
              #define JT_CROSS     0x0002    // Explicit use of the CROSS keyword
              #define JT_NATURAL   0x0004    // True for a "natural" join
              #define JT_LEFT      0x0008    // Left outer join
              #define JT_RIGHT     0x0010    // Right outer join
              #define JT_OUTER     0x0020    // The "OUTER" keyword is present
              #define JT_ERROR     0x0040    // unknown or unsupported join type
            */

            printf( "\t\t\t join type: 0x%x\n", (int) item->jointype );
            printf( "\t\t\t table: %s\n", item->zName );
        }
    }

    printf( "\t\t End printSrcList:\n");
}




//////////// can we remove??
/*
** Walk the parse trees associated with all subqueries in the
** FROM clause of SELECT statement p.  Do not invoke the select
** callback on p, but do invoke it on each FROM clause subquery
** and on any subqueries further down in the tree.  Return
** WRC_Abort or WRC_Continue;
*/
int sqlite3WalkSelectFrom(Walker *pWalker, Select *p)
{
    SrcList *pSrc;
    int i;
    struct SrcList_item *pItem;

    pSrc = p->pSrc;
    if( ALWAYS(pSrc) )
    {
        for(i=pSrc->nSrc, pItem=pSrc->a; i>0; i--, pItem++)
        {
            if( sqlite3WalkSelect(pWalker, pItem->pSelect) )
            {
                return WRC_Abort;
            }
        }
    }
    return WRC_Continue;
}

//////////// can we remove??
/*
** Call sqlite3WalkExpr() for every expression in Select statement p.
** Invoke sqlite3WalkSelect() for subqueries in the FROM clause and
** on the compound select chain, p->pPrior.
**
** Return WRC_Continue under normal conditions.  Return WRC_Abort if
** there is an abort request.
**
** If the Walker does not have an xSelectCallback() then this routine
** is a no-op returning WRC_Continue.
*/
int sqlite3WalkSelect(Walker *pWalker, Select *p)
{
    int rc;
    //if( p==0 || pWalker->xSelectCallback==0 ) return WRC_Continue;
    if( p==0 ) return WRC_Continue; /////////////////////////////////////////// FIXME

    rc = WRC_Continue;
    while( p  )
    {
        //rc = pWalker->xSelectCallback(pWalker, p); /////////////////////////////////////////// FIXME
        if( rc ) break;
        if( sqlite3WalkSelectExpr(pWalker, p) ) return WRC_Abort;
        if( sqlite3WalkSelectFrom(pWalker, p) ) return WRC_Abort;
        p = p->pPrior;
    }
    return rc & WRC_Abort;
}



/*
** Walk an expression tree.  Invoke the callback once for each node
** of the expression, while decending.  (In other words, the callback
** is invoked before visiting children.)
**
** The return value from the callback should be one of the WRC_*
** constants to specify how to proceed with the walk.
**
**    WRC_Continue      Continue descending down the tree.
**
**    WRC_Prune         Do not descend into child nodes.  But allow
**                      the walk to continue with sibling nodes.
**
**    WRC_Abort         Do no more callbacks.  Unwind the stack and
**                      return the top-level walk call.
**
** The return value from this routine is WRC_Abort to abandon the tree walk
** and WRC_Continue to continue.
*/
int sqlite3WalkExpr(Walker *pWalker, Expr * const pExpr)
{
    int rc;
    if( pExpr==0 ) return WRC_Continue;

    rc = WRC_Continue;
    //rc = pWalker->xExprCallback(pWalker, pExpr); ////////////////////////////////////////////////// FIXME
    printExpression( pExpr, 0 );

    if( rc==WRC_Continue
        && !ExprHasAnyProperty(pExpr,EP_TokenOnly) ) // ExprHasAnyProperty is a macro in 'callbacks.h'
    {
        if( sqlite3WalkExpr(pWalker, pExpr->pLeft) ) return WRC_Abort;
        if( sqlite3WalkExpr(pWalker, pExpr->pRight) ) return WRC_Abort;

        if( ExprHasProperty(pExpr, EP_xIsSelect) ) // ExprHasProperty is a macro in 'callbacks.h'
        {
            RaiseBreakpointSignalOnlyWhenDebuggerExists();
            if( sqlite3WalkSelect(pWalker, pExpr->x.pSelect) ) return WRC_Abort;
        }
        else
        {
            if( sqlite3WalkExprList(pWalker, pExpr->x.pList) ) return WRC_Abort;
        }
    }
    return rc & WRC_Abort;
}

/*
** Call sqlite3WalkExpr() for every expression in list p or until
** an abort request is seen.
*/
int sqlite3WalkExprList(Walker *pWalker, ExprList *p)
{
    int i;
    struct ExprList_item *pItem;

    if( p )
    {
        for(i=p->nExpr, pItem=p->a; i>0; i--, pItem++)
        {
            if( sqlite3WalkExpr(pWalker, pItem->pExpr) ) return WRC_Abort;
        }
    }
    return WRC_Continue;
}

/*
** Walk all expressions associated with SELECT statement p.  Do
** not invoke the SELECT callback on p, but do (of course) invoke
** any expr callbacks and SELECT callbacks that come from subqueries.
** Return WRC_Abort or WRC_Continue.
*/
int sqlite3WalkSelectExpr(Walker *pWalker, Select *p){
  if( sqlite3WalkExprList(pWalker, p->pEList) ) return WRC_Abort;
  if( sqlite3WalkExpr(pWalker, p->pWhere) ) return WRC_Abort;
  if( sqlite3WalkExprList(pWalker, p->pGroupBy) ) return WRC_Abort;
  if( sqlite3WalkExpr(pWalker, p->pHaving) ) return WRC_Abort;
  if( sqlite3WalkExprList(pWalker, p->pOrderBy) ) return WRC_Abort;
  if( sqlite3WalkExpr(pWalker, p->pLimit) ) return WRC_Abort;
  if( sqlite3WalkExpr(pWalker, p->pOffset) ) return WRC_Abort;
  return WRC_Continue;
}

/* #define WRC_Continue    0   /\* Continue down into children *\/ */
/* #define WRC_Prune       1   /\* Omit children but continue walking siblings *\/ */
/* #define WRC_Abort       2   /\* Abandon the tree walk *\/ */
int walk_sqlite3SelectNew
(
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
    // IMPORTANT:  so far we are not doing anything with 'pSrc' (the FROM clause)
    // IMPORTANT:  so far we are not doing anything with the 'isDistinct' parameter!
    Walker* pWalker = 0;

    printf( "\t\t walking column list:\n");
    if( sqlite3WalkExprList  (pWalker, pEList) ) return WRC_Abort;

    printf( "\t\t walking from clause:\n");
    printSrcList( pSrc );

    printf( "\t\t walking where clause:\n");
    if( sqlite3WalkExpr      (pWalker, pWhere) ) return WRC_Abort;

    printf( "\t\t walking group by clause:\n");
    if( sqlite3WalkExprList  (pWalker, pGroupBy) ) return WRC_Abort;

    printf( "\t\t walking having clause:\n");
    if( sqlite3WalkExpr      (pWalker, pHaving) ) return WRC_Abort;

    printf( "\t\t walking order by clause:\n");
    if( sqlite3WalkExprList  (pWalker, pOrderBy) ) return WRC_Abort;

    printf( "\t\t walking LIMIT clause:\n");
    if( sqlite3WalkExpr      (pWalker, pLimit) ) return WRC_Abort;

    printf( "\t\t walking OFFSET clause:\n");
    if( sqlite3WalkExpr      (pWalker, pOffset) ) return WRC_Abort;

    return WRC_Continue;
}



