#ifndef PRACTICUM_SQL_PARSING_CALLBACKS_H
#define PRACTICUM_SQL_PARSING_CALLBACKS_H


#include "callbacks.h"

void sqlite3ExprDelete(sqlite3* db, Expr* e);
void sqlite3SelectDelete(sqlite3* db, Select* sel);
void sqlite3ExprListDelete(sqlite3* db, ExprList* elist);
void sqlite3SrcListDelete(sqlite3* db, SrcList* slist);
void sqlite3IdListDelete(sqlite3* db, IdList*idlist);
void sqlite3DeleteTriggerStep(sqlite3* db, TriggerStep* trigstep);
void sqlite3ErrorMsg(Parse* p, const char* str, ...);
void sqlite3BeginParse(Parse* p,int i1);
void sqlite3FinishCoding(Parse* p);
void sqlite3BeginTransaction(Parse* p,int i1);
void sqlite3CommitTransaction(Parse* p);
void sqlite3RollbackTransaction(Parse* p);
void sqlite3StartTable(Parse* p,Token* tk,Token* tk2,int i1,int i2,int i3,int i4);
void sqlite3EndTable(Parse* p,Token* tk,Token* tk2,Select* sel);
void sqlite3AddColumn(Parse* p,Token* tk);
void sqlite3AddColumnType(Parse* p,Token* tk);
void sqlite3AddDefaultValue(Parse* p,ExprSpan* espan);
void sqlite3AddNotNull(Parse* p,int i1);
void sqlite3AddPrimaryKey(Parse* p, ExprList* elist,int i1,int i2,int i3);
void sqlite3AddCheckConstraint(Parse* p, Expr* e);
void sqlite3CreateForeignKey(Parse* p, ExprList* elist, Token* tk, ExprList* elist2,int i1);
void sqlite3DeferForeignKey(Parse* p,int i1);
void sqlite3AddCollateType(Parse* p, Token* tk);
void sqlite3DropTable(Parse* p, SrcList* slist,int i1,int i2);
void sqlite3CreateView(Parse* p,Token* tk,Token* tk2,Token* tk3,Select* sel,int i1,int i2);
void sqlite3ExprListSetName(Parse* p,ExprList* elist,Token* tk,int i1);
void sqlite3ExprListSetSpan(Parse* p,ExprList* elist,ExprSpan* espan);
void *sqlite3DbMallocZero(sqlite3* db,int n);
void sqlite3DeleteFrom(Parse* p, SrcList* slist, Expr* e);
void sqlite3SrcListIndexedBy(Parse *p, SrcList *slist, Token *tk);
void sqlite3ExprListCheckLength(Parse* p, ExprList* elist, const char* str);
void sqlite3Insert(Parse* p, SrcList* slist, ExprList* elist, Select* sel, IdList* idlist,int i1);
void sqlite3ExprAssignVarNumber(Parse* p, Expr* e);
void sqlite3ExprSetHeight(Parse *pParse, Expr *p);
void sqlite3DropIndex(Parse* p, SrcList* slist,int i1);
void sqlite3Vacuum(Parse* p);
void sqlite3Pragma(Parse* p,Token* tk,Token* tk2,Token* tk3,int i1);
void sqlite3FinishTrigger(Parse* p, TriggerStep* trigstep, Token* tk);
void sqlite3DropTrigger(Parse* p, SrcList* slist,int i1);
void sqlite3Attach(Parse* p, Expr* e, Expr* e2, Expr* e3);
void sqlite3Detach(Parse* p, Expr* e);
void sqlite3Reindex(Parse* p, Token* tk, Token* tk2);
void sqlite3Analyze(Parse* p, Token* tk, Token* tk2);
void sqlite3AlterRenameTable(Parse* p, SrcList* slist, Token* tk);
void sqlite3AlterFinishAddColumn(Parse *p, Token *tk);
void sqlite3AlterBeginAddColumn(Parse *p, SrcList *slist);
void sqlite3VtabFinishParse(Parse* p, Token* tk);
void sqlite3VtabBeginParse(Parse* p, Token* tk, Token* tk2, Token* tk3);
void sqlite3VtabArgInit(Parse* p);
void sqlite3VtabArgExtend(Parse* p, Token* tk);
void sqlite3ExplainBegin(Vdbe*v);
void sqlite3ExplainSelect(Vdbe*v, Select*s);
void sqlite3ExplainFinish(Vdbe*v);
void *sqlite3DbRealloc(sqlite3 *db, void *p, int n);
void sqlite3BeginTrigger(Parse*p, Token*tk1,Token*tk2,int i1,int i2,IdList* idlist,SrcList* slist, Expr*e,int i3, int i4);
void sqlite3Savepoint(Parse*p, int i1, Token* tk);
void sqlite3SrcListShiftJoinType(SrcList *p);
void sqlite3Update(Parse*p, SrcList*slist, ExprList*elist, Expr*e, int i1);




TriggerStep *sqlite3TriggerUpdateStep(sqlite3* db,Token* tk,ExprList* elist, Expr* e, u8 val);

TriggerStep *sqlite3TriggerDeleteStep(sqlite3* db,Token* tk, Expr* e);


TriggerStep *sqlite3TriggerSelectStep(sqlite3* db,Select* s);

Expr *sqlite3PExpr(Parse* p,int i1, Expr* e, Expr* e2, const Token* tk);




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
);





IdList *sqlite3IdListAppend(sqlite3* db, IdList* idlist, Token* tk);


int sqlite3GetInt32(const char *str,int *ip);


Expr *sqlite3ExprSetCollByToken(Parse *pParse, Expr* e, Token* tk);



Expr *sqlite3ExprFunction(Parse* p,ExprList* elist, Token* tk);

int sqlite3JoinType(Parse* p, Token* tk, Token* tk2, Token* tk3);


Index *sqlite3CreateIndex(Parse* p,Token* tk,Token* tk2,SrcList* slist,ExprList* elist,int i1,Token* tk3,Token* tk4, int i2, int i3);



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
);




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
);




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
);



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
);




TriggerStep *sqlite3TriggerInsertStep(sqlite3* db,Token* tk, IdList* idlist, ExprList*elist,Select*s,u8 val);


int sqlite3Select(Parse*p, Select*s, SelectDest*sd);


/// there are other callbacks beginning with 'sqlite3Expr', but this is the one with the shortest name.
/// "allocate a new expression node from a zero-terminated token that has already been dequoted."
Expr *sqlite3Expr
(
 sqlite3*db,
 int op, // expression opcode
 const char* zToken // possibly NULL token
);



#endif // #ifndef PRACTICUM_SQL_PARSING_CALLBACKS_H
