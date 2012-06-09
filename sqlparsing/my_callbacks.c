#include <stdarg.h> // for va_start
#include  <signal.h>      // for SIGTRAP

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



void *sqlite3DbMallocZero(sqlite3* db,int i1)
{
    DebugHelperSilliness();
}




void sqlite3DeleteFrom(Parse* p, SrcList* slist, Expr* e)
{
    DebugHelperSilliness();
}

void sqlite3SrcListIndexedBy(Parse *p, SrcList *slist, Token *tk)
{
    DebugHelperSilliness();
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


ExprList *sqlite3ExprListAppend(Parse* p,ExprList* elist,Expr* e)
{
    DebugHelperSilliness();
    return NULL;
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





SrcList *sqlite3SrcListAppend(sqlite3* db, SrcList* slist, Token* tk, Token* tk2)
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


Select *sqlite3SelectNew(Parse* p,ExprList* elist,SrcList* slist,Expr* e,ExprList* elist2,Expr*e2,ExprList*elist3,int i1,Expr*e3,Expr*e4)
{
    DebugHelperSilliness();
    return NULL;
}


SrcList * sqlite3SrcListAppendFromTerm(Parse* p, SrcList* slist, Token* tk, Token* tk1,Token*tk2, Select*s, Expr*e, IdList*ilist)
{
    DebugHelperSilliness();
    return NULL;
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

Expr *sqlite3Expr(sqlite3*db,int i1,const char* str)
{
    DebugHelperSilliness();
    return NULL;
}

void sqlite3SrcListShiftJoinType(SrcList* slist)
{
    DebugHelperSilliness();
}

void sqlite3Update(Parse*p, SrcList*slist, ExprList*elist, Expr*e, int i1)
{
    DebugHelperSilliness();
}



