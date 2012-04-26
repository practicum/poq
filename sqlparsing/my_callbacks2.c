


TriggerStep *sqlite3TriggerUpdateStep(sqlite3* db,Token* tk,ExprList* elist, Expr* e, u8 val)
{
    return NULL;
}



TriggerStep *sqlite3TriggerDeleteStep(sqlite3* db,Token* tk, Expr* e)
{
    return NULL;
}

TriggerStep *sqlite3TriggerSelectStep(sqlite3* db,Select* s)
{
    return NULL;
}

Expr *sqlite3PExpr(Parse* p,int i1, Expr* e, Expr* e2, const Token* tk)
{
    return NULL;
}


ExprList *sqlite3ExprListAppend(Parse* p,ExprList* elist,Expr* e)
{
    return NULL;
}


IdList *sqlite3IdListAppend(sqlite3* db, IdList* idlist, Token* tk)
{
    return NULL;
}






int sqlite3GetInt32(const char *str,int *ip)
{
    return -1;
}


Expr *sqlite3ExprSetCollByToken(Parse *pParse, Expr* e, Token* tk)
{
    return NULL;
}


Expr *sqlite3ExprFunction(Parse* p,ExprList* elist, Token* tk)
{
    return NULL;
}





SrcList *sqlite3SrcListAppend(sqlite3* db, SrcList* slist, Token* tk, Token* tk2)
{
    return NULL;
}

int sqlite3JoinType(Parse* p, Token* tk, Token* tk2, Token* tk3)
{
    return -1;
}





Index *sqlite3CreateIndex(Parse* p,Token* tk,Token* tk2,SrcList* slist,ExprList* elist,int i1,Token* tk3,Token* tk4, int i2, int i3)
{
    return NULL;
}


Select *sqlite3SelectNew(Parse* p,ExprList* elist,SrcList* slist,Expr* e,ExprList* elist2,Expr*e2,ExprList*elist3,int i1,Expr*e3,Expr*e4)
{
    return NULL;
}


SrcList * sqlite3SrcListAppendFromTerm(Parse* p, SrcList* slist, Token* tk, Token* tk1,Token*tk2, Select*s, Expr*e, IdList*ilist)
{
    return NULL;
}



TriggerStep *sqlite3TriggerInsertStep(sqlite3* db,Token* tk, IdList* idlist, ExprList*elist,Select*s,u8 val)
{
    return NULL;
}


void sqlite3BeginTrigger(Parse*p, Token*tk1,Token*tk2,int i1,int i2,IdList* idlist,SrcList* slist, Expr*e,int i3, int i4)
{

}

void sqlite3Savepoint(Parse*p, int i1, Token* tk)
{}

int sqlite3Select(Parse*p, Select*s, SelectDest*sd)
{
    return -1;
}

Expr *sqlite3Expr(sqlite3*db,int i1,const char* str)
{
    return NULL;
}

void sqlite3SrcListShiftJoinType(SrcList* slist)
{}

void sqlite3Update(Parse*p, SrcList*slist, ExprList*elist, Expr*e, int i1)
{}



