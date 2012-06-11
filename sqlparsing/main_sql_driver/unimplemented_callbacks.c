#include "my_callbacks.h"
#include <stdarg.h> // for va_start
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h> // for NULL
#include "callbacks.h"
#include <assert.h>



void sqlite3Insert(Parse* p, SrcList* slist, ExprList* elist, Select* sel, IdList* idlist,int i1)
{
    assert( ! "not implemented: sqlite3Insert" );
    printf( "not implemented: sqlite3Insert\n" );
    abort();
}



void sqlite3DeleteTriggerStep(sqlite3* db, TriggerStep* trigstep)
{
    assert( ! "not implemented: sqlite3DeleteTriggerStep" );
    printf( "not implemented: sqlite3DeleteTriggerStep\n" );
    abort();
}



void sqlite3FinishTrigger(Parse* p, TriggerStep* trigstep, Token* tk)
{
    assert( ! "not implemented: sqlite3FinishTrigger" );
    printf( "not implemented: sqlite3FinishTrigger\n" );
    abort();
}


void sqlite3DropTrigger(Parse* p, SrcList* slist,int i1)
{
    assert( ! "not implemented: sqlite3DropTrigger" );
    printf( "not implemented: sqlite3DropTrigger\n" );
    abort();
}


TriggerStep *sqlite3TriggerUpdateStep(sqlite3* db,Token* tk,ExprList* elist, Expr* e, u8 val)
{
    assert( ! "not implemented: sqlite3TriggerUpdateStep" );
    printf( "not implemented: sqlite3TriggerUpdateStep\n" );
    abort();
    return NULL;
}



TriggerStep *sqlite3TriggerDeleteStep(sqlite3* db,Token* tk, Expr* e)
{
    assert( ! "not implemented: sqlite3TriggerDeleteStep" );
    printf( "not implemented: sqlite3TriggerDeleteStep\n" );
    abort();
    return NULL;
}

TriggerStep *sqlite3TriggerSelectStep(sqlite3* db,Select* s)
{
    assert( ! "not implemented: sqlite3TriggerSelectStep" );
    printf( "not implemented: sqlite3TriggerSelectStep\n" );
    abort();
    return NULL;
}


TriggerStep *sqlite3TriggerInsertStep(sqlite3* db,Token* tk, IdList* idlist, ExprList*elist,Select*s,u8 val)
{
    assert( ! "not implemented: sqlite3TriggerInsertStep" );
    printf( "not implemented: sqlite3TriggerInsertStep\n" );
    abort();
    return NULL;
}


void sqlite3BeginTrigger(Parse*p, Token*tk1,Token*tk2,int i1,int i2,IdList* idlist,SrcList* slist, Expr*e,int i3, int i4)
{
    assert( ! "not implemented: sqlite3TriggerInsertStep" );
    printf( "not implemented: sqlite3TriggerInsertStep\n" );
    abort();
}


void sqlite3BeginTransaction(Parse* p,int i1)
{
    assert( ! "not implemented: sqlite3BeginTransaction" );
    printf( "not implemented: sqlite3BeginTransaction\n" );
    abort();
}

void sqlite3CommitTransaction(Parse* p)
{
    assert( ! "not implemented: sqlite3CommitTransaction" );
    printf( "not implemented: sqlite3CommitTransaction\n" );
    abort();
}

void sqlite3RollbackTransaction(Parse* p)
{
    assert( ! "not implemented: sqlite3RollbackTransaction" );
    printf( "not implemented: sqlite3RollbackTransaction\n" );
    abort();
}


// i believe this is for CREATE TABLE
void sqlite3StartTable(Parse* p,Token* tk,Token* tk2,int i1,int i2,int i3,int i4)
{
    assert( ! "not implemented: sqlite3StartTable" );
    printf( "not implemented: sqlite3StartTable\n" );
    abort();
}


// i believe this is for CREATE TABLE
void sqlite3EndTable(Parse* p,Token* tk,Token* tk2,Select* sel)
{
    assert( ! "not implemented: sqlite3EndTable" );
    printf( "not implemented: sqlite3EndTable\n" );
    abort();
}


// for creating tables
void sqlite3AddColumn(Parse* p,Token* tk)
{
    assert( ! "not implemented: sqlite3AddColumn" );
    printf( "not implemented: sqlite3AddColumn\n" );
    abort();
}

// for creating tables
void sqlite3AddColumnType(Parse* p,Token* tk)
{
    assert( ! "not implemented: sqlite3AddColumnType" );
    printf( "not implemented: sqlite3AddColumnType\n" );
    abort();
}


// for creating tables
void sqlite3AddDefaultValue(Parse* p,ExprSpan* espan)
{
    assert( ! "not implemented: sqlite3AddDefaultValuet" );
    printf( "not implemented: sqlite3AddDefaultValuet\n" );
    abort();
}

// for creating tables
void sqlite3AddNotNull(Parse* p,int i1)
{
    assert( ! "not implemented: sqlite3AddNotNull" );
    printf( "not implemented: sqlite3AddNotNull\n" );
    abort();
}

// for creating tables
void sqlite3AddPrimaryKey(Parse* p, ExprList* elist,int i1,int i2,int i3)
{
    assert( ! "not implemented: sqlite3AddPrimaryKey" );
    printf( "not implemented: sqlite3AddPrimaryKey\n" );
    abort();
}

// for creating tables
void sqlite3AddCheckConstraint(Parse* p, Expr* e)
{
    assert( ! "not implemented: sqlite3AddCheckConstraint" );
    printf( "not implemented: sqlite3AddCheckConstraint\n" );
    abort();
}


void sqlite3CreateForeignKey(Parse* p, ExprList* elist, Token* tk, ExprList* elist2,int i1)
{
    assert( ! "not implemented: sqlite3CreateForeignKey" );
    printf( "not implemented: sqlite3CreateForeignKey\n" );
    abort();
}

void sqlite3DeferForeignKey(Parse* p,int i1)
{
    assert( ! "not implemented: sqlite3DeferForeignKey" );
    printf( "not implemented: sqlite3DeferForeignKey\n" );
    abort();
}

void sqlite3AddCollateType(Parse* p, Token* tk)
{
    assert( ! "not implemented: sqlite3AddCollateType" );
    printf( "not implemented: sqlite3AddCollateType\n" );
    abort();
}




void sqlite3DropTable(Parse* p, SrcList* slist,int i1,int i2)
{
    assert( ! "not implemented: sqlite3DropTable" );
    printf( "not implemented: sqlite3DropTable\n" );
    abort();
}

void sqlite3CreateView(Parse* p,Token* tk,Token* tk2,Token* tk3,Select* sel,int i1,int i2)
{
    assert( ! "not implemented: sqlite3CreateView" );
    printf( "not implemented: sqlite3CreateView\n" );
    abort();
}




void sqlite3DeleteFrom(Parse* p, SrcList* slist, Expr* e)
{
    assert( ! "not implemented: sqlite3DeleteFrom" );
    printf( "not implemented: sqlite3DeleteFrom\n" );
    abort();
}

/// we would only need to implement this if we want to support INDEXED BY or NOT INDEXED
void sqlite3SrcListIndexedBy(Parse *p, SrcList *slist, Token *tk)
{
    if ( tk->z != 0 )
    {
        assert( ! "we do not support the INDEXED keyword");

        assert( ! "not implemented: sqlite3SrcListIndexedBy" );
        printf( "not implemented: sqlite3SrcListIndexedBy\n" );
        abort();
    }
}


void sqlite3DropIndex(Parse* p, SrcList* slist,int i1)
{
    assert( ! "not implemented: sqlite3DropIndex" );
    printf( "not implemented: sqlite3DropIndex\n" );
    abort();
}

void sqlite3Vacuum(Parse* p)
{
    assert( ! "not implemented: sqlite3Vacuum" );
    printf( "not implemented: sqlite3Vacuum\n" );
    abort();
}

void sqlite3Pragma(Parse* p,Token* tk,Token* tk2,Token* tk3,int i1)
{
    assert( ! "not implemented: sqlite3Pragma" );
    printf( "not implemented: sqlite3Pragma\n" );
    abort();
}


void sqlite3Attach(Parse* p, Expr* e, Expr* e2, Expr* e3)
{
    assert( ! "not implemented: sqlite3Attach" );
    printf( "not implemented: sqlite3Attach\n" );
    abort();
}

void sqlite3Detach(Parse* p, Expr* e)
{
    assert( ! "not implemented: sqlite3Detach" );
    printf( "not implemented: sqlite3Detach\n" );
    abort();
}



void sqlite3Reindex(Parse* p, Token* tk, Token* tk2)
{
    assert( ! "not implemented: sqlite3Reindex" );
    printf( "not implemented: sqlite3Reindex\n" );
    abort();
}



void sqlite3Analyze(Parse* p, Token* tk, Token* tk2)
{
    assert( ! "not implemented: sqlite3Analyze" );
    printf( "not implemented: sqlite3Analyze\n" );
    abort();
}

void sqlite3AlterRenameTable(Parse* p, SrcList* slist, Token* tk)
{
    assert( ! "not implemented: sqlite3AlterRenameTable" );
    printf( "not implemented: sqlite3AlterRenameTable\n" );
    abort();
}

void sqlite3AlterFinishAddColumn(Parse *p, Token *tk)
{
    assert( ! "not implemented: sqlite3AlterFinishAddColumn" );
    printf( "not implemented: sqlite3AlterFinishAddColumn\n" );
    abort();
}

void sqlite3AlterBeginAddColumn(Parse *p, SrcList *slist)
{
    assert( ! "not implemented: sqlite3AlterBeginAddColumn" );
    printf( "not implemented: sqlite3AlterBeginAddColumn\n" );
    abort();
}



void sqlite3VtabFinishParse(Parse* p, Token* tk)
{
    assert( ! "not implemented: sqlite3VtabFinishParse" );
    printf( "not implemented: sqlite3VtabFinishParse\n" );
    abort();
}



void sqlite3VtabBeginParse(Parse* p, Token* tk, Token* tk2, Token* tk3)
{
    assert( ! "not implemented: sqlite3VtabBeginParse" );
    printf( "not implemented: sqlite3VtabBeginParse\n" );
    abort();
}

void sqlite3VtabArgInit(Parse* p)
{
    assert( ! "not implemented: sqlite3VtabArgInit" );
    printf( "not implemented: sqlite3VtabArgInit\n" );
    abort();
}

void sqlite3VtabArgExtend(Parse* p, Token* tk)
{
    assert( ! "not implemented: sqlite3VtabArgExtend" );
    printf( "not implemented: sqlite3VtabArgExtend\n" );
    abort();
}

Index *sqlite3CreateIndex(Parse* p,Token* tk,Token* tk2,SrcList* slist,ExprList* elist,int i1,Token* tk3,Token* tk4, int i2, int i3)
{
    assert( ! "not implemented: sqlite3CreateIndex" );
    printf( "not implemented: sqlite3CreateIndex\n" );
    abort();
    return NULL;
}


void sqlite3Savepoint(Parse*p, int i1, Token* tk)
{
    assert( ! "not implemented: sqlite3Savepoint" );
    printf( "not implemented: sqlite3Savepoint\n" );
    abort();
}


void sqlite3Update(Parse*p, SrcList*slist, ExprList*elist, Expr*e, int i1)
{
    assert( ! "not implemented: sqlite3Update" );
    printf( "not implemented: sqlite3Update\n" );
    abort();
}
