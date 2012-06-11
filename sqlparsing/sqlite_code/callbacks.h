#ifndef BORROWED_SQLITE_CODE_FOR_CALLBACKS
#define BORROWED_SQLITE_CODE_FOR_CALLBACKS

#include "sqliteLimit.h"
/*
** Forward references to structures
*/
typedef struct AggInfo AggInfo;
typedef struct AuthContext AuthContext;
typedef struct AutoincInfo AutoincInfo;
typedef struct Bitvec Bitvec;
typedef struct CollSeq CollSeq;
typedef struct Column Column;
typedef struct Db Db;
typedef struct Schema Schema;
typedef struct Expr Expr;
typedef struct ExprList ExprList;
typedef struct ExprSpan ExprSpan;
typedef struct FKey FKey;
typedef struct FuncDestructor FuncDestructor;
typedef struct FuncDef FuncDef;
typedef struct FuncDefHash FuncDefHash;
typedef struct IdList IdList;
typedef struct Index Index;
typedef struct IndexSample IndexSample;
typedef struct KeyClass KeyClass;
typedef struct KeyInfo KeyInfo;
typedef struct Lookaside Lookaside;
typedef struct LookasideSlot LookasideSlot;
typedef struct Module Module;
typedef struct NameContext NameContext;
typedef struct Parse Parse;
typedef struct RowSet RowSet;
typedef struct Savepoint Savepoint;
typedef struct Select Select;
typedef struct SrcList SrcList;
typedef struct StrAccum StrAccum;
typedef struct Table Table;
typedef struct TableLock TableLock;
typedef struct Token Token;
typedef struct Trigger Trigger;
typedef struct TriggerPrg TriggerPrg;
typedef struct TriggerStep TriggerStep;
typedef struct UnpackedRecord UnpackedRecord;
typedef struct VTable VTable;
typedef struct VtabCtx VtabCtx;
typedef struct Walker Walker;
typedef struct WherePlan WherePlan;
typedef struct WhereInfo WhereInfo;
typedef struct WhereLevel WhereLevel;

typedef struct SelectDest SelectDest;
typedef struct sqlite3 sqlite3;
typedef struct Vdbe Vdbe;

typedef struct sqlite3_vfs sqlite3_vfs;
typedef struct sqlite3_mutex sqlite3_mutex;
typedef struct sqlite3_value sqlite3_value;
typedef struct lookaside lookaside;
typedef struct Hash Hash;
typedef struct BusyHandler BusyHandler;
typedef struct sqlite3_context sqlite3_context;
typedef struct Btree Btree;


/*
** The ALWAYS and NEVER macros surround boolean expressions which 
** are intended to always be true or false, respectively.  Such
** expressions could be omitted from the code completely.  But they
** are included in a few cases in order to enhance the resilience
** of SQLite to unexpected behavior - to make the code "self-healing"
** or "ductile" rather than being "brittle" and crashing at the first
** hint of unplanned behavior.
**
** In other words, ALWAYS and NEVER are added for defensive code.
**
** When doing coverage testing ALWAYS and NEVER are hard-coded to
** be true and false so that the unreachable code then specify will
** not be counted as untested code.
*/
#if defined(SQLITE_COVERAGE_TEST)
# define ALWAYS(X)      (1)
# define NEVER(X)       (0)
#elif !defined(NDEBUG)
# define ALWAYS(X)      ((X)?1:(assert(0),0))
# define NEVER(X)       ((X)?(assert(0),1):0)
#else
# define ALWAYS(X)      (X)
# define NEVER(X)       (X)
#endif




/*
** The following macros are used to suppress compiler warnings and to
** make it clear to human readers when a function parameter is deliberately 
** left unused within the body of a function. This usually happens when
** a function is called via a function pointer. For example the 
** implementation of an SQL aggregate step callback may not use the
** parameter indicating the number of arguments passed to the aggregate,
** if it knows that this is enforced elsewhere.
**
** When a function parameter is not used at all within the body of a function,
** it is generally named "NotUsed" or "NotUsed2" to make things even clearer.
** However, these macros may also be used to suppress warnings related to
** parameters that may or may not be used depending on compilation options.
** For example those parameters only used in assert() statements. In these
** cases the parameters are named as per the usual conventions.
*/
#define UNUSED_PARAMETER(x) (void)(x)
#define UNUSED_PARAMETER2(x,y) UNUSED_PARAMETER(x),UNUSED_PARAMETER(y)




/*
** Size of the column cache
*/
#ifndef SQLITE_N_COLCACHE
# define SQLITE_N_COLCACHE 10
#endif


/*
** CAPI3REF: 64-Bit Integer Types
** KEYWORDS: sqlite_int64 sqlite_uint64
**
** Because there is no cross-platform way to specify 64-bit integer types
** SQLite includes typedefs for 64-bit signed and unsigned integers.
**
** The sqlite3_int64 and sqlite3_uint64 are the preferred type definitions.
** The sqlite_int64 and sqlite_uint64 types are supported for backwards
** compatibility only.
**
** ^The sqlite3_int64 and sqlite_int64 types can store integer values
** between -9223372036854775808 and +9223372036854775807 inclusive.  ^The
** sqlite3_uint64 and sqlite_uint64 types can store integer values 
** between 0 and +18446744073709551615 inclusive.
*/
#ifdef SQLITE_INT64_TYPE
  typedef SQLITE_INT64_TYPE sqlite_int64;
  typedef unsigned SQLITE_INT64_TYPE sqlite_uint64;
#elif defined(_MSC_VER) || defined(__BORLANDC__)
  typedef __int64 sqlite_int64;
  typedef unsigned __int64 sqlite_uint64;
#else
  typedef long long int sqlite_int64;
  typedef unsigned long long int sqlite_uint64;
#endif
typedef sqlite_int64 sqlite3_int64;
typedef sqlite_uint64 sqlite3_uint64;

/*
** If compiling for a processor that lacks floating point support,
** substitute integer for floating-point.
*/
#ifdef SQLITE_OMIT_FLOATING_POINT
# define double sqlite3_int64
#endif


/*
** CAPI3REF: Run-Time Limit Categories
** KEYWORDS: {limit category} {*limit categories}
**
** These constants define various performance limits
** that can be lowered at run-time using [sqlite3_limit()].
** The synopsis of the meanings of the various limits is shown below.
** Additional information is available at [limits | Limits in SQLite].
**
** <dl>
** [[SQLITE_LIMIT_LENGTH]] ^(<dt>SQLITE_LIMIT_LENGTH</dt>
** <dd>The maximum size of any string or BLOB or table row, in bytes.<dd>)^
**
** [[SQLITE_LIMIT_SQL_LENGTH]] ^(<dt>SQLITE_LIMIT_SQL_LENGTH</dt>
** <dd>The maximum length of an SQL statement, in bytes.</dd>)^
**
** [[SQLITE_LIMIT_COLUMN]] ^(<dt>SQLITE_LIMIT_COLUMN</dt>
** <dd>The maximum number of columns in a table definition or in the
** result set of a [SELECT] or the maximum number of columns in an index
** or in an ORDER BY or GROUP BY clause.</dd>)^
**
** [[SQLITE_LIMIT_EXPR_DEPTH]] ^(<dt>SQLITE_LIMIT_EXPR_DEPTH</dt>
** <dd>The maximum depth of the parse tree on any expression.</dd>)^
**
** [[SQLITE_LIMIT_COMPOUND_SELECT]] ^(<dt>SQLITE_LIMIT_COMPOUND_SELECT</dt>
** <dd>The maximum number of terms in a compound SELECT statement.</dd>)^
**
** [[SQLITE_LIMIT_VDBE_OP]] ^(<dt>SQLITE_LIMIT_VDBE_OP</dt>
** <dd>The maximum number of instructions in a virtual machine program
** used to implement an SQL statement.  This limit is not currently
** enforced, though that might be added in some future release of
** SQLite.</dd>)^
**
** [[SQLITE_LIMIT_FUNCTION_ARG]] ^(<dt>SQLITE_LIMIT_FUNCTION_ARG</dt>
** <dd>The maximum number of arguments on a function.</dd>)^
**
** [[SQLITE_LIMIT_ATTACHED]] ^(<dt>SQLITE_LIMIT_ATTACHED</dt>
** <dd>The maximum number of [ATTACH | attached databases].)^</dd>
**
** [[SQLITE_LIMIT_LIKE_PATTERN_LENGTH]]
** ^(<dt>SQLITE_LIMIT_LIKE_PATTERN_LENGTH</dt>
** <dd>The maximum length of the pattern argument to the [LIKE] or
** [GLOB] operators.</dd>)^
**
** [[SQLITE_LIMIT_VARIABLE_NUMBER]]
** ^(<dt>SQLITE_LIMIT_VARIABLE_NUMBER</dt>
** <dd>The maximum index number of any [parameter] in an SQL statement.)^
**
** [[SQLITE_LIMIT_TRIGGER_DEPTH]] ^(<dt>SQLITE_LIMIT_TRIGGER_DEPTH</dt>
** <dd>The maximum depth of recursion for triggers.</dd>)^
** </dl>
*/
#define SQLITE_LIMIT_LENGTH                    0
#define SQLITE_LIMIT_SQL_LENGTH                1
#define SQLITE_LIMIT_COLUMN                    2
#define SQLITE_LIMIT_EXPR_DEPTH                3
#define SQLITE_LIMIT_COMPOUND_SELECT           4
#define SQLITE_LIMIT_VDBE_OP                   5
#define SQLITE_LIMIT_FUNCTION_ARG              6
#define SQLITE_LIMIT_ATTACHED                  7
#define SQLITE_LIMIT_LIKE_PATTERN_LENGTH       8
#define SQLITE_LIMIT_VARIABLE_NUMBER           9
#define SQLITE_LIMIT_TRIGGER_DEPTH            10


/*
** The number of different kinds of things that can be limited
** using the sqlite3_limit() interface.
*/
#define SQLITE_N_LIMIT (SQLITE_LIMIT_TRIGGER_DEPTH+1)



/*
** The following are the meanings of bits in the Expr.flags field.
*/
#define EP_FromJoin   0x0001  /* Originated in ON or USING clause of a join */
#define EP_Agg        0x0002  /* Contains one or more aggregate functions */
#define EP_Resolved   0x0004  /* IDs have been resolved to COLUMNs */
#define EP_Error      0x0008  /* Expression contains one or more errors */
#define EP_Distinct   0x0010  /* Aggregate function with DISTINCT keyword */
#define EP_VarSelect  0x0020  /* pSelect is correlated, not constant */
#define EP_DblQuoted  0x0040  /* token.z was originally in "..." */
#define EP_InfixFunc  0x0080  /* True for an infix function: LIKE, GLOB, etc */
#define EP_ExpCollate 0x0100  /* Collating sequence specified explicitly */
#define EP_FixedDest  0x0200  /* Result needed in a specific register */
#define EP_IntValue   0x0400  /* Integer value contained in u.iValue */
#define EP_xIsSelect  0x0800  /* x.pSelect is valid (otherwise x.pList is) */
#define EP_Hint       0x1000  /* Optimizer hint. Not required for correctness */
#define EP_Reduced    0x2000  /* Expr struct is EXPR_REDUCEDSIZE bytes only */
#define EP_TokenOnly  0x4000  /* Expr struct is EXPR_TOKENONLYSIZE bytes only */
#define EP_Static     0x8000  /* Held in memory not obtained from malloc() */

/*
** The following are the meanings of bits in the Expr.flags2 field.
*/
#define EP2_MallocedToken  0x0001  /* Need to sqlite3DbFree() Expr.zToken */
#define EP2_Irreducible    0x0002  /* Cannot EXPRDUP_REDUCE this Expr */


/*
** A sort order can be either ASC or DESC.
*/
#define SQLITE_SO_ASC       0  /* Sort in ascending order */
#define SQLITE_SO_DESC      1  /* Sort in ascending order */



/*
** Permitted values of the SrcList.a.jointype field
*/
#define JT_INNER     0x0001    /* Any kind of inner or cross join */
#define JT_CROSS     0x0002    /* Explicit use of the CROSS keyword */
#define JT_NATURAL   0x0004    /* True for a "natural" join */
#define JT_LEFT      0x0008    /* Left outer join */
#define JT_RIGHT     0x0010    /* Right outer join */
#define JT_OUTER     0x0020    /* The "OUTER" keyword is present */
#define JT_ERROR     0x0040    /* unknown or unsupported join type */


/*
** The results of a select can be distributed in several ways.  The
** "SRT" prefix means "SELECT Result Type".
*/
#define SRT_Union        1  /* Store result as keys in an index */
#define SRT_Except       2  /* Remove result from a UNION index */
#define SRT_Exists       3  /* Store 1 if the result is not empty */
#define SRT_Discard      4  /* Do not save the results anywhere */

/* The ORDER BY clause is ignored for all of the above */
#define IgnorableOrderby(X) ((X->eDest)<=SRT_Discard)

#define SRT_Output       5  /* Output each row of result */
#define SRT_Mem          6  /* Store result in a memory cell */
#define SRT_Set          7  /* Store results as keys in an index */
#define SRT_Table        8  /* Store result as data with an automatic rowid */
#define SRT_EphemTab     9  /* Create transient tab and store like SRT_Table */
#define SRT_Coroutine   10  /* Generate a single row of result */


/*
** The following are used as the second parameter to sqlite3Savepoint(),
** and as the P1 argument to the OP_Savepoint instruction.
*/
#define SAVEPOINT_BEGIN      0
#define SAVEPOINT_RELEASE    1
#define SAVEPOINT_ROLLBACK   2



/*
** Integers of known sizes.  These typedefs might change for architectures
** where the sizes very.  Preprocessor macros are available so that the
** types can be conveniently redefined at compile-type.  Like this:
**
**         cc '-DUINTPTR_TYPE=long long int' ...
*/
#ifndef UINT32_TYPE
# ifdef HAVE_UINT32_T
#  define UINT32_TYPE uint32_t
# else
#  define UINT32_TYPE unsigned int
# endif
#endif
#ifndef UINT16_TYPE
# ifdef HAVE_UINT16_T
#  define UINT16_TYPE uint16_t
# else
#  define UINT16_TYPE unsigned short int
# endif
#endif
#ifndef INT16_TYPE
# ifdef HAVE_INT16_T
#  define INT16_TYPE int16_t
# else
#  define INT16_TYPE short int
# endif
#endif
#ifndef UINT8_TYPE
# ifdef HAVE_UINT8_T
#  define UINT8_TYPE uint8_t
# else
#  define UINT8_TYPE unsigned char
# endif
#endif
#ifndef INT8_TYPE
# ifdef HAVE_INT8_T
#  define INT8_TYPE int8_t
# else
#  define INT8_TYPE signed char
# endif
#endif
#ifndef LONGDOUBLE_TYPE
# define LONGDOUBLE_TYPE long double
#endif
typedef sqlite_int64 i64;          /* 8-byte signed integer */
typedef sqlite_uint64 u64;         /* 8-byte unsigned integer */
typedef UINT32_TYPE u32;           /* 4-byte unsigned integer */
typedef UINT16_TYPE u16;           /* 2-byte unsigned integer */
typedef INT16_TYPE i16;            /* 2-byte signed integer */
typedef UINT8_TYPE u8;             /* 1-byte unsigned integer */
typedef INT8_TYPE i8;              /* 1-byte signed integer */

/*
** SQLITE_MAX_U32 is a u64 constant that is the maximum u64 value
** that can be stored in a u32 without loss of data.  The value
** is 0x00000000ffffffff.  But because of quirks of some compilers, we
** have to specify the value in the less intuitive manner shown:
*/
#define SQLITE_MAX_U32  ((((u64)1)<<32)-1)


/*
** The datatype ynVar is a signed integer, either 16-bit or 32-bit.
** Usually it is 16-bits.  But if SQLITE_MAX_VARIABLE_NUMBER is greater
** than 32767 we have to make it 32-bit.  16-bit is preferred because
** it uses less memory in the Expr object, which is a big memory user
** in systems with lots of prepared statements.  And few applications
** need more than about 10 or 20 variables.  But some extreme users want
** to have prepared statements with over 32767 variables, and for them
** the option is available (at compile-time).
*/
#if SQLITE_MAX_VARIABLE_NUMBER<=32767
typedef i16 ynVar;
#else
typedef int ynVar;
#endif


/*
** The bitmask datatype defined below is used for various optimizations.
**
** Changing this from a 64-bit to a 32-bit type limits the number of
** tables in a join to 32 instead of 64.  But it also reduces the size
** of the library by 738 bytes on ix86.
*/
typedef u64 Bitmask;

/*
** The number of bits in a Bitmask.  "BMS" means "BitMask Size".
*/
#define BMS  ((int)(sizeof(Bitmask)*8))


/*
** The yDbMask datatype for the bitmask of all attached databases.
*/
#if SQLITE_MAX_ATTACHED>30
  typedef sqlite3_uint64 yDbMask;
#else
  typedef unsigned int yDbMask;
#endif


/*
** SQLite supports many different ways to resolve a constraint
** error.  ROLLBACK processing means that a constraint violation
** causes the operation in process to fail and for the current transaction
** to be rolled back.  ABORT processing means the operation in process
** fails and any prior changes from that one operation are backed out,
** but the transaction is not rolled back.  FAIL processing means that
** the operation in progress stops and returns an error code.  But prior
** changes due to the same operation are not backed out and no rollback
** occurs.  IGNORE means that the particular row that caused the constraint
** error is not inserted or updated.  Processing continues and no error
** is returned.  REPLACE means that preexisting database rows that caused
** a UNIQUE constraint violation are removed so that the new insert or
** update can proceed.  Processing continues and no error is reported.
**
** RESTRICT, SETNULL, and CASCADE actions apply only to foreign keys.
** RESTRICT is the same as ABORT for IMMEDIATE foreign keys and the
** same as ROLLBACK for DEFERRED keys.  SETNULL means that the foreign
** key is set to NULL.  CASCADE means that a DELETE or UPDATE of the
** referenced table row is propagated into the row that holds the
** foreign key.
** 
** The following symbolic values are used to record which type
** of action to take.
*/
#define OE_None     0   /* There is no constraint to check */
#define OE_Rollback 1   /* Fail the operation and rollback the transaction */
#define OE_Abort    2   /* Back out changes but do no rollback transaction */
#define OE_Fail     3   /* Stop the operation but leave all prior changes */
#define OE_Ignore   4   /* Ignore the error. Do not do the INSERT or UPDATE */
#define OE_Replace  5   /* Delete existing record, then do INSERT or UPDATE */

#define OE_Restrict 6   /* OE_Abort for IMMEDIATE, OE_Rollback for DEFERRED */
#define OE_SetNull  7   /* Set the foreign key value to NULL */
#define OE_SetDflt  8   /* Set the foreign key value to its default */
#define OE_Cascade  9   /* Cascade the changes */

#define OE_Default  99  /* Do whatever the default action is */





/*
** Each token coming out of the lexer is an instance of
** this structure.  Tokens are also used as part of an expression.
**
** Note if Token.z==0 then Token.dyn and Token.n are undefined and
** may contain random values.  Do not make any assumptions about Token.dyn
** and Token.n when Token.z==0.
*/
struct Token {
  const char *z;     /* Text of the token.  Not NULL-terminated! */
  unsigned int n;    /* Number of characters in this token */
};


/*
** Each node of an expression in the parse tree is an instance
** of this structure.
**
** Expr.op is the opcode. The integer parser token codes are reused
** as opcodes here. For example, the parser defines TK_GE to be an integer
** code representing the ">=" operator. This same integer code is reused
** to represent the greater-than-or-equal-to operator in the expression
** tree.
**
** If the expression is an SQL literal (TK_INTEGER, TK_FLOAT, TK_BLOB, 
** or TK_STRING), then Expr.token contains the text of the SQL literal. If
** the expression is a variable (TK_VARIABLE), then Expr.token contains the 
** variable name. Finally, if the expression is an SQL function (TK_FUNCTION),
** then Expr.token contains the name of the function.
**
** Expr.pRight and Expr.pLeft are the left and right subexpressions of a
** binary operator. Either or both may be NULL.
**
** Expr.x.pList is a list of arguments if the expression is an SQL function,
** a CASE expression or an IN expression of the form "<lhs> IN (<y>, <z>...)".
** Expr.x.pSelect is used if the expression is a sub-select or an expression of
** the form "<lhs> IN (SELECT ...)". If the EP_xIsSelect bit is set in the
** Expr.flags mask, then Expr.x.pSelect is valid. Otherwise, Expr.x.pList is 
** valid.
**
** An expression of the form ID or ID.ID refers to a column in a table.
** For such expressions, Expr.op is set to TK_COLUMN and Expr.iTable is
** the integer cursor number of a VDBE cursor pointing to that table and
** Expr.iColumn is the column number for the specific column.  If the
** expression is used as a result in an aggregate SELECT, then the
** value is also stored in the Expr.iAgg column in the aggregate so that
** it can be accessed after all aggregates are computed.
**
** If the expression is an unbound variable marker (a question mark 
** character '?' in the original SQL) then the Expr.iTable holds the index 
** number for that variable.
**
** If the expression is a subquery then Expr.iColumn holds an integer
** register number containing the result of the subquery.  If the
** subquery gives a constant result, then iTable is -1.  If the subquery
** gives a different answer at different times during statement processing
** then iTable is the address of a subroutine that computes the subquery.
**
** If the Expr is of type OP_Column, and the table it is selecting from
** is a disk table or the "old.*" pseudo-table, then pTab points to the
** corresponding table definition.
**
** ALLOCATION NOTES:
**
** Expr objects can use a lot of memory space in database schema.  To
** help reduce memory requirements, sometimes an Expr object will be
** truncated.  And to reduce the number of memory allocations, sometimes
** two or more Expr objects will be stored in a single memory allocation,
** together with Expr.zToken strings.
**
** If the EP_Reduced and EP_TokenOnly flags are set when
** an Expr object is truncated.  When EP_Reduced is set, then all
** the child Expr objects in the Expr.pLeft and Expr.pRight subtrees
** are contained within the same memory allocation.  Note, however, that
** the subtrees in Expr.x.pList or Expr.x.pSelect are always separately
** allocated, regardless of whether or not EP_Reduced is set.
*/
struct Expr {
  u8 op;                 /* Operation performed by this node */
  char affinity;         /* The affinity of the column or 0 if not a column */
  u16 flags;             /* Various flags.  EP_* See below */
  union {
    char *zToken;          /* Token value. Zero terminated and dequoted */
    int iValue;            /* Non-negative integer value if EP_IntValue */
  } u;

  /* If the EP_TokenOnly flag is set in the Expr.flags mask, then no
  ** space is allocated for the fields below this point. An attempt to
  ** access them will result in a segfault or malfunction. 
  *********************************************************************/

  Expr *pLeft;           /* Left subnode */
  Expr *pRight;          /* Right subnode */
  union {
    ExprList *pList;     /* Function arguments or in "<expr> IN (<expr-list)" */
    Select *pSelect;     /* Used for sub-selects and "<expr> IN (<select>)" */
  } x;
  CollSeq *pColl;        /* The collation type of the column or 0 */

  /* If the EP_Reduced flag is set in the Expr.flags mask, then no
  ** space is allocated for the fields below this point. An attempt to
  ** access them will result in a segfault or malfunction.
  *********************************************************************/

  int iTable;            /* TK_COLUMN: cursor number of table holding column
                         ** TK_REGISTER: register number
                         ** TK_TRIGGER: 1 -> new, 0 -> old */
  ynVar iColumn;         /* TK_COLUMN: column index.  -1 for rowid.
                         ** TK_VARIABLE: variable number (always >= 1). */
  i16 iAgg;              /* Which entry in pAggInfo->aCol[] or ->aFunc[] */
  i16 iRightJoinTable;   /* If EP_FromJoin, the right table of the join */
  u8 flags2;             /* Second set of flags.  EP2_... */
  u8 op2;                /* If a TK_REGISTER, the original value of Expr.op */
  AggInfo *pAggInfo;     /* Used by TK_AGG_COLUMN and TK_AGG_FUNCTION */
  Table *pTab;           /* Table for TK_COLUMN expressions. */
#if SQLITE_MAX_EXPR_DEPTH>0
  int nHeight;           /* Height of the tree headed by this node */
#endif
};


/*
** A list of expressions.  Each expression may optionally have a
** name.  An expr/name combination can be used in several ways, such
** as the list of "expr AS ID" fields following a "SELECT" or in the
** list of "ID = expr" items in an UPDATE.  A list of expressions can
** also be used as the argument to a function, in which case the a.zName
** field is not used.
*/
struct ExprList {
  int nExpr;             /* Number of expressions on the list */
  int nAlloc;            /* Number of entries allocated below */
  int iECursor;          /* VDBE Cursor associated with this ExprList */
  struct ExprList_item {
    Expr *pExpr;           /* The list of expressions */
    char *zName;           /* Token associated with this expression */
    char *zSpan;           /* Original text of the expression */
    u8 sortOrder;          /* 1 for DESC or 0 for ASC */
    u8 done;               /* A flag to indicate when processing is finished */
    u16 iOrderByCol;       /* For ORDER BY, column number in result set */
    u16 iAlias;            /* Index into Parse.aAlias[] for zName */
  } *a;                  /* One entry for each expression */
};

/*
** An instance of this structure is used by the parser to record both
** the parse tree for an expression and the span of input text for an
** expression.
*/
struct ExprSpan {
  Expr *pExpr;          /* The expression parse tree */
  const char *zStart;   /* First character of input text */
  const char *zEnd;     /* One character past the end of input text */
};



/*
** These macros can be used to test, set, or clear bits in the 
** Expr.flags field.
*/
#define ExprHasProperty(E,P)     (((E)->flags&(P))==(P))
#define ExprHasAnyProperty(E,P)  (((E)->flags&(P))!=0)
#define ExprSetProperty(E,P)     (E)->flags|=(P)
#define ExprClearProperty(E,P)   (E)->flags&=~(P)


/*
** An instance of this structure can hold a simple list of identifiers,
** such as the list "a,b,c" in the following statements:
**
**      INSERT INTO t(a,b,c) VALUES ...;
**      CREATE INDEX idx ON t(a,b,c);
**      CREATE TRIGGER trig BEFORE UPDATE ON t(a,b,c) ...;
**
** The IdList.a.idx field is used when the IdList represents the list of
** column names after a table name in an INSERT statement.  In the statement
**
**     INSERT INTO t(a,b,c) ...
**
** If "a" is the k-th column of table "t", then IdList.a[0].idx==k.
*/
struct IdList {
  struct IdList_item {
    char *zName;      /* Name of the identifier */
    int idx;          /* Index in some Table.aCol[] of a column named zName */
  } *a;
  int nId;         /* Number of identifiers on the list */
  int nAlloc;      /* Number of entries allocated for a[] below */
};


/*
** The following structure describes the FROM clause of a SELECT statement.
** Each table or subquery in the FROM clause is a separate element of
** the SrcList.a[] array.
**
** With the addition of multiple database support, the following structure
** can also be used to describe a particular table such as the table that
** is modified by an INSERT, DELETE, or UPDATE statement.  In standard SQL,
** such a table must be a simple name: ID.  But in SQLite, the table can
** now be identified by a database name, a dot, then the table name: ID.ID.
**
** The jointype starts out showing the join type between the current table
** and the next table on the list.  The parser builds the list this way.
** But sqlite3SrcListShiftJoinType() later shifts the jointypes so that each
** jointype expresses the join between the table and the previous table.
**
** In the colUsed field, the high-order bit (bit 63) is set if the table
** contains more than 63 columns and the 64-th or later column is used.
*/
struct SrcList {
  i16 nSrc;        /* Number of tables or subqueries in the FROM clause */
  i16 nAlloc;      /* Number of entries allocated in a[] below */
  struct SrcList_item {
    char *zDatabase;  /* Name of database holding this table */
    char *zName;      /* Name of the table */
    char *zAlias;     /* The "B" part of a "A AS B" phrase.  zName is the "A" */
    Table *pTab;      /* An SQL table corresponding to zName */
    Select *pSelect;  /* A SELECT statement used in place of a table name */
    int addrFillSub;  /* Address of subroutine to manifest a subquery */
    int regReturn;    /* Register holding return address of addrFillSub */
    u8 jointype;      /* Type of join between this able and the previous */
    u8 notIndexed;    /* True if there is a NOT INDEXED clause */
    u8 isCorrelated;  /* True if sub-query is correlated */
#ifndef SQLITE_OMIT_EXPLAIN
    u8 iSelectId;     /* If pSelect!=0, the id of the sub-select in EQP */
#endif
    int iCursor;      /* The VDBE cursor number used to access this table */
    Expr *pOn;        /* The ON clause of a join */
    IdList *pUsing;   /* The USING clause of a join */
    Bitmask colUsed;  /* Bit N (1<<N) set if column N of pTab is used */
    char *zIndex;     /* Identifier from "INDEXED BY <zIndex>" clause */
    Index *pIndex;    /* Index structure corresponding to zIndex, if any */
  } a[1];             /* One entry for each identifier on the list */
};


/*
** An instance of the following structure contains all information
** needed to generate code for a single SELECT statement.
**
** nLimit is set to -1 if there is no LIMIT clause.  nOffset is set to 0.
** If there is a LIMIT clause, the parser sets nLimit to the value of the
** limit and nOffset to the value of the offset (or 0 if there is not
** offset).  But later on, nLimit and nOffset become the memory locations
** in the VDBE that record the limit and offset counters.
**
** addrOpenEphm[] entries contain the address of OP_OpenEphemeral opcodes.
** These addresses must be stored so that we can go back and fill in
** the P4_KEYINFO and P2 parameters later.  Neither the KeyInfo nor
** the number of columns in P2 can be computed at the same time
** as the OP_OpenEphm instruction is coded because not
** enough information about the compound query is known at that point.
** The KeyInfo for addrOpenTran[0] and [1] contains collating sequences
** for the result set.  The KeyInfo for addrOpenTran[2] contains collating
** sequences for the ORDER BY clause.
*/
struct Select {
  ExprList *pEList;      /* The fields of the result */
  u8 op;                 /* One of: TK_UNION TK_ALL TK_INTERSECT TK_EXCEPT */
  char affinity;         /* MakeRecord with this affinity for SRT_Set */
  u16 selFlags;          /* Various SF_* values */
  SrcList *pSrc;         /* The FROM clause */
  Expr *pWhere;          /* The WHERE clause */
  ExprList *pGroupBy;    /* The GROUP BY clause */
  Expr *pHaving;         /* The HAVING clause */
  ExprList *pOrderBy;    /* The ORDER BY clause */
  Select *pPrior;        /* Prior select in a compound select statement */
  Select *pNext;         /* Next select to the left in a compound */
  Select *pRightmost;    /* Right-most select in a compound select statement */
  Expr *pLimit;          /* LIMIT expression. NULL means not used. */
  Expr *pOffset;         /* OFFSET expression. NULL means not used. */
  int iLimit, iOffset;   /* Memory registers holding LIMIT & OFFSET counters */
  int addrOpenEphm[3];   /* OP_OpenEphem opcodes related to this select */
  double nSelectRow;     /* Estimated number of result rows */
};


/*
** A structure used to customize the behavior of sqlite3Select(). See
** comments above sqlite3Select() for details.
*/
typedef struct SelectDest SelectDest;
struct SelectDest {
  u8 eDest;         /* How to dispose of the results */
  u8 affinity;      /* Affinity used when eDest==SRT_Set */
  int iParm;        /* A parameter used by the eDest disposal method */
  int iMem;         /* Base register where results are written */
  int nMem;         /* Number of registers allocated */
};



/*
 * Each trigger present in the database schema is stored as an instance of
 * struct Trigger. 
 *
 * Pointers to instances of struct Trigger are stored in two ways.
 * 1. In the "trigHash" hash table (part of the sqlite3* that represents the 
 *    database). This allows Trigger structures to be retrieved by name.
 * 2. All triggers associated with a single table form a linked list, using the
 *    pNext member of struct Trigger. A pointer to the first element of the
 *    linked list is stored as the "pTrigger" member of the associated
 *    struct Table.
 *
 * The "step_list" member points to the first element of a linked list
 * containing the SQL statements specified as the trigger program.
 */
struct Trigger {
  char *zName;            /* The name of the trigger                        */
  char *table;            /* The table or view to which the trigger applies */
  u8 op;                  /* One of TK_DELETE, TK_UPDATE, TK_INSERT         */
  u8 tr_tm;               /* One of TRIGGER_BEFORE, TRIGGER_AFTER */
  Expr *pWhen;            /* The WHEN clause of the expression (may be NULL) */
  IdList *pColumns;       /* If this is an UPDATE OF <column-list> trigger,
                             the <column-list> is stored here */
  Schema *pSchema;        /* Schema containing the trigger */
  Schema *pTabSchema;     /* Schema containing the table */
  TriggerStep *step_list; /* Link list of trigger program steps             */
  Trigger *pNext;         /* Next trigger associated with the table */
};

/*
** A trigger is either a BEFORE or an AFTER trigger.  The following constants
** determine which. 
**
** If there are multiple triggers, you might of some BEFORE and some AFTER.
** In that cases, the constants below can be ORed together.
*/
#define TRIGGER_BEFORE  1
#define TRIGGER_AFTER   2

/*
 * An instance of struct TriggerStep is used to store a single SQL statement
 * that is a part of a trigger-program. 
 *
 * Instances of struct TriggerStep are stored in a singly linked list (linked
 * using the "pNext" member) referenced by the "step_list" member of the 
 * associated struct Trigger instance. The first element of the linked list is
 * the first step of the trigger-program.
 * 
 * The "op" member indicates whether this is a "DELETE", "INSERT", "UPDATE" or
 * "SELECT" statement. The meanings of the other members is determined by the 
 * value of "op" as follows:
 *
 * (op == TK_INSERT)
 * orconf    -> stores the ON CONFLICT algorithm
 * pSelect   -> If this is an INSERT INTO ... SELECT ... statement, then
 *              this stores a pointer to the SELECT statement. Otherwise NULL.
 * target    -> A token holding the quoted name of the table to insert into.
 * pExprList -> If this is an INSERT INTO ... VALUES ... statement, then
 *              this stores values to be inserted. Otherwise NULL.
 * pIdList   -> If this is an INSERT INTO ... (<column-names>) VALUES ... 
 *              statement, then this stores the column-names to be
 *              inserted into.
 *
 * (op == TK_DELETE)
 * target    -> A token holding the quoted name of the table to delete from.
 * pWhere    -> The WHERE clause of the DELETE statement if one is specified.
 *              Otherwise NULL.
 * 
 * (op == TK_UPDATE)
 * target    -> A token holding the quoted name of the table to update rows of.
 * pWhere    -> The WHERE clause of the UPDATE statement if one is specified.
 *              Otherwise NULL.
 * pExprList -> A list of the columns to update and the expressions to update
 *              them to. See sqlite3Update() documentation of "pChanges"
 *              argument.
 * 
 */
struct TriggerStep {
  u8 op;               /* One of TK_DELETE, TK_UPDATE, TK_INSERT, TK_SELECT */
  u8 orconf;           /* OE_Rollback etc. */
  Trigger *pTrig;      /* The trigger that this step is a part of */
  Select *pSelect;     /* SELECT statment or RHS of INSERT INTO .. SELECT ... */
  Token target;        /* Target table for DELETE, UPDATE, INSERT */
  Expr *pWhere;        /* The WHERE clause for DELETE or UPDATE steps */
  ExprList *pExprList; /* SET clause for UPDATE.  VALUES clause for INSERT */
  IdList *pIdList;     /* Column names for INSERT */
  TriggerStep *pNext;  /* Next in the link-list */
  TriggerStep *pLast;  /* Last element in link-list. Valid for 1st elem only */
};


/*
** An SQL parser context.  A copy of this structure is passed through
** the parser and down into all the parser action routine in order to
** carry around information that is global to the entire parse.
**
** The structure is divided into two parts.  When the parser and code
** generate call themselves recursively, the first part of the structure
** is constant but the second part is reset at the beginning and end of
** each recursion.
**
** The nTableLock and aTableLock variables are only used if the shared-cache 
** feature is enabled (if sqlite3Tsd()->useSharedData is true). They are
** used to store the set of table-locks required by the statement being
** compiled. Function sqlite3TableLock() is used to add entries to the
** list.
*/
struct Parse {
  sqlite3 *db;         /* The main database structure */
  int rc;              /* Return code from execution */
  char *zErrMsg;       /* An error message */
  Vdbe *pVdbe;         /* An engine for executing database bytecode */
  u8 colNamesSet;      /* TRUE after OP_ColumnName has been issued to pVdbe */
  u8 checkSchema;      /* Causes schema cookie check after an error */
  u8 nested;           /* Number of nested calls to the parser/code generator */
  u8 nTempReg;         /* Number of temporary registers in aTempReg[] */
  u8 nTempInUse;       /* Number of aTempReg[] currently checked out */
  int aTempReg[8];     /* Holding area for temporary registers */
  int nRangeReg;       /* Size of the temporary register block */
  int iRangeReg;       /* First register in temporary register block */
  int nErr;            /* Number of errors seen */
  int nTab;            /* Number of previously allocated VDBE cursors */
  int nMem;            /* Number of memory cells used so far */
  int nSet;            /* Number of sets used so far */
  int nOnce;           /* Number of OP_Once instructions so far */
  int ckBase;          /* Base register of data during check constraints */
  int iCacheLevel;     /* ColCache valid when aColCache[].iLevel<=iCacheLevel */
  int iCacheCnt;       /* Counter used to generate aColCache[].lru values */
  u8 nColCache;        /* Number of entries in aColCache[] */
  u8 iColCache;        /* Next entry in aColCache[] to replace */
  struct yColCache {
    int iTable;           /* Table cursor number */
    int iColumn;          /* Table column number */
    u8 tempReg;           /* iReg is a temp register that needs to be freed */
    int iLevel;           /* Nesting level */
    int iReg;             /* Reg with value of this column. 0 means none. */
    int lru;              /* Least recently used entry has the smallest value */
  } aColCache[SQLITE_N_COLCACHE];  /* One for each column cache entry */
  yDbMask writeMask;   /* Start a write transaction on these databases */
  yDbMask cookieMask;  /* Bitmask of schema verified databases */
  u8 isMultiWrite;     /* True if statement may affect/insert multiple rows */
  u8 mayAbort;         /* True if statement may throw an ABORT exception */
  int cookieGoto;      /* Address of OP_Goto to cookie verifier subroutine */
  int cookieValue[SQLITE_MAX_ATTACHED+2];  /* Values of cookies to verify */
#ifndef SQLITE_OMIT_SHARED_CACHE
  int nTableLock;        /* Number of locks in aTableLock */
  TableLock *aTableLock; /* Required table locks for shared-cache mode */
#endif
  int regRowid;        /* Register holding rowid of CREATE TABLE entry */
  int regRoot;         /* Register holding root page number for new objects */
  AutoincInfo *pAinc;  /* Information about AUTOINCREMENT counters */
  int nMaxArg;         /* Max args passed to user function by sub-program */

  /* Information used while coding trigger programs. */
  Parse *pToplevel;    /* Parse structure for main program (or NULL) */
  Table *pTriggerTab;  /* Table triggers are being coded for */
  u32 oldmask;         /* Mask of old.* columns referenced */
  u32 newmask;         /* Mask of new.* columns referenced */
  u8 eTriggerOp;       /* TK_UPDATE, TK_INSERT or TK_DELETE */
  u8 eOrconf;          /* Default ON CONFLICT policy for trigger steps */
  u8 disableTriggers;  /* True to disable triggers */
  double nQueryLoop;   /* Estimated number of iterations of a query */

  /* Above is constant between recursions.  Below is reset before and after
  ** each recursion */

  int nVar;            /* Number of '?' variables seen in the SQL so far */
  int nzVar;           /* Number of available slots in azVar[] */
  char **azVar;        /* Pointers to names of parameters */
  Vdbe *pReprepare;    /* VM being reprepared (sqlite3Reprepare()) */
  int nAlias;          /* Number of aliased result set columns */
  int *aAlias;         /* Register used to hold aliased result */
  u8 explain;          /* True if the EXPLAIN flag is found on the query */
  Token sNameToken;    /* Token with unqualified schema object name */
  Token sLastToken;    /* The last token parsed */
  const char *zTail;   /* All SQL text past the last semicolon parsed */
  Table *pNewTable;    /* A table being constructed by CREATE TABLE */
  Trigger *pNewTrigger;     /* Trigger under construct by a CREATE TRIGGER */
  const char *zAuthContext; /* The 6th parameter to db->xAuth callbacks */
#ifndef SQLITE_OMIT_VIRTUALTABLE
  Token sArg;                /* Complete text of a module argument */
  u8 declareVtab;            /* True if inside sqlite3_declare_vtab() */
  int nVtabLock;             /* Number of virtual tables to lock */
  Table **apVtabLock;        /* Pointer to virtual tables needing locking */
#endif
  int nHeight;            /* Expression tree height of current sub-select */
  Table *pZombieTab;      /* List of Table objects to delete after code gen */
  TriggerPrg *pTriggerPrg;    /* Linked list of coded triggers */

#ifndef SQLITE_OMIT_EXPLAIN
  int iSelectId;
  int iNextSelectId;
#endif
};


/*
** Lookaside malloc is a set of fixed-size buffers that can be used
** to satisfy small transient memory allocation requests for objects
** associated with a particular database connection.  The use of
** lookaside malloc provides a significant performance enhancement
** (approx 10%) by avoiding numerous malloc/free requests while parsing
** SQL statements.
**
** The Lookaside structure holds configuration information about the
** lookaside malloc subsystem.  Each available memory allocation in
** the lookaside subsystem is stored on a linked list of LookasideSlot
** objects.
**
** Lookaside allocations are only allowed for objects that are associated
** with a particular database connection.  Hence, schema information cannot
** be stored in lookaside because in shared cache mode the schema information
** is shared by multiple database connections.  Therefore, while parsing
** schema information, the Lookaside.bEnabled flag is cleared so that
** lookaside allocations are not used to construct the schema objects.
*/
struct Lookaside {
  u16 sz;                 /* Size of each buffer in bytes */
  u8 bEnabled;            /* False to disable new lookaside allocations */
  u8 bMalloced;           /* True if pStart obtained from sqlite3_malloc() */
  int nOut;               /* Number of buffers currently checked out */
  int mxOut;              /* Highwater mark for nOut */
  int anStat[3];          /* 0: hits.  1: size misses.  2: full misses */
  LookasideSlot *pFree;   /* List of available buffers */
  void *pStart;           /* First byte of available memory space */
  void *pEnd;             /* First byte past end of available space */
};
struct LookasideSlot {
  LookasideSlot *pNext;    /* Next buffer in the list of free buffers */
};



/* Forward declarations of structures. */
typedef struct Hash Hash;
typedef struct HashElem HashElem;

/* A complete hash table is an instance of the following structure.
** The internals of this structure are intended to be opaque -- client
** code should not attempt to access or modify the fields of this structure
** directly.  Change this structure only by using the routines below.
** However, some of the "procedures" and "functions" for modifying and
** accessing this structure are really macros, so we can't really make
** this structure opaque.
**
** All elements of the hash table are on a single doubly-linked list.
** Hash.first points to the head of this list.
**
** There are Hash.htsize buckets.  Each bucket points to a spot in
** the global doubly-linked list.  The contents of the bucket are the
** element pointed to plus the next _ht.count-1 elements in the list.
**
** Hash.htsize and Hash.ht may be zero.  In that case lookup is done
** by a linear search of the global list.  For small tables, the 
** Hash.ht table is never allocated because if there are few elements
** in the table, it is faster to do a linear search than to manage
** the hash table.
*/
struct Hash {
  unsigned int htsize;      /* Number of buckets in the hash table */
  unsigned int count;       /* Number of entries in this table */
  HashElem *first;          /* The first element of the array */
  struct _ht {              /* the hash table */
    int count;                 /* Number of entries with this hash */
    HashElem *chain;           /* Pointer to first entry with this hash */
  } *ht;
};

/* Each element in the hash table is an instance of the following 
** structure.  All elements are stored on a single doubly-linked list.
**
** Again, this structure is intended to be opaque, but it can't really
** be opaque because it is used by macros.
*/
struct HashElem {
  HashElem *next, *prev;       /* Next and previous elements in the table */
  void *data;                  /* Data associated with this element */
  const char *pKey; int nKey;  /* Key associated with this element */
};


/*
** This structure encapsulates a user-function destructor callback (as
** configured using create_function_v2()) and a reference counter. When
** create_function_v2() is called to create a function with a destructor,
** a single object of this type is allocated. FuncDestructor.nRef is set to 
** the number of FuncDef objects created (either 1 or 3, depending on whether
** or not the specified encoding is SQLITE_ANY). The FuncDef.pDestructor
** member of each of the new FuncDef objects is set to point to the allocated
** FuncDestructor.
**
** Thereafter, when one of the FuncDef objects is deleted, the reference
** count on this object is decremented. When it reaches 0, the destructor
** is invoked and the FuncDestructor structure freed.
*/
struct FuncDestructor {
  int nRef;
  void (*xDestroy)(void *);
  void *pUserData;
};


/*
** Each SQL function is defined by an instance of the following
** structure.  A pointer to this structure is stored in the sqlite.aFunc
** hash table.  When multiple functions have the same name, the hash table
** points to a linked list of these structures.
*/
struct FuncDef {
  i16 nArg;            /* Number of arguments.  -1 means unlimited */
  u8 iPrefEnc;         /* Preferred text encoding (SQLITE_UTF8, 16LE, 16BE) */
  u8 flags;            /* Some combination of SQLITE_FUNC_* */
  void *pUserData;     /* User data parameter */
  FuncDef *pNext;      /* Next function with same name */
  void (*xFunc)(sqlite3_context*,int,sqlite3_value**); /* Regular function */
  void (*xStep)(sqlite3_context*,int,sqlite3_value**); /* Aggregate step */
  void (*xFinalize)(sqlite3_context*);                /* Aggregate finalizer */
  char *zName;         /* SQL name of the function. */
  FuncDef *pHash;      /* Next with a different name but the same hash */
  FuncDestructor *pDestructor;   /* Reference counted destructor function */
};

/*
** A hash table for function definitions.
**
** Hash each FuncDef structure into one of the FuncDefHash.a[] slots.
** Collisions are on the FuncDef.pHash chain.
*/
struct FuncDefHash {
  FuncDef *a[23];       /* Hash table for functions */
};




/*
** An instance of the following structure is used to store the busy-handler
** callback for a given sqlite handle. 
**
** The sqlite.busyHandler member of the sqlite struct contains the busy
** callback for the database handle. Each pager opened via the sqlite
** handle is passed a pointer to sqlite.busyHandler. The busy-handler
** callback is currently invoked only from within pager.c.
*/
typedef struct BusyHandler BusyHandler;
struct BusyHandler {
  int (*xFunc)(void *,int);  /* The busy callback */
  void *pArg;                /* First arg to busy callback */
  int nBusy;                 /* Incremented with each busy call */
};



/*
** Each database file to be accessed by the system is an instance
** of the following structure.  There are normally two of these structures
** in the sqlite.aDb[] array.  aDb[0] is the main database file and
** aDb[1] is the database file used to hold temporary tables.  Additional
** databases may be attached.
*/
struct Db {
  char *zName;         /* Name of this database */
  Btree *pBt;          /* The B*Tree structure for this database file */
  u8 inTrans;          /* 0: not writable.  1: Transaction.  2: Checkpoint */
  u8 safety_level;     /* How aggressive at syncing data to disk */
  Schema *pSchema;     /* Pointer to database schema (possibly shared) */
};


/*
** Each database connection is an instance of the following structure.
**
** The sqlite.lastRowid records the last insert rowid generated by an
** insert statement.  Inserts on views do not affect its value.  Each
** trigger has its own context, so that lastRowid can be updated inside
** triggers as usual.  The previous value will be restored once the trigger
** exits.  Upon entering a before or instead of trigger, lastRowid is no
** longer (since after version 2.8.12) reset to -1.
**
** The sqlite.nChange does not count changes within triggers and keeps no
** context.  It is reset at start of sqlite3_exec.
** The sqlite.lsChange represents the number of changes made by the last
** insert, update, or delete statement.  It remains constant throughout the
** length of a statement and is then updated by OP_SetCounts.  It keeps a
** context stack just like lastRowid so that the count of changes
** within a trigger is not seen outside the trigger.  Changes to views do not
** affect the value of lsChange.
** The sqlite.csChange keeps track of the number of current changes (since
** the last statement) and is used to update sqlite_lsChange.
**
** The member variables sqlite.errCode, sqlite.zErrMsg and sqlite.zErrMsg16
** store the most recent error code and, if applicable, string. The
** internal function sqlite3Error() is used to set these variables
** consistently.
*/
struct sqlite3 {
  sqlite3_vfs *pVfs;            /* OS Interface */
  int nDb;                      /* Number of backends currently in use */
  Db *aDb;                      /* All backends */
  int flags;                    /* Miscellaneous flags. See below */
  unsigned int openFlags;       /* Flags passed to sqlite3_vfs.xOpen() */
  int errCode;                  /* Most recent error code (SQLITE_*) */
  int errMask;                  /* & result codes with this before returning */
  u8 autoCommit;                /* The auto-commit flag. */
  u8 temp_store;                /* 1: file 2: memory 0: default */
  u8 mallocFailed;              /* True if we have seen a malloc failure */
  u8 dfltLockMode;              /* Default locking-mode for attached dbs */
  signed char nextAutovac;      /* Autovac setting after VACUUM if >=0 */
  u8 suppressErr;               /* Do not issue error messages if true */
  u8 vtabOnConflict;            /* Value to return for s3_vtab_on_conflict() */
  int nextPagesize;             /* Pagesize after VACUUM if >0 */
  int nTable;                   /* Number of tables in the database */
  CollSeq *pDfltColl;           /* The default collating sequence (BINARY) */
  i64 lastRowid;                /* ROWID of most recent insert (see above) */
  u32 magic;                    /* Magic number for detect library misuse */
  int nChange;                  /* Value returned by sqlite3_changes() */
  int nTotalChange;             /* Value returned by sqlite3_total_changes() */
  sqlite3_mutex *mutex;         /* Connection mutex */
  int aLimit[SQLITE_N_LIMIT];   /* Limits */
  struct sqlite3InitInfo {      /* Information used during initialization */
    int iDb;                    /* When back is being initialized */
    int newTnum;                /* Rootpage of table being initialized */
    u8 busy;                    /* TRUE if currently initializing */
    u8 orphanTrigger;           /* Last statement is orphaned TEMP trigger */
  } init;
  int nExtension;               /* Number of loaded extensions */
  void **aExtension;            /* Array of shared library handles */
  struct Vdbe *pVdbe;           /* List of active virtual machines */
  int activeVdbeCnt;            /* Number of VDBEs currently executing */
  int writeVdbeCnt;             /* Number of active VDBEs that are writing */
  int vdbeExecCnt;              /* Number of nested calls to VdbeExec() */
  void (*xTrace)(void*,const char*);        /* Trace function */
  void *pTraceArg;                          /* Argument to the trace function */
  void (*xProfile)(void*,const char*,u64);  /* Profiling function */
  void *pProfileArg;                        /* Argument to profile function */
  void *pCommitArg;                 /* Argument to xCommitCallback() */   
  int (*xCommitCallback)(void*);    /* Invoked at every commit. */
  void *pRollbackArg;               /* Argument to xRollbackCallback() */   
  void (*xRollbackCallback)(void*); /* Invoked at every commit. */
  void *pUpdateArg;
  void (*xUpdateCallback)(void*,int, const char*,const char*,sqlite_int64);
#ifndef SQLITE_OMIT_WAL
  int (*xWalCallback)(void *, sqlite3 *, const char *, int);
  void *pWalArg;
#endif
  void(*xCollNeeded)(void*,sqlite3*,int eTextRep,const char*);
  void(*xCollNeeded16)(void*,sqlite3*,int eTextRep,const void*);
  void *pCollNeededArg;
  sqlite3_value *pErr;          /* Most recent error message */
  char *zErrMsg;                /* Most recent error message (UTF-8 encoded) */
  char *zErrMsg16;              /* Most recent error message (UTF-16 encoded) */
  union {
    volatile int isInterrupted; /* True if sqlite3_interrupt has been called */
    double notUsed1;            /* Spacer */
  } u1;
  Lookaside lookaside;          /* Lookaside malloc configuration */
#ifndef SQLITE_OMIT_AUTHORIZATION
  int (*xAuth)(void*,int,const char*,const char*,const char*,const char*);
                                /* Access authorization function */
  void *pAuthArg;               /* 1st argument to the access auth function */
#endif
#ifndef SQLITE_OMIT_PROGRESS_CALLBACK
  int (*xProgress)(void *);     /* The progress callback */
  void *pProgressArg;           /* Argument to the progress callback */
  int nProgressOps;             /* Number of opcodes for progress callback */
#endif
#ifndef SQLITE_OMIT_VIRTUALTABLE
  Hash aModule;                 /* populated by sqlite3_create_module() */
  VtabCtx *pVtabCtx;            /* Context for active vtab connect/create */
  VTable **aVTrans;             /* Virtual tables with open transactions */
  int nVTrans;                  /* Allocated size of aVTrans */
  VTable *pDisconnect;    /* Disconnect these in next sqlite3_prepare() */
#endif
  FuncDefHash aFunc;            /* Hash table of connection functions */
  Hash aCollSeq;                /* All collating sequences */
  BusyHandler busyHandler;      /* Busy callback */
  int busyTimeout;              /* Busy handler timeout, in msec */
  Db aDbStatic[2];              /* Static space for the 2 default backends */
  Savepoint *pSavepoint;        /* List of active savepoints */
  int nSavepoint;               /* Number of non-transaction savepoints */
  int nStatement;               /* Number of nested statement-transactions  */
  u8 isTransactionSavepoint;    /* True if the outermost savepoint is a TS */
  i64 nDeferredCons;            /* Net deferred constraints this transaction. */
  int *pnBytesFreed;            /* If not NULL, increment this in DbFree() */

#ifdef SQLITE_ENABLE_UNLOCK_NOTIFY
  /* The following variables are all protected by the STATIC_MASTER 
  ** mutex, not by sqlite3.mutex. They are used by code in notify.c. 
  **
  ** When X.pUnlockConnection==Y, that means that X is waiting for Y to
  ** unlock so that it can proceed.
  **
  ** When X.pBlockingConnection==Y, that means that something that X tried
  ** tried to do recently failed with an SQLITE_LOCKED error due to locks
  ** held by Y.
  */
  sqlite3 *pBlockingConnection; /* Connection that caused SQLITE_LOCKED */
  sqlite3 *pUnlockConnection;           /* Connection to watch for unlock */
  void *pUnlockArg;                     /* Argument to xUnlockNotify */
  void (*xUnlockNotify)(void **, int);  /* Unlock notify callback */
  sqlite3 *pNextBlocked;        /* Next in list of all blocked connections */
#endif
};





extern const Token sqlite3IntTokens[];

TriggerStep *sqlite3TriggerUpdateStep(sqlite3* db,Token* tk,ExprList* elist, Expr* e, u8 val);


TriggerStep *sqlite3TriggerDeleteStep(sqlite3* db,Token* tk, Expr* e);
TriggerStep *sqlite3TriggerSelectStep(sqlite3* db,Select* s);
Expr *sqlite3PExpr(Parse* p,int i1, Expr* e, Expr* e2, const Token* tk);

ExprList *sqlite3ExprListAppend(Parse* p,ExprList* elist,Expr* e);

IdList *sqlite3IdListAppend(sqlite3* db, IdList* idlist, Token* tk);





int sqlite3GetInt32(const char *str,int *ip);

Expr *sqlite3ExprSetCollByToken(Parse *pParse, Expr* e, Token* tk);

Expr *sqlite3ExprFunction(Parse* p,ExprList* elist, Token* tk);




SrcList *sqlite3SrcListAppend(sqlite3* db, SrcList* slist, Token* tk, Token* tk2);
int sqlite3JoinType(Parse* p, Token* tk, Token* tk2, Token* tk3);




Index *sqlite3CreateIndex(Parse* p,Token* tk,Token* tk2,SrcList* slist,ExprList* elist,int i1,Token* tk3,Token* tk4, int i2, int i3);

Select *sqlite3SelectNew(Parse* p,ExprList* elist,SrcList* slist,Expr* e,ExprList* elist2,Expr*e2,ExprList*elist3,int i1,Expr*e3,Expr*e4);

SrcList * sqlite3SrcListAppendFromTerm(Parse* p, SrcList* slist, Token* tk, Token* tk1,Token*tk2, Select*s, Expr*e, IdList*ilist);


TriggerStep *sqlite3TriggerInsertStep(sqlite3* db,Token* tk, IdList* idlist, ExprList*elist,Select*s,u8 val);

void sqlite3BeginTrigger(Parse*p, Token*tk1,Token*tk2,int i1,int i2,IdList* idlist,SrcList* slist, Expr*e,int i3, int i4);

void sqlite3Savepoint(Parse*p, int i1, Token* tk);
int sqlite3Select(Parse*p, Select*s, SelectDest*sd);
Expr *sqlite3Expr(sqlite3*db,int i1,const char* str);
void sqlite3SrcListShiftJoinType(SrcList* slist);
void sqlite3Update(Parse*p, SrcList*slist, ExprList*elist, Expr*e, int i1);




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


void *sqlite3DbMallocZero(sqlite3* db,int i1);



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

#endif // BORROWED_SQLITE_CODE_FOR_CALLBACKS
