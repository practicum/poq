#ifndef PRACTICUM_MAIN_SQL_PARSE_RUN_PARSER
#define PRACTICUM_MAIN_SQL_PARSE_RUN_PARSER

typedef struct Parse Parse; // forward reference

/*
** Run the parser on the given SQL string.  The parser structure is passed in.
** An SQLITE_ status code is returned.  If an error occurs then an attempt is
** made to write an error message into memory obtained from sqlite3_malloc() and
** to make *pzErrMsg point to that error message.
*/
int sqlite3RunParser(Parse *pParse, const char *zSql, char **pzErrMsg);

#endif //#ifndef PRACTICUM_MAIN_SQL_PARSE_RUN_PARSER

