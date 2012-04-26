
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stddef.h>


#include "sqliteLimit.h"
#include "tk_defs.h"
#include <assert.h>
#include "sql_parse.c"

#include "my_callbacks.c"
#include "my_callbacks2.c"

/*
** Constant tokens for values 0 and 1.
*/
const Token sqlite3IntTokens[] = {
   { "0", 1 },
   { "1", 1 }
};


int main()
{
    // sql_driver_main.c:7:19: warning: initialization makes pointer from integer without a cast [enabled by default]
  void* pParser = sqlite3ParserAlloc (malloc);
  printf("did alloc\n");



  sqlite3ParserFree(pParser, free );
  printf("did free\n");
}
