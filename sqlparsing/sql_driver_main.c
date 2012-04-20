
#include "tk_defs.h"
#include <assert.h>
#include "sql_parse.c"


int main()
{
    // sql_driver_main.c:7:19: warning: initialization makes pointer from integer without a cast [enabled by default]
  void* pParser = ParseAlloc (malloc);



  ParseFree(pParser, free );

}
