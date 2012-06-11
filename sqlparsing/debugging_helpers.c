#include "debugging_helpers.h"

#include  <signal.h>      // for SIGTRAP
//#include <sys/ptrace.h> // i have not yet mastered detecting gdb with:   if (ptrace(PTRACE_TRACEME, 0, 0, 0) == -1)

#include <stdio.h>
#include <unistd.h>

// based off of code and info from: http://xorl.wordpress.com/2009/01/05/more-gdb-anti-debugging/ and http://silviocesare.wordpress.com/2008/05/13/gdb-leaves-file-descriptors-open-in-debugee/

// gdb apparently opens FD(s) 3,4,5 (whereas a typical prog uses only stdin=0, stdout=1,stderr=2)
int detect_gdb(void)
{
    int rc = 0;
    FILE *fd = fopen("/tmp", "r");

    if (fileno(fd) > 5)
    {
        rc = 1;
    }

    fclose(fd);
    return rc;
}


void RaiseBreakpointSignalOnlyWhenDebuggerExists()
{
    if( detect_gdb() )
    {
        raise(SIGTRAP);
    }
    else
    {
        // we cannot use SIGTRAP because no debugger is attached.
    }
}
