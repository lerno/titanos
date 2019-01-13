#include "error.h"

#include <unistd.h>

int use_colors(void)
{
    static int use = -1;
    if (use == -1)
    {
        use = isatty(1) != 0;
    }
    return use != 0;
}
