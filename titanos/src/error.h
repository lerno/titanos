#pragma once

#include <stdio.h>
#include <stdlib.h>
#include "ansi_color.h"
#define FATAL_ERROR(_string, ...) do { printf("FATAL ERROR: " _string, ##__VA_ARGS__); printf("\n"); exit(-1); } while(0)

#define ASSERT(_condition, _string, ...) while (!(_condition)) { FATAL_ERROR(_string, ##__VA_ARGS__); }

#define EXPECT(_string, _value, _expected) \
 do { long long __tempval1 = _value; long long __tempval2 = _expected; \
    ASSERT(__tempval1 == __tempval2, "Checking " _string ": expected %lld but was %lld.", __tempval2, __tempval1); } while(0);

int use_colors(void);

#define PRINT_ERROR(_string, ...) do { fprintf(stderr, "c@2c: " _string "\n", ##__VA_ARGS__); } while(0)

#define LOG(_color, _string, ...) do {\
  if (use_colors()) { \
    printf(_color _string ANSI_NORMAL "\n", ##__VA_ARGS__);\
  } else {\
    printf(_string "\n", ##__VA_ARGS__);\
  } } while (0)


#define DEBUG

#ifdef DEBUG
#define DEBUG_LOG(_string, ...) do { printf("%s:%d " _string, __func__, __LINE__, ##__VA_ARGS__); printf("\n"); } while(0)
#else
#define DEBUG_LOG(_string, ...)
#endif
#define LOG_FUNC DEBUG_LOG("[ENTERED]");
