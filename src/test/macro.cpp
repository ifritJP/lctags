#include <stdio.h>

#define aaa 123

#define DECL( AAA )			\
  int val##AAA;				\
  int VAL##AAA;				\


typedef struct {
  DECL( aaa );
  DECL( 2 );
  DECL( 3 );
  DECL( 4 );
  DECL( 5 );
  DECL( 6 );
} VAL;


void macro_func()
{
}

void macro_func2()
{
}

#define macro_func() macro_func2()


void macro_func3()
{
    macro_func();
}
