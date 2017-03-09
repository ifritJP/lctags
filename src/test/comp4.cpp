#include <stdio.h>

class Test1
{
  int member;
public:
  static int sub()
  {
    return 1;
  }
  int func() {
    return 0;
  }
};

void zzzz( int val1, int val2 )
{
}

Test1 * hoge( int val1, int val2 )
{
  return NULL;
}

int main()
{
  Test1 * pTest1; /** */

  Test1::sub();

  ((Test1*)NULL)->
//   return 0;
// }
    
}

