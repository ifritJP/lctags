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

int main()
{
  Test1 * pTest1; /** */

  Test1::sub();

  zzzz( pTest1->me
//   return 0;
// }
    
}

