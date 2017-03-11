#include <stdio.h>
class Test2;
class Test1
{
  int member;
public:
  Test2 * pTest;
  static int sub() {
    return 1;
  }
  int func() {
    return 0;
  }
};
class Test2
{
  Test1 * pTest;
}

int main()
{
  Test1 * pTest1; /** */

  Test1::sub();
  pTest1->pTest->
//   return 0;
// }
    
}

