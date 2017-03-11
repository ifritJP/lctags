#include <stdio.h>

namespace ns1 {
  int val1 = 1;
  int val2 = 2;
  namespace ns2 {
    int val2 = -2;
    namespace ns3 {
      int val3 = 3;
    }
    void sub() {
      printf( "%d, %d, %d, %d\n", val1, val2, ns1::val2, ns3::val3 );
    }
  }
}

main()
{
  ns1::ns2::sub();
}
