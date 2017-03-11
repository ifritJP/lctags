#include <stdio.h>

typedef enum {
  TEST90_1,
  TEST90_2,
  TEST90_3
} enum_TEST90;

class TEST901;
class TEST902;
class TEST903;

namespace ns91 {
  typedef enum {
    TEST9_1,
    TEST9_2,
    TEST9_3
  } enum_TEST9;

  class TEST91;
  class TEST92;
  class TEST93;

  namespace ns92 {
    typedef enum {
      TEST92_1,
      TEST92_2,
      TEST92_3
    } enum_TEST92;

    class TEST911;
    class TEST912;
    class TEST913;

    namespace ns93 {
      typedef enum {
	TEST93_1,
	TEST93_2,
	TEST93_3
      } enum_TEST93;

      class TEST921;
      class TEST922;
      class TEST923;
    }
    int sub()
    {
      ::T
    }
  }
}

