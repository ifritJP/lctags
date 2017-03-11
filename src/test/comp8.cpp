#include <stdio.h>

typedef enum {
  TEST80_1,
  TEST80_2,
  TEST80_3
} enum_TEST80;

class TEST801;
class TEST802;
class TEST803;

namespace ns81 {
  typedef enum {
    TEST8_1,
    TEST8_2,
    TEST8_3
  } enum_TEST8;

  class TEST81;
  class TEST82;
  class TEST83;

  namespace ns82 {
    typedef enum {
      TEST82_1,
      TEST82_2,
      TEST82_3
    } enum_TEST82;

    class TEST811;
    class TEST812;
    class TEST813;

    namespace ns83 {
      typedef enum {
	TEST83_1,
	TEST83_2,
	TEST83_3
      } enum_TEST83;

      class TEST821;
      class TEST822;
      class TEST823;
    }
    int sub()
    {
      ns81::TE
    }
  }
}
