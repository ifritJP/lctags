#include <stdio.h>

typedef enum {
  TEST70_1,
  TEST70_2,
  TEST70_3
} enum_TEST70;

class TEST701;
class TEST702;
class TEST703;

namespace ns71 {
  typedef enum {
    TEST7_1,
    TEST7_2,
    TEST7_3
  } enum_TEST7;

  class TEST71;
  class TEST72;
  class TEST73;

  namespace ns72 {
    typedef enum {
      TEST72_1,
      TEST72_2,
      TEST72_3
    } enum_TEST72;

    class TEST711;
    class TEST712;
    class TEST713;

    namespace ns73 {
      typedef enum {
	TEST73_1,
	TEST73_2,
	TEST73_3
      } enum_TEST73;

      class TEST721;
      class TEST722;
      class TEST723;
    }
    int sub()
    {
      ns71::ns72::
    }
  }
}
