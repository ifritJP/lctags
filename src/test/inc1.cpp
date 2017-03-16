#include "inc1.h"

#define STRUCT_AAAA                       \
    union {                               \
        struct {                          \
            struct _struct_AAA  *_pppp;   \
        } bbbbb;                          \
    } aaaa;                               \
#endif

struct _str_aaaa {
    STRUCT_AAAA
};

struct _str_bbbb {
    STRUCT_AAAA
};
