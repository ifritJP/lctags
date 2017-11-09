typedef struct {
    int val;
    int val2;
} VAL_t;

typedef struct {
    int val;
    int val2;
} VAL2_t;

typedef enum {
    enum_val1,
    enum_val2,
    enum_val3
} enum_t;

#define M_VAL enum_val3

extern VAL2_t * func();

void sub( VAL_t * pVal, VAL2_t * pVal2 )
{
    /** テスト */ pVal->val = enum_val1;
    pVal2->val = enum_val2;
    pVal->val2 = enum_val3;
    pVal2->val2 = M_VAL;
    func()->val = enum_val2;
}
