typedef struct VAL_t {
    int val;
    int val2;
} VAL_t;

typedef struct VAL2_t {
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

void sub( VAL_t * pVal, VAL2_t * pVal2, VAL_t * pVal3[], struct VAL2_t VAL2_t )
{
    /** テスト */ pVal->val = enum_val1;
    pVal2->val = enum_val2;
    pVal->val2 = enum_val3;
    pVal2->val2 = M_VAL;
    func()->val = enum_val2;
    pVal3[0]->val = enum_val1;
    VAL2_t.val = enum_val3;
}
