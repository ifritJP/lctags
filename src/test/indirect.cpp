extern "C" {
    typedef struct {
        void (*func_t)(void);
    } test_str_t;
}


typedef void (test_indirect_t)(void);

static void test_indirect( void )
{
}

static void test_indirect2( void )
{
}

static void test_indirect3( void )
{
}

void test_sub( test_indirect_t * pFunc, test_str_t * pStr) {
    pFunc();
    pStr->func_t();
}

static void foo( test_str_t * pStr )
{
    test_sub( test_indirect, pStr );
}
