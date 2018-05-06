typedef void (test_indirect_t)(void);

static void test_indirect( void )
{
}

void sub( test_indirect_t * pFunc) {
    pFunc();
}

static void foo()
{
    sub( test_indirect );
}
