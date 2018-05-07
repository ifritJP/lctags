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

void sub( test_indirect_t * pFunc) {
    pFunc();
}

static void foo()
{
    sub( test_indirect );
}
