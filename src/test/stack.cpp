static void func1( int val );
static void func2( int val );
static void func3( int val );
static void sub();


static void func1( int val )
{
    if ( val >= 10 ) {
        return;
    }
    {
        char buf[ 10 ];
        func2( val + 1 );
    }
    {
        char buf[ 100 ];
        func2( val + 1 );
    }
}

static void func2( int val )
{
    if ( val >= 10 ) {
        return;
    }
    {
        char buf[ 10 ];
        func3( val + 1 );
    }
    {
        char buf[ 100 ];
        func3( val + 1 );
    }
}

static void func3( int val )
{
    if ( val >= 10 ) {
        return;
    }
    {
        char buf[ 10 ];
        func1( val + 1 );
    }
    {
        char buf[ 100 ];
        func1( val + 1 );
    }
}

static void sub()
{
    char buf[ 10 ];
    func1( 0 );
}
