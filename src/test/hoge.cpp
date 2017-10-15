#include <stdio.h>
#include <time.h>


#define NNNN
#include <./hoge.h>

#ifdef AAA
#endif
#if BBB
#endif

#define ARRAY_LENGTH 10
typedef int int_t;

typedef enum { enum_val1,
	       enum_val2,
	       enum_val3 = enum_val1 + 1000
} enum_t;

typedef struct zzzz ZZZZ;
struct zzzz {
    int aaaa;
#include <field.h>
int cccc;
  va_list argp;
};

namespace iiii {
#include <field.h>
#if 0
#endif
}

typedef struct TEST {
    int aaaa;
} TEST;

typedef ZZZZ (Callback_t)( TEST * pTest );

struct yyyy;
typedef struct yyyy YYYY;
struct yyyy {
    int kkkk;
  Callback_t * pCallback;
};

typedef struct {
    char charValue;
    char * charValueP;
    char charValueA[ ARRAY_LENGTH ];
    char * charValueAP[ ARRAY_LENGTH ];
    char charValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    char * charValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];

    short shortValue;
    short * shortValueP;
    short shortValueA[ ARRAY_LENGTH ];
    short * shortValueAP[ ARRAY_LENGTH ];
    short shortValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    short * shortValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    
    int intValue;
    int * intValueP;
    int intValueA[ ARRAY_LENGTH ];
    int * intValueAP[ ARRAY_LENGTH ];
    int intValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    int * intValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    
    long longValue;
    long * longValueP;
    long longValueA[ ARRAY_LENGTH ];
    long * longValueAP[ ARRAY_LENGTH ];
    long longValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    long * longValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    
    long long longLongValue;
    long long * longLongValueP;
    long long longLongValueA[ ARRAY_LENGTH ];
    long long * longLongValueAP[ ARRAY_LENGTH ];
    long long longLongValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    long long * longLongValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
} struct_value_t;

typedef struct {
    char charValue;
    char * charValueP;
    char charValueA[ ARRAY_LENGTH ];
    char * charValueAP[ ARRAY_LENGTH ];
    char charValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    char * charValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];

    short shortValue;
    short * shortValueP;
    short shortValueA[ ARRAY_LENGTH ];
    short * shortValueAP[ ARRAY_LENGTH ];
    short shortValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    short * shortValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    
    int intValue;
    int * intValueP;
    int intValueA[ ARRAY_LENGTH ];
    int * intValueAP[ ARRAY_LENGTH ];
    int intValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    int * intValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    
    long longValue;
    long * longValueP;
    long longValueA[ ARRAY_LENGTH ];
    long * longValueAP[ ARRAY_LENGTH ];
    long longValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    long * longValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    
    long long longLongValue;
    long long * longLongValueP;
    long long longLongValueA[ ARRAY_LENGTH ];
    long long * longLongValueAP[ ARRAY_LENGTH ];
    long long longLongValueA2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    long long * longLongValueAP2[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
} struct_value2_t;



void test( int argInt[ ARRAY_LENGTH ], int_t argInt2[ ARRAY_LENGTH ] );


void test( int argInt[ ARRAY_LENGTH ], int_t argInt2[ ARRAY_LENGTH ] )
{
    int_t array2Int[ ARRAY_LENGTH ][ ARRAY_LENGTH ];
    struct_value_t stVal;
    struct_value_t * stValP;
    struct_value_t stValA[ ARRAY_LENGTH ];
    struct_value_t * stValPA[ ARRAY_LENGTH ];
    struct_value2_t stVal2, stVal3;
    stVal.longValue = 0;
    stVal2.longValue = 0;
}


class TestClass {

public:
    TestClass() { }

public:
    int func() {
        return 0; 
    }
};

namespace ns1 {

    int g_global = 0;
    static int s_static = 0;

    typedef struct {
        Callback_t * pCallback;
        struct {
          int efgh;
        } abcd;
    } struct_func_t;

    namespace ns2 {
      typedef struct {
        Callback_t * pCallback;
        struct {
          int z1;
          int z2;
        };
        union {
          int z3;
          int z4;
        };
      } struct_func_t;
      
      static int s_static2 = 0;

        /**
         * func0 comment
         *
         * ほげ
         */
        int func0( ZZZZ (func)( TEST * pClass ) ) {
            char buf[ 1 ];
            func( NULL );
            return 0;
        }

        /**
         * func1 comment
         * 
         * あげ
         */
        int func1( Callback_t callback ) {
            char buf[ 1 ] = { enum_val1 };
            callback( NULL );
            return 0;
        }

        /** func2 comment */
        static ZZZZ func2( TEST * pTest ) {
            char buf[ 2 ];
            static int ps_local = s_static;
            g_global = s_static;
	    ZZZZ aaa;
            return aaa;
        }

        int func5( struct_func_t * pClass ) {
            char buf[5];
            pClass->   pCallback( NULL );
            pClass->z2 = 1;
            TestClass  aaa;
            return 0;
        }
      
        /** func3 comment */
        int func3( Callback_t * pCallback ) {
            char buf[50];
            {
                char buf[100];
                func1( func2 );
                {
                    char buf[100];
                    func3( func2 );
                }
            }
            {
                char buf[10];
                func2( NULL );
                (*pCallback)( NULL );
                func0( func2 );
                func5( NULL );
            }
            ::ns1::struct_func_t val;
            val.abcd.efgh = 1;
            return enum_val1;
        }

        int func4( TestClass * pTest ) {
            char buf[50];
            pTest->func();
            return 0;
        }


    }

    namespace ns31 {

        void sub( char ** ppArg ) {
            for ( ; *ppArg != NULL; ppArg++ ) {
                printf( "%s\n", *ppArg );
                int aa;
                aa = 0;
            }
        }

        void sub2() {
            ns2::func3( ns2::func2 );
        }
    }

  struct yyyy sub2() {
    YYYY aYYYYY;
    aYYYYY.kkkk = 0;
    aYYYYY.pCallback( NULL );
    return aYYYYY;
  }
}

namespace ns4 {
  int values[] = {
    ns1::g_global
  };
}

template <typename T1, typename T2> class TEMP
{
  typedef T1 tmp_t;
public:
  const tmp_t s_val;

  TEMP(T1 val ) : s_val( val )
  {}

  T2 aaaaaa(const T1 &t) const
  {
    return t;
  }

  T2 bbbbbb(const T1 &t) const
  {
    return aaaaaa( t);
  }
};

namespace {
  int rootval;

  void func()
  {
    TEMP<int_t,int> tmp(1);
    TEMP<int_t,int> tmp2(1);
    tmp.aaaaaa(2);
    
    if ( 1 ) {
      rootval = 1;
      ns1::sub2();
    }
  }
}

typedef int INT_t;

static int s_vals[] = { enum_val1, enum_val2 };

class TestClass2
{
public:
    ~TestClass2()
    {
      enum { INN };
      YYYY aYYYYY = { 0 };
      int val = INN;
      int val2[] = { enum_val1 };
      val = 0;
    }

#define POINTER(X) (&(X))
#define VAL5P (&val5)
#define SETVAL6 int * pVal6 = &val6;
    void func( int val, int val2, int * val3, int val4, int val5, int val6 )
    {
        int abcddd[ 10 ][ 1 ] = { 0 };
        int aaa[ 1 ][ 2 ] = { 0 };
        {
            int * pVal = POINTER(val);
            int valval2 = -val2;
            int valval3 = *(int *)(val3 + 1);
	    int * pAAA = &aaa[0][0];
            //int * pValval5 = VAL5P;
            //SETVAL6;
            abcddd[0][0] = val;
            *pVal = 1;
            val4 = 2;
            s_vals[0] = 3;
	    *pAAA = 4;
        }
    }
} CLASS2, CLASS3;
