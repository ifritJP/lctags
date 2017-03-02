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

typedef int (Callback_t)(void);

typedef enum { enum_val1, enum_val2 } enum_t;

struct yyyy;

typedef struct yyyy YYYY;

struct yyyy {
    int kkkk;
  Callback_t * pCallback;
};

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
    struct_value2_t stVal2;
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
      } struct_func_t;
      
      

        /**
         * func0 comment
         *
         * ほげ
         */
        int func0( int (func)( void ) ) {
            char buf[ 1 ];
            func();
            return 0;
        }

        /**
         * func1 comment
         * 
         * あげ
         */
        int func1( Callback_t callback ) {
            char buf[ 1 ];
            callback();
            return 0;
        }

        /** func2 comment */
        static int func2() {
            char buf[ 2 ];
            static int ps_local = s_static;
            g_global = s_static;
            return 0;
        }

        int func5( struct_func_t * pClass ) {
            char buf[5];
            pClass->   pCallback();

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
                func2();
                (*pCallback)();
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
                int a;
            }
        }

        void sub2() {
            ns2::func3( ns2::func2 );
        }
    }

    void sub2() {
        YYYY aYYYYY;
        aYYYYY.kkkk = 0;
	aYYYYY.pCallback();
    }
}

namespace ns4 {
  int values[] = {
    ns1::g_global
  };
}
