#include <stdio.h>

int s_vals[1];
#define POINTER(X) (&(X))
#define VAL5P (&val5)
#define SETVAL6 int * pVal6 = &val6;
int func3( int aaa[1][2], int abcddd[10][1], int* pval1, int val2, int * val3, int* pval4 )
{
    int * pVal = POINTER((*pval1));
    int valval2 = -val2;
    int valval3 = *(int *)(val3 + 1);
    int * pAAA = &aaa[0][0];
    //int * pValval5 = VAL5P;
    //SETVAL6;
    abcddd[0][0] = (*pval1);
    *pVal = 1;
    (*pval4) = 2;
    s_vals[0] = 3;
    *pAAA = 4;
    return 1;
}

void func2( int val1, int val2, int * val3, int val4, int val5, int val6 )
{
  int abcddd[ 10 ][ 1 ] = { 0 };
  int aaa[ 1 ][ 2 ] = { 0 };
  int bbb[ 1 ][ 2 ] = { 0 };

  func3( aaa, abcddd, &val1, val2, val3, &val4 );

  printf( "val = %d, s_vals[0]=%d, aaa[0] = %d, val4 = %d, abcddd[0][0] = %d\n",
	  val1, s_vals[0], aaa[0][0], val4, abcddd[0][0] );
}


int func( int val1, int val2, int * val3, int val4, int val5, int val6 )
{
  int abcddd[ 10 ][ 1 ] = { 0 };
  int aaa[ 1 ][ 2 ] = { 0 };
  int bbb[ 1 ][ 2 ] = { 0 };
  {
    int * pVal = POINTER(val1);
    int valval2 = -val2;
    int valval3 = *(int *)(val3 + 1);
    int * pAAA = &aaa[0][0];
    //int * pValval5 = VAL5P;
    //SETVAL6;
    abcddd[0][0] = val1;
    *pVal = 1;
    val4 = 0;
    s_vals[0] = 0;
    *pAAA = 0;
    return 1;
  }
}

#define SYM val
#define RET_SYM return SYM;
#define RET return
int func4( int val )
{
  if ( val ) {
    return 1;
  }
  if ( val == 1 ) {
    return val;
  }
  if ( val == 2 ) {
    return val + 2;
  }
  if ( val == 3 ) {
    return SYM;
  }
  if ( val == 4 ) {
    RET   1   ;
  }
  if ( val == 5 ) {
    RET_SYM;
  }
  return 1;
}

int main()
{
  int val = 2;
  //func( 1, 2, &val, 4, 5, 6 );
  func2( 10, 2, &val, 4, 5, 6 );
}

