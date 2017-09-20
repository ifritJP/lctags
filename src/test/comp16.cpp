#define METHOD() typedef void (func)( int val );

METHOD();

struct DATA {
  func * pFunc;
};

void sub( DATA * pData )
{
  pData->
}
