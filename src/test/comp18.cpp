struct HOGE {
  int value1;
  int value2;
  struct sub_{
    int value;
    struct {
      int value;
    } sub2;
    struct sub_ * pSub;
  } sub;
};

void func( HOGE * pHoge )
{
  pHoge->
}


