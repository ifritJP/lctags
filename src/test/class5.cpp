template <typename T1, typename T2> class TEMP2
{
  typedef T1 tmp_t;
public:
  const tmp_t s_val;

  TEMP2() : s_val( 0 )
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

static void func()
{
  TEMP2<int,int> tmp;
  tmp.
}
