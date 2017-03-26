
class Super1 {
  int val1;
};

template <typename T>
class Super2 {
  T val2;
};

template <typename T>
  class Sub : public Super1, Super2<T> {
public:
  T val3;
  Super2<T> func( T val ) {
    this->
  }
}
