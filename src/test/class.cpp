#include "class.h"

template <typename T> class Set;

template <typename T>
class List : public std::vector<T>
{
    typedef std::vector<T> Base;
public:
    static const size_t npos = std::string::npos;
    explicit List(size_t count = 0, const T &defaultValue = T())
        : Base(count, defaultValue)
    {}

    template <typename CompatibleType>
    List(const std::vector<CompatibleType> &other)
        : Base(other.size(), T())
    {
        const size_t len = other.size();
        for (size_t i=0; i<len; ++i) {
            std::vector<T>::operator[](i) = other.at(i);
        }
    }

    List(std::initializer_list<T> list)
        : Base(list)
    {}

    List(typename Base::const_iterator f, typename Base::const_iterator l)
        : Base(f, l)
    {
    }

    bool contains(const T &t) const
    {
        return std::find(Base::begin(), Base::end(), t) != Base::end();
    }

    bool isEmpty() const
    {
        return Base::empty();
    }






  
  
};
