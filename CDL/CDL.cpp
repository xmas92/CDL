
// Copyright (c) 2020 Axel Boldt-Christmas
//

#include "CDL.h"

#include <cassert>
#include <map>
#include <memory>
#include <span>
#include <typeinfo>
#include <vector>

#include "persistent/chunked_array.hpp"
#include "persistent/map.hpp"
#include "persistent/set.hpp"

template <class T>
class MyAllocator : public std::allocator<T> {
 public:
  using size_type = size_t;
  using difference_type = ptrdiff_t;
  using pointer = T*;
  using const_pointer = const T*;
  using reference = T&;
  using const_reference = const T&;
  using value_type = T;

  MyAllocator() = default;
  MyAllocator(const MyAllocator&) = default;

  pointer allocate(size_type n, const void* hint = 0) {
    auto alloc = std::allocator<T>();
    T* t = std::allocator_traits<std::allocator<T>>::allocate(alloc, n, hint);
    std::cout << "  used MyAllocator to allocate   at address " << t << " (+) ";
    std::cout << typeid(*t).name() << "[" << n << "]" << std::endl;
    return t;
  }

  void deallocate(pointer p, size_type n) {
    auto alloc = std::allocator<T>();
    std::cout << "  used MyAllocator to deallocate at address " << p << " (-) ";
    std::cout << typeid(*p).name() << "[" << n << "]" << std::endl;
    std::allocator_traits<std::allocator<T>>::deallocate(alloc, p, n);
  }

  pointer address(reference x) const { return &x; }
  const_pointer address(const_reference x) const { return &x; }
  MyAllocator<T>& operator=(const MyAllocator&) noexcept { return *this; }
  void construct(pointer p, const T& val) noexcept {
    auto alloc = std::allocator<T>();
    std::cout << "  used MyAllocator to construct at address " << p << " (-) ";
    std::cout << typeid(*p).name() << std::endl;
    new ((T*)p) T(val);
  }
  void destroy(pointer p) noexcept {
    std::cout << "  used MyAllocator to destroy    at address " << p << " (-) ";
    std::cout << typeid(*p).name() << std::endl;
    p->~T();
  }

  size_type max_size() const noexcept { return size_t(-1); }

  template <class U>
  struct rebind {
    typedef MyAllocator<U> other;
  };

  template <class U>
  MyAllocator([[maybe_unused]] const MyAllocator<U>&) {}

  template <class U>
  MyAllocator& operator=(const MyAllocator<U>&) {
    return *this;
  }
};

struct Myhash {
  size_t operator()(const int& value) const noexcept { return value; }
};

template <typename Set>
void testSet(Set& set) {
  for (int i = 0; i < 32; i++) {
    set.insert((1 << i) - 1);
    set.insert(1 << i);
    set.insert((1 << i) + 1);
  }
  for (int i = 0; i < 32; i++) {
    assert(set.contains((1 << i) - 1));
    assert(set.contains(1 << i));
    assert(set.contains((1 << i) + 1));
  }
  std::cout << set.size() << std::endl;
  for (auto& v : set) {
    std::cout << v << std::endl;
  }
  for (int i = 0; i < 32; i++) {
    set.erase((1 << i) - 1);
    set.erase(1 << i);
    set.erase((1 << i) + 1);
  }
  for (int i = 0; i < 32; i++) {
    assert(not set.contains((1 << i) - 1));
    assert(not set.contains(1 << i));
    assert(not set.contains((1 << i) + 1));
  }
}

int main() {
  using persistent::chunked_array::ChunkedArray;
  [[maybe_unused]] ChunkedArray<int> chunkedArray;
  using persistent::ConcurrentUnorderedMap;
  using persistent::ConcurrentUnorderedSet;
  using persistent::UnorderedMap;
  using persistent::UnorderedSet;
  using std::cout;
  using std::endl;
  cout << std::boolalpha;

  UnorderedSet<int, Myhash, std::equal_to<>, 64, MyAllocator<int>> USet;
  ConcurrentUnorderedSet<int, Myhash, std::equal_to<>, 64, MyAllocator<int>>
      CUSet;

  UnorderedMap<int, int, Myhash, std::equal_to<>, std::equal_to<>, 64,
               MyAllocator<std::pair<int,int>>>
      UMap;
  ConcurrentUnorderedMap<int, int, Myhash, std::equal_to<>, std::equal_to<>, 64,
                         MyAllocator<std::pair<int, int>>>
      CUMap;

  cout << "MakeTransient" << endl;
  decltype(USet)::TransientContainerType<std::allocator<int>> TUSet = {
      1, 10, 100, 1000, 10000};
  cout << "MakePersistent" << endl;
  USet.data = TUSet.MakePersistent<MyAllocator<int>>();
  cout << "Done" << endl;

  // testSet(USet);
  // testSet(CUSet);
  UnorderedSet<int> USet0;
  ConcurrentUnorderedSet<int> CUSet0;

  UnorderedMap<int, int> UMap0;
  ConcurrentUnorderedMap<int, int> CUMap0;
  testSet(USet0);
  testSet(CUSet0);

  return 0;
}
