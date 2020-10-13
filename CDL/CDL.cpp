
// Copyright (c) 2020 Axel Boldt-Christmas
//

#include "CDL.h"

#include <cassert>
#include <map>
#include <memory>
#include <span>
#include <sstream>
#include <typeinfo>
#include <vector>

#include "parsing/base_rules.hpp"
#include "parsing/detail/helpers.hpp"
#include "parsing/translation_input.hpp"
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
#include <codecvt>
#include <locale>
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
  std::u8string アップル(u8"アップル");
  auto& f = std::use_facet<std::codecvt<char32_t, char8_t, std::mbstate_t>>(
      std::locale());
  auto& f2 = std::use_facet<std::codecvt<char32_t, char16_t, std::mbstate_t>>(
      std::locale());
  auto& f3 = std::use_facet<std::codecvt<char32_t, wchar_t, std::mbstate_t>>(
      std::locale());
  std::u32string u32str(アップル.length(), 0);
  std::u32string u32strDir(U"アップル");
  char32_t ch1 = 'A';

  std::mbstate_t mb{};
  const std::u8string::value_type* from_ptr;
  std::u32string::value_type* to_ptr;

  auto res = f.in(mb, &アップル.front(), &アップル.back() + 1, from_ptr,
                  &u32str.front(), &u32str.back(), to_ptr);
  u32str.resize(to_ptr - &u32str.front());

  cout << "Max Length: " << f.max_length() << endl;
  cout << "Sizeof: " << sizeof(char8_t) << endl;
  cout << "Max Length: " << f2.max_length() << endl;
  cout << "Sizeof: " << sizeof(char16_t) << endl;
  cout << "Max Length: " << f3.max_length() << endl;
  cout << "Sizeof: " << sizeof(wchar_t) << endl;

  std::istringstream s1(
      "test1test1test1test1test1test1test1test1test1test1test1test1test1test1te"
      "st1test1test1test1test1test1test1test1test1test1test1test1test1test1");
  std::wistringstream s2(
      L"test2アップルアップルアップルアップルアップルアップルアップルアップル\n"
      L"アップルアップルアップルアップルップルアップルアップルアップルップル");
  std::basic_istringstream<char8_t> s3(
      u8"test3アップルアップルアップルアップルアップルアップルアップルア\r\nッ"
      u8"ルアップルアップルアップルアップルップルアップルアップルアップルップ");
  std::basic_istringstream<char32_t> s4(
      U"test4アップルアップルアップルアップルアップルアップルアップルアップ\n\r"
      U"アップルアップルアップルアップルップルアップルアップルアップルップル");
  std::istringstream s5(
      "\ntest5ÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖ\n"
      "ÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖÖ");

  parsing::LazyTranslationInput ti1(std::move(s1));
  parsing::EagerTranslationInput ti2(std::move(s2));
  parsing::EagerTranslationInput ti3(std::move(s3));
  parsing::EagerTranslationInput ti4(std::move(s4));
  parsing::EagerTranslationInput ti5(std::move(s5));

  std::basic_istringstream<char8_t> s6(u8"ルア\nップ");
  std::basic_istringstream<char8_t> s7(u8"ルア\rップ");
  std::basic_istringstream<char8_t> s8(u8"ルア\n\rップ");
  std::basic_istringstream<char8_t> s9(u8"ルア\r\nップ");

  parsing::EagerTranslationInput ti6(std::move(s6));
  parsing::EagerTranslationInput ti7(std::move(s7));
  parsing::EagerTranslationInput ti8(std::move(s8));
  parsing::EagerTranslationInput ti9(std::move(s9));

  auto str1 = ti1.Next(1);
  auto str2 = ti2.Next(2);
  auto str3 = ti3.Next(3);
  auto str4 = ti4.Next(4);
  auto str5 = ti5.Next(5);

  auto t10 = ti9;
  auto t11 = ti1;

  parsing::helper::VariadicBinaryFilter_t<
      std::is_same, std::tuple<int, int, float, double, float, unsigned>>
      a;
  cout << typeid(a).name() << endl;

  using parsing::rules::Tokenize;
  using parsing::rules::non_terminal::And;
  using parsing::rules::non_terminal::Not;
  using parsing::rules::non_terminal::Opt;
  using parsing::rules::non_terminal::Or;
  using parsing::rules::non_terminal::Plus;
  using parsing::rules::non_terminal::Seq;
  using parsing::rules::non_terminal::Star;
  using parsing::rules::terminal::Empty;
  using parsing::rules::terminal::Literal;
  Seq<Empty, Empty, Literal<U't'>> r1;
  Seq<Empty, Empty, Literal<U'e', Tokenize::Yes>, Literal<U's'>,
      Literal<U't', Tokenize::Yes>>
      r2;
  Seq<Or<Literal<U't', Tokenize::Yes>, Literal<U'e', Tokenize::Yes>>> r3;
  Seq<And<Literal<U'e'>, Literal<U's'>>, Not<Literal<U't'>, Literal<U'e'>>,
      Plus<Literal<U'e', Tokenize::Yes>, Literal<U's', Tokenize::Yes>>,
      Literal<U't'>, Literal<U'1', Tokenize::Yes>,
      Opt<Literal<U'1', Tokenize::Yes>>,
      Plus<Or<Literal<U'e'>, Literal<U's'>, Literal<U't', Tokenize::Yes>>>,
      Star<Or<Literal<U't', Tokenize::Yes>, Literal<U'e', Tokenize::Yes>,
              Literal<U's', Tokenize::Yes>, Literal<U'1'>>>>
      r4;
  assert(not r1.Match(ti1));
  assert(r2.Match(ti1));
  assert(r3.Match(ti1));
  assert(r4.Match(ti1));

  UnorderedSet<int, Myhash, std::equal_to<>, 64, MyAllocator<int>> USet;
  ConcurrentUnorderedSet<int, Myhash, std::equal_to<>, 64, MyAllocator<int>>
      CUSet;

  UnorderedMap<int, int, Myhash, std::equal_to<>, std::equal_to<>, 64,
               MyAllocator<std::pair<int, int>>>
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
