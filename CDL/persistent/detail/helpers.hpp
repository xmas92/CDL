
// Copyright (c) 2020 Axel Boldt-Christmas
//

#pragma once
#include <bit>
#include <bitset>
#include <climits>
#include <concepts>
#include <limits>

namespace persistent {

namespace helper {
using std::numeric_limits;
using std::unsigned_integral;
template <unsigned_integral Base>
    [[nodiscard]] constexpr auto Log2(Base x) noexcept {
  auto i = sizeof(Base) * CHAR_BIT - 1;
  while (i > 0) {
    if (x >= static_cast<Base>(1u) << i) return i;
    i--;
  }
  return i;
}

template <unsigned_integral Base>
    [[nodiscard]] constexpr bool IsPowerOf2(Base x) noexcept {
  for (auto i = 0; i < sizeof(Base) * CHAR_BIT; ++i)
    if (x == static_cast<Base>(1u) << i) return true;
  return false;
}

using std::bitset;
template <size_t N>
[[nodiscard]] inline constexpr size_t CountLowBits(
    const bitset<N>& bitmap, const size_t bitIndex) noexcept {
  using std::popcount;
  size_t low_bits_count = 0;
  if constexpr (N <= sizeof(unsigned long) * CHAR_BIT) {
    low_bits_count = popcount((bitmap.to_ulong() & ((1ul << bitIndex) - 1)));
  } else if constexpr (N <= sizeof(unsigned long long) * CHAR_BIT) {
    low_bits_count = popcount((bitmap.to_ullong() & ((1ull << bitIndex) - 1)));
  } else {
    low_bits_count = (bitmap & (bitset<N>().set() >> (N - bitIndex))).count();
  }
  return low_bits_count;
}

using std::bitset;
template <uint16_t N>
[[nodiscard]] inline constexpr size_t Count(const bitset<N>& bitmap) noexcept {
  using std::popcount;
  if constexpr (N <= sizeof(unsigned long) * CHAR_BIT) {
    return popcount(bitmap.to_ulong());
  } else if constexpr (N <= sizeof(unsigned long long) * CHAR_BIT) {
    return popcount(bitmap.to_ullong());
  } else {
    return bitmap.count();
  }
}
/*
struct ValueConstructBaseTag {
  explicit ValueConstructBaseTag() = default;
};
struct DefaultConstructBaseTag {
  explicit DefaultConstructBaseTag() = default;
};
using std::conjunction_v;
using std::forward;
using std::is_empty_v;
using std::is_final_v;
using std::is_nothrow_constructible;
using std::is_nothrow_default_constructible;
template <class Base, class T>
requires(is_empty_v<Base> && !is_final_v<Base>) class AssociateBase final
    : private Base {
 public:
  T value;

  template <class... Args>
  constexpr explicit AssociateBase(DefaultConstructBaseTag, Args&&... args) noexcept(
      conjunction_v<is_nothrow_default_constructible<Base>,
                    is_nothrow_constructible<T, Args...>>)
      : Base(), value(forward<Args>(args)...) {}

  template <class BaseArg, class... Args>
  constexpr AssociateBase(
      ValueConstructBaseTag, BaseArg&& base_arg,
      Args&&... args) noexcept(conjunction_v<is_nothrow_constructible<Base,
                                                                      BaseArg>,
                                             is_nothrow_constructible<T,
                                                                      Args...>>)
      : Base(forward<BaseArg>(base_arg)), value(forward<Args>(args)...) {}

  constexpr Base& GetBase() noexcept { return *this; }
  constexpr const Base& GetBase() const noexcept { return *this; }
};
*/
}  // namespace detail

}  // namespace persistent