
// Copyright (c) 2020 Axel Boldt-Christmas
//

#pragma once
#include <climits>
#include <concepts>
#include <functional>
#include <memory>

#include "hamt.hpp"

namespace persistent {

template <typename T, typename Hash, typename Predicate, size_t TrieFanOut,
          typename Allocator, bool ordered, bool ThreadSafe>
requires(helper::IsPowerOf2(TrieFanOut) &&
         std::unsigned_integral<std::invoke_result_t<Hash, T>> &&
         std::predicate<Predicate, T, T>) struct Set {
 private:
  using HAMT = hamt::HAMT<T, Hash, Predicate, TrieFanOut, Allocator, ThreadSafe>;

 public:
  using ContainerType = std::conditional_t<ordered, void, HAMT>;
  template <typename Allocator = Allocator>
  using TransientContainerType =
      std::conditional_t<ordered, void,
                         typename HAMT::template TransientHAMT<Allocator>>;

  ContainerType data;

  using difference_type = typename ContainerType::difference_type;
  using value_type = typename ContainerType::value_type;
  using size_type = typename ContainerType::size_type;
  using const_pointer = typename ContainerType::const_pointer;
  using const_reference = typename ContainerType::const_reference;
  using const_iterator = typename ContainerType::const_iterator;

  constexpr Set() noexcept {};
  template <typename InputIter>
  constexpr Set(InputIter first, InputIter last) : data(first, last) {}
  constexpr Set(const Set&) = default;
  constexpr Set(std::initializer_list<T> il)
      : data(std::cbegin(il), std::cend(il)) {}
  constexpr Set& operator=(const Set&) = default;
  constexpr Set(Set&&) = default;
  constexpr Set& operator=(Set&&) = default;
  ~Set() = default;

  [[nodiscard]] constexpr bool contains(const T& value) const {
    return data.contains(value);
  }
  [[nodiscard]] constexpr bool contains(T&& value) {
    using std::forward;
    return data.contains(forward<T>(value));
  }

  [[nodiscard]] constexpr std::optional<T> find(const T& value) {
    return data.find(value);
  }
  [[nodiscard]] constexpr std::optional<T> find(T&& value) {
    using std::forward;
    return data.find(forward<T>(value));
  }

  template <typename InputIter>
  constexpr void insert(InputIter first, InputIter last) {
    for (; first != last; first++) insert(*first);
  }

  constexpr void insert(const T& value) {
    using std::move;
    data = move(data.insert(value));
  }
  constexpr void insert(T&& value) {
    using std::forward;
    using std::move;
    data = move(data.insert(forward<T>(value)));
  }

  constexpr void erase(const T& value) {
    using std::move;
    data = move(data.erase(value));
  }
  constexpr void erase(T&& value) {
    using std::forward;
    using std::move;
    data = move(data.erase(forward<T>(value)));
  }

  [[nodiscard]] constexpr size_t size() const noexcept { return data.size(); }

  [[nodiscard]] constexpr bool empty() const noexcept { return data.empty(); }

  constexpr const_iterator begin() const noexcept { return data.begin(); }
  constexpr const_iterator end() const noexcept { return data.end(); }
  constexpr const_iterator cbegin() const noexcept { return data.cbegin(); }
  constexpr const_iterator cend() const noexcept { return data.cend(); }

  constexpr void clear() noexcept { data = ContainerType(); }
};

template <typename T, typename Hash = std::hash<T>,
          typename Predicate = std::less<T>,
          size_t TrieFanOut = sizeof(T) * CHAR_BIT,
          typename Allocator = std::allocator<T>>
using OrderedSet = Set<T, Hash, Predicate, TrieFanOut, Allocator, true, false>;

template <typename T, typename Hash = std::hash<T>,
          typename Predicate = std::equal_to<T>,
          size_t TrieFanOut = sizeof(T) * CHAR_BIT,
          typename Allocator = std::allocator<T>>
using UnorderedSet =
    Set<T, Hash, Predicate, TrieFanOut, Allocator, false, false>;

template <typename T, typename Hash = std::hash<T>,
          typename Predicate = std::less<T>,
          size_t TrieFanOut = sizeof(T) * CHAR_BIT,
          typename Allocator = std::allocator<T>>
using ConcurrentOrderedSet =
    Set<T, Hash, Predicate, TrieFanOut, Allocator, true, true>;

template <typename T, typename Hash = std::hash<T>,
          typename Predicate = std::equal_to<T>,
          size_t TrieFanOut = sizeof(T) * CHAR_BIT,
          typename Allocator = std::allocator<T>>
using ConcurrentUnorderedSet =
    Set<T, Hash, Predicate, TrieFanOut, Allocator, false, true>;

}  // namespace persistent