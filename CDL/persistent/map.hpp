
// Copyright (c) 2020 Axel Boldt-Christmas
//

#pragma once
#include <utility>

#include "hamt.hpp"

namespace persistent {

template <typename Key, typename T, typename KeyHash, typename KeyPredicate,
          typename TPredicate, size_t TrieFanOut, typename Allocator,
          bool ordered, bool ThreadSafe>
requires(helper::IsPowerOf2(TrieFanOut) &&
         std::unsigned_integral<std::invoke_result_t<KeyHash, Key>> &&
         std::predicate<KeyPredicate, Key, Key> &&
         std::predicate<TPredicate, T, T>) struct Map {
  using ValueType = std::pair<Key, T>;
  struct Predicate {
    constexpr bool operator()(const ValueType& lhs,
                              const ValueType& rhs) const {
      const auto& [left_key, left] = lhs;
      const auto& [right_key, right] = rhs;
      return Predicate()(left_key, right_key);
    }
  };
  struct UnnecessaryExchangePredicate {
    constexpr bool operator()(const ValueType& lhs,
                              const ValueType& rhs) const {
      const auto& [left_key, left] = lhs;
      const auto& [right_key, right] = rhs;
      if constexpr (ordered) {
        return !Predicate()(left_key, right_key) and
               !Predicate()(right_key, left_key) and TPredicate()(left, right);
      } else {
        return Predicate()(left_key, right_key) and TPredicate()(left, right);
      }
    }
  };
  struct Hash {
    [[no_unique_address]] const KeyHash hasher;
    std::size_t operator()(ValueType const& value) const noexcept {
      const auto& [key, ignore] = value;
      return hasher(key);
    }
  };
  using HAMT =
      hamt::HAMT<ValueType, Hash, Predicate, TrieFanOut, Allocator, ThreadSafe>;

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

  constexpr Map() noexcept {};
  template <typename InputIter>
  constexpr Map(InputIter first, InputIter last) : data(first, last) {}
  constexpr Map(const Map&) = default;
  constexpr Map(std::initializer_list<value_type> il)
      : data(std::cbegin(il), std::cend(il)) {}
  constexpr Map& operator=(const Map&) = default;
  constexpr Map(Map&&) = default;
  constexpr Map& operator=(Map&&) = default;
  ~Map() = default;

  [[nodiscard]] constexpr bool contains(const value_type& value) const {
    return data.contains(value);
  }
  [[nodiscard]] constexpr bool contains(value_type&& value) {
    using std::forward;
    return data.contains(forward<T>(value));
  }

  [[nodiscard]] constexpr std::optional<value_type> find(
      const value_type& value) {
    return data.find(value);
  }
  [[nodiscard]] constexpr std::optional<value_type> find(value_type&& value) {
    using std::forward;
    return data.find(forward<value_type>(value));
  }

  template <typename InputIter>
  constexpr void insert(InputIter first, InputIter last) {
    for (; first != last; first++) insert(*first);
  }

  constexpr void insert(const value_type& value) {
    using std::move;
    data = move(data.template insert<UnnecessaryExchangePredicate>(value));
  }
  constexpr void insert(value_type&& value) {
    using std::forward;
    using std::move;
    data = move(
        data.template insert<UnnecessaryExchangePredicate>(forward<T>(value)));
  }

  constexpr void erase(const value_type& value) {
    using std::move;
    data = move(data.erase(value));
  }
  constexpr void erase(value_type&& value) {
    using std::forward;
    using std::move;
    data = move(data.erase(forward<value_type>(value)));
  }

  [[nodiscard]] constexpr size_t size() const noexcept { return data.size(); }

  [[nodiscard]] constexpr bool empty() const noexcept { return data.empty(); }

  constexpr const_iterator begin() const noexcept { return data.begin(); }
  constexpr const_iterator end() const noexcept { return data.end(); }
  constexpr const_iterator cbegin() const noexcept { return data.cbegin(); }
  constexpr const_iterator cend() const noexcept { return data.cend(); }

  constexpr void clear() noexcept { data = ContainerType(); }
};

template <typename Key, typename T, typename KeyHash = std::hash<Key>,
          typename KeyPredicate = std::less<Key>,
          typename TPredicate = std::equal_to<T>,
          size_t TrieFanOut = sizeof(T) * CHAR_BIT,
          typename Allocator = std::allocator<std::pair<Key, T>>>
using OrderedMap = Map<Key, T, KeyHash, KeyPredicate, TPredicate, TrieFanOut,
                       Allocator, true, false>;

template <typename Key, typename T, typename KeyHash = std::hash<Key>,
          typename KeyPredicate = std::equal_to<Key>,
          typename TPredicate = std::equal_to<T>,
          size_t TrieFanOut = sizeof(T) * CHAR_BIT,
          typename Allocator = std::allocator<std::pair<Key, T>>>
using UnorderedMap = Map<Key, T, KeyHash, KeyPredicate, TPredicate, TrieFanOut,
                         Allocator, false, false>;

template <typename Key, typename T, typename KeyHash = std::hash<Key>,
          typename KeyPredicate = std::less<Key>,
          typename TPredicate = std::equal_to<T>,
          size_t TrieFanOut = sizeof(T) * CHAR_BIT,
          typename Allocator = std::allocator<std::pair<Key, T>>>
using ConcurrentOrderedMap = Map<Key, T, KeyHash, KeyPredicate, TPredicate,
                                 TrieFanOut, Allocator, true, true>;

template <typename Key, typename T, typename KeyHash = std::hash<Key>,
          typename KeyPredicate = std::equal_to<Key>,
          typename TPredicate = std::equal_to<T>,
          size_t TrieFanOut = sizeof(T) * CHAR_BIT,
          typename Allocator = std::allocator<std::pair<Key, T>>>
using ConcurrentUnorderedMap = Map<Key, T, KeyHash, KeyPredicate, TPredicate,
                                   TrieFanOut, Allocator, false, true>;

}  // namespace persistent