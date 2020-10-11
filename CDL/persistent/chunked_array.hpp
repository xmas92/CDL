
// Copyright (c) 2020 Axel Boldt-Christmas
//

/*
    TODO: Implement it :)
            * How?
                * AMT (requires fixed size for direct indexing)
            * Slicing?
                * Slicing beginning is trivial
                * Middle is hard if misaligned. Solution?
                    * Maybe allow non fixed sized chunks and/or have a global
                        offset
                    * Regardless requires some cleaver and cheap AMT compatible
                        indexing.
                    * Either direct index mapped or using binary search. Search
                        would be O(log_x n * log_2 x) == O(log_2 n)
*/

#pragma once
#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <climits>
#include <concepts>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <memory>
#include <optional>
#include <span>
#include <type_traits>
#include <utility>
#include <variant>

#include "detail/helpers.hpp"

namespace persistent {
namespace chunked_array {
using std::initializer_list;
using std::optional;
using std::variant;
namespace detail {
using std::atomic;
using std::bitset;
using std::conditional_t;
using std::invoke_result_t;
using std::shared_ptr;
template <size_t ChunkSize, size_t TrieFanOut>
struct IndexUtility {
  constexpr static auto trie_bitmask_size = helper::Log2(TrieFanOut);
  constexpr static auto chunk_bitmask_size = helper::Log2(ChunkSize);
  constexpr static auto trie_bit_size =
      ((sizeof(size_t) * CHAR_BIT) - chunk_bitmask_size);
  constexpr static auto trie_bitmask = (1 << trie_bitmask_size) - 1;
  constexpr static auto chunk_bitmask = (1 << chunk_bitmask_size) - 1;
  constexpr static auto max_depth =
      (trie_bit_size) / trie_bitmask_size +
      ((trie_bit_size % trie_bitmask_size) == 0 ? 0 : 1);
  size_t offset;
  size_t index;
  size_t level;
  IndexUtility(size_t index, size_t size, size_t offset = 0)
      : offset(offset), index(index) {
    assert(offset < ChunkSize);
    if (size <= ChunkSize)
      level = -1;
    else
      level = (trie_bit_size - (std::countl_zero(size + offset - 1) - 1)) /
              trie_bitmask_size;
  }

  constexpr size_t peak() const noexcept {
    return ((index + offset) >>
            (level * trie_bitmask_size + chunk_bitmask_size)) &
           trie_bitmask;
  }
  constexpr size_t chunk() const noexcept {
    return (index + offset) & chunk_bitmask;
  }
};
#define CHUNKED_ARRAY_TEMPLATE_TYPES                                   \
  typename T, size_t ChunkSize, size_t TrieFanOut, typename Allocator, \
      bool ThreadSafe
#define CHUNKED_ARRAY_TEMPLATE_NAMES \
  T, ChunkSize, TrieFanOut, Allocator, ThreadSafe
template <CHUNKED_ARRAY_TEMPLATE_TYPES>
class Node;

template <CHUNKED_ARRAY_TEMPLATE_TYPES>
class Leaf;

template <CHUNKED_ARRAY_TEMPLATE_TYPES>
class Leaf {
  using ContainerPointer = std::shared_ptr<T[]>;
  using Container =
      std::conditional_t<ThreadSafe, std::atomic<ContainerPointer>,
                         ContainerPointer>;

  using AllocatorType =
      typename std::allocator_traits<Allocator>::template rebind_alloc<T>;
  using AllocatorTypeTraits = std::allocator_traits<AllocatorType>;

  using NodeType = Node<CHUNKED_ARRAY_TEMPLATE_NAMES>;
  using LeafType = Leaf<CHUNKED_ARRAY_TEMPLATE_NAMES>;
  using IndexUtilityType = IndexUtility<ChunkSize, TrieFanOut>;
  using Branch = variant<NodeType, LeafType>;

  friend NodeType;

  using AllocatorTypeBranch =
      typename std::allocator_traits<Allocator>::template rebind_alloc<Branch>;
  using AllocatorTypeBranchTypeTraits =
      std::allocator_traits<AllocatorTypeBranch>;

  Container values_;
  size_t size_;

  [[no_unique_address]] const AllocatorType allocator_;
  [[no_unique_address]] const AllocatorTypeBranch branch_allocator_;

 public:
  constexpr Leaf() noexcept : size_(0) { values_ = nullptr; }
  constexpr Leaf(const Leaf& other) : size_(other.size_) {
    // Try replace with shared_ptr(other.values)
    if constexpr (ThreadSafe) {
      values_ = other.values_.load();
    } else {
      values_ = other.values_;
    }
  }
  constexpr Leaf(size_t size) : size_(size) {
    values_ = allocate_shared<T[]>(allocator_, size_);
  }
  constexpr Leaf& operator=(const Leaf& other) {
    if constexpr (ThreadSafe) {
      values_ = other.values_.load();
    } else {
      values_ = other.values_;
    }
    size_ = other.size_;
    return *this;
  }

  constexpr Leaf(Leaf&&) = default;
  constexpr Leaf& operator=(Leaf&&) = default;

  ~Leaf() = default;

  constexpr Leaf(const T& value) : size_(1) {
    values_ = allocate_shared<T[]>(allocator_, 1);
    if constexpr (ThreadSafe) {
      values_.load()[0] = value;
    } else {
      values_[0] = value;
    }
  }

  [[nodiscard]] constexpr T* GetPtr() const noexcept {
    if constexpr (ThreadSafe) {
      return values_.load().get();
    } else {
      return values_.get();
    }
  }

  [[nodiscard]] constexpr size_t size() const noexcept { return size_; }
};

template <CHUNKED_ARRAY_TEMPLATE_TYPES>
class Node {
  using NodeType = Node<CHUNKED_ARRAY_TEMPLATE_NAMES>;
  using LeafType = Leaf<CHUNKED_ARRAY_TEMPLATE_NAMES>;
  using IndexUtilityType = IndexUtility<ChunkSize, TrieFanOut>;
  using Branch = variant<NodeType, LeafType>;
  using BranchesPtr = shared_ptr<Branch[]>;
  using Branches = conditional_t<ThreadSafe, atomic<BranchesPtr>, BranchesPtr>;

  using AllocatorType =
      typename std::allocator_traits<Allocator>::template rebind_alloc<T>;
  using AllocatorTypeTraits = std::allocator_traits<AllocatorType>;

  friend LeafType;

  using BranchAllocatorType =
      typename std::allocator_traits<Allocator>::template rebind_alloc<Branch>;
  using BranchAllocatorTraits = std::allocator_traits<BranchAllocatorType>;

  Branches branches_;
  size_t size_;

  [[no_unique_address]] AllocatorType allocator_;
  [[no_unique_address]] BranchAllocatorType branch_allocator_;

 public:
  constexpr Node() noexcept : size_(0) { branches_ = nullptr; }
  constexpr Node(size_t size) : size_(size) {
    using std::forward;
    branches_ = allocate_shared<Branch[]>(branch_allocator_, size);
  }
  constexpr Node(const Node& other) {
    size_ = other.size_;
    if constexpr (ThreadSafe) {
      branches_ = other.branches_.load();
    } else {
      branches_ = other.branches_;
    }
  }
  constexpr Node& operator=(const Node& other) {
    size_ = other.size_;
    if constexpr (ThreadSafe) {
      branches_ = other.branches_.load();
    } else {
      branches_ = other.branches_;
    }
    return *this;
  }
  constexpr Node(Node&&) = default;
  constexpr Node& operator=(Node&&) = default;
  ~Node() = default;

  [[nodiscard]] constexpr Branch* GetPtr() const noexcept {
    if constexpr (ThreadSafe) {
      return branches_.load().get();
    } else {
      return branches_.get();
    }
  }

  [[nodiscard]] constexpr size_t size() const noexcept { return size_; }
};
}  // namespace detail
template <typename T, size_t ChunkSize = 32>
requires(helper::IsPowerOf2(ChunkSize)) struct ChunkedArray {};
}  // namespace chunked_array
}  // namespace persistent