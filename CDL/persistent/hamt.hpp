
// Copyright (c) 2020 Axel Boldt-Christmas
//

/*
    TODO: Refactor and reduce code duplication?
    TODO: Relax the memory order.
    TODO: Clean up interface, auxiliary functions
            * insert, find, erase like std::unordered_map
            * reverse iterator
            * Should there be non constant iterators for set and map that return
                some pseudo reference struct that updates the stored HAMT value?
    TODO: Use gsl for span and array access?
*/

#pragma once
#include <algorithm>
#include <array>
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
namespace hamt {
using std::initializer_list;
using std::optional;
using std::variant;
namespace detail {
using helper::Count;
using helper::CountLowBits;
using std::atomic;
using std::bitset;
using std::conditional_t;
using std::invoke_result_t;
using std::shared_ptr;
template <typename T, typename Hash, size_t TrieFanOut>
struct HashUtility {
  constexpr static auto bitmask_size = helper::Log2(TrieFanOut) - 1;
  constexpr static auto bitmask = (1 << bitmask_size) - 1;
  using HashValue = invoke_result_t<Hash, T>;
  constexpr static auto max_depth =
      (sizeof(HashValue) * CHAR_BIT) / bitmask_size +
      (((sizeof(HashValue) * CHAR_BIT) % bitmask_size) == 0 ? 0 : 1);

  HashValue hash;
  unsigned int shift_index;

  constexpr HashUtility(const T& value) : hash(Hash{}(value)), shift_index(0) {}

  constexpr HashValue peak() const noexcept {
    assert(shift_index < max_depth);
    return (hash >> (shift_index * bitmask_size)) & bitmask;
  }
  constexpr HashValue peakOther(const HashValue& otherHash) const noexcept {
    assert(shift_index < max_depth);
    return (otherHash >> (shift_index * bitmask_size)) & bitmask;
  }
  constexpr HashValue next() noexcept {
    assert(shift_index < max_depth);
    return (hash >> (shift_index++ * bitmask_size)) & bitmask;
  }

  constexpr std::pair<HashValue, HashValue> nextPair(
      const HashValue& otherHash) noexcept {
    assert(shift_index < max_depth);
    shift_index++;
    return {(hash >> ((shift_index - 1) * bitmask_size)) & bitmask,
            (otherHash >> ((shift_index - 1) * bitmask_size)) & bitmask};
  }
};

#define HAMT_TEMPLATE_TYPES                                              \
  typename T, typename Hash, typename EqualPredicate, size_t TrieFanOut, \
      typename Allocator, bool ThreadSafe
#define HAMT_TEMPLATE_NAMES \
  T, Hash, EqualPredicate, TrieFanOut, Allocator, ThreadSafe
template <HAMT_TEMPLATE_TYPES>
class HAMTIterator;

template <HAMT_TEMPLATE_TYPES>
class Node;

template <HAMT_TEMPLATE_TYPES>
class Leaf;

template <HAMT_TEMPLATE_TYPES>
class Leaf {
  using HashValue = std::invoke_result_t<Hash, T>;
  using ContainerPointer = std::shared_ptr<T[]>;
  using Container =
      std::conditional_t<ThreadSafe, std::atomic<ContainerPointer>,
                         ContainerPointer>;

  using AllocatorType =
      typename std::allocator_traits<Allocator>::template rebind_alloc<T>;
  using AllocatorTypeTraits = std::allocator_traits<AllocatorType>;

  using NodeType = Node<HAMT_TEMPLATE_NAMES>;
  using LeafType = Leaf<HAMT_TEMPLATE_NAMES>;
  using HashUtilityType = HashUtility<T, Hash, TrieFanOut>;
  using Branch = variant<NodeType, LeafType>;

  friend NodeType;

  using AllocatorTypeBranch =
      typename std::allocator_traits<Allocator>::template rebind_alloc<Branch>;
  using AllocatorTypeBranchTypeTraits =
      std::allocator_traits<AllocatorTypeBranch>;

  HashValue hash_;
  Container values_;
  size_t size_;

  [[no_unique_address]] const Hash hasher_;
  [[no_unique_address]] const EqualPredicate equal_predicate_;
  [[no_unique_address]] const AllocatorType allocator_;
  [[no_unique_address]] const AllocatorTypeBranch branch_allocator_;

 public:
  constexpr Leaf() noexcept : hash_(0), size_(0) { values_ = nullptr; }
  constexpr Leaf(const Leaf& other) : hash_(other.hash_), size_(other.size_) {
    // Try replace with shared_ptr(other.values)
    if constexpr (ThreadSafe) {
      values_ = other.values_.load();
    } else {
      values_ = other.values_;
    }
  }
  constexpr Leaf(HashValue hash, size_t size) : hash_(hash), size_(size) {
    values_ = allocate_shared<T[]>(allocator_, size_);
  }
  constexpr Leaf& operator=(const Leaf& other) {
    hash_ = other.hash_;
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
    hash_ = hasher_(value);
    values_ = allocate_shared<T[]>(allocator_, 1);
    if constexpr (ThreadSafe) {
      values_.load()[0] = value;
    } else {
      values_[0] = value;
    }
  }

  constexpr Leaf(const T& value, HashValue known_hash)
      : hash_(known_hash), size_(1) {
    assert(known_hash == hasher_(value));
    values_ = allocate_shared<T[]>(allocator_, 1);
    if constexpr (ThreadSafe) {
      values_.load()[0] = value;
    } else {
      values_[0] = value;
    }
  }

  constexpr optional<T> find(const T& value) const {
    using std::bind_front;
    using std::cbegin;
    using std::cend;
    using std::cref;
    using std::find_if;
    using std::span;
    span<T> value_span;
    if constexpr (ThreadSafe) {
      value_span = span<T>(values_.load().get(), size_);
    } else {
      value_span = span<T>(values_.get(), size_);
    }
    auto result = find_if(cbegin(value_span), cend(value_span),
                          bind_front(equal_predicate_, cref(value)));
    if (result != cend(value_span)) return optional(*result);
    return optional<T>();
  }

  constexpr optional<T> find(const T& value,
                             const HashUtilityType&& hash_utility) const {
    // Hash collision, search leaf.
    if (hash_utility.hash == hash_) {
      return find(value);
    }
    return optional<T>();
  }

  template <typename UnnecessaryExchangePredicate = EqualPredicate>
  constexpr optional<Branch> insert(const T& value) const {
    assert(size_ > 0 and hasher_(value) == hash_);

    // EqualPredicate found a value
    if (const auto res = find(value); res.has_value()) {
      using std::is_same_v;
      if constexpr (not is_same_v<UnnecessaryExchangePredicate,
                                  EqualPredicate>) {
        if (UnnecessaryExchangePredicate()(value, *res))
          return optional<Branch>();

        // Value needs an update, crate leaf with new value.
        using std::begin;
        using std::cbegin;
        using std::cend;
        using std::copy_if;
        using std::span;
        Leaf ret;
        ret.hash_ = hash_;
        ret.values_ = allocate_shared<T[]>(allocator_, size_);
        ret.size_ = size_;
        span<T> from_span;
        span<T> to_span;
        if constexpr (ThreadSafe) {
          auto ret_values = ret.values_.load();
          ret_values[size_ - 1] = value;
          from_span = span<T>(values_.load().get(), size_);
          to_span = span<T>(ret_values.get(), ret.size_);
        } else {
          ret.values_[size_ - 1] = value;
          from_span = span<T>(values_.get(), size_);
          to_span = span<T>(ret.values_.get(), ret.size_);
        }
        copy_if(cbegin(from_span), cend(from_span), begin(to_span),
                [&equal_predicate_, &value](auto&& v) {
                  return not equal_predicate_(value, forward<decltype(v)>(v));
                });

        return ret;
      } else {
        return optional<Branch>();
      }
    }

    using std::begin;
    using std::cbegin;
    using std::cend;
    using std::copy;
    using std::span;
    // Hash collision return newly created leaf.
    Leaf ret;
    ret.hash_ = hash_;
    ret.values_ = allocate_shared<T[]>(allocator_, size_ + 1);
    ret.size_ = size_ + 1;
    span<T> from_span;
    span<T> to_span;
    if constexpr (ThreadSafe) {
      auto ret_values = ret.values_.load();
      ret_values[size_] = value;
      from_span = span<T>(values_.load().get(), size_);
      to_span = span<T>(ret_values.get(), ret.size_);
    } else {
      ret.values_[size_] = value;
      from_span = span<T>(values_.get(), size_);
      to_span = span<T>(ret.values_.get(), ret.size_);
    }
    copy(cbegin(from_span), cend(from_span), begin(to_span));

    return ret;
  }

  template <typename UnnecessaryExchangePredicate = EqualPredicate>
  constexpr optional<Branch> insert(const T& value,
                                    HashUtilityType&& hash_utility) const {
    // Hash collision insert into self.
    if (hash_utility.hash == hash_) {
      return insert<UnnecessaryExchangePredicate>(value);
    }

    using std::forward;
    // Different hash, extend branch one level.
    return optional<Branch>(SplitLeaf<UnnecessaryExchangePredicate>(
        value, forward<HashUtilityType>(hash_utility)));
  }

  constexpr optional<Branch> erase(const T& value,
                                   const HashUtilityType&& hash_utility) const {
    using std::forward;
    if (hash_utility.hash != hash_ or not find(value).has_value()) {
      return optional<Branch>();
    }
    if (size_ == 1) {
      // Special case return empty leaf
      return Leaf();
    }
    // Return new leaf without value
    using std::begin;
    using std::cbegin;
    using std::cend;
    using std::copy_if;
    using std::span;
    Leaf ret;
    ret.hash_ = hash_;
    ret.values_ = allocate_shared<T[]>(allocator_, size_ - 1);
    ret.size_ = size_ - 1;
    span<T> from_span;
    span<T> to_span;
    if constexpr (ThreadSafe) {
      from_span = span<T>(values_.load().get(), size_);
      to_span = span<T>(ret.values_.load().get(), ret.size_);
    } else {
      from_span = span<T>(values_.get(), size_);
      to_span = span<T>(ret.values_.get(), ret.size_);
    }
    copy_if(cbegin(from_span), cend(from_span), begin(to_span),
            [eq = EqualPredicate(), &value](auto&& v) {
              return not eq(value, forward<decltype(v)>(v));
            });
    return ret;
  }

  // Not really SRP.
  constexpr NodeType Promote() const {
    if (size_ == 0) return NodeType();
    NodeType ret(bitset<TrieFanOut>().set(HashUtilityType::bitmask & hash_),
                 1u);
    assert(ret.bitmap_.test(HashUtilityType::bitmask & hash_) and
           ret.bitmap_.count() == 1);
    if constexpr (ThreadSafe) {
      ret.branches_.load()[0] = *this;
    } else {
      ret.branches_[0] = *this;
    }
    return ret;
  }

  template <typename UnnecessaryExchangePredicate>
  [[nodiscard]] constexpr NodeType SplitLeaf(
      const T& value, HashUtility<T, Hash, TrieFanOut>&& hash_utility) const {
    using std::forward;
    using std::move;

    assert(hash_utility.hash != hash_);
    const auto [bit_index, leaf_bit_index] = hash_utility.nextPair(hash_);
    NodeType ret;
    ret.bitmap_.set(bit_index).set(leaf_bit_index);
    assert(ret.bitmap_[bit_index] and ret.bitmap_[leaf_bit_index]);
    if (bit_index == leaf_bit_index) {
      auto ret_branches = allocate_shared<Branch[]>(branch_allocator_, 1);
      ret.size_ = 1;
      ret_branches[0] = *insert<UnnecessaryExchangePredicate>(
          value, forward<HashUtility<T, Hash, TrieFanOut>>(hash_utility));
      ret.branches_ = move(ret_branches);
    } else {
      auto ret_branches = allocate_shared<Branch[]>(branch_allocator_, 2);
      ret.size_ = 2;

      if (bit_index < leaf_bit_index) {
        ret_branches[0] = Leaf(value);
        ret_branches[1] = *this;
      } else {
        ret_branches[0] = *this;
        ret_branches[1] = Leaf(value);
      }
      ret.branches_ = move(ret_branches);
    }
    return ret;
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

template <HAMT_TEMPLATE_TYPES>
class Node {
  using NodeType = Node<HAMT_TEMPLATE_NAMES>;
  using LeafType = Leaf<HAMT_TEMPLATE_NAMES>;
  using HashUtilityType = HashUtility<T, Hash, TrieFanOut>;
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

  bitset<TrieFanOut> bitmap_;
  Branches branches_;
  size_t size_;

  [[no_unique_address]] AllocatorType allocator_;
  [[no_unique_address]] BranchAllocatorType branch_allocator_;

 public:
  constexpr Node() noexcept : bitmap_(0), size_(0) { branches_ = nullptr; }
  constexpr Node(const bitset<TrieFanOut>& bitmap, size_t size)
      : bitmap_(bitmap), size_(size) {
    using std::forward;
    assert(Count<TrieFanOut>(bitmap) == size);
    branches_ = allocate_shared<Branch[]>(branch_allocator_, size);
  }
  constexpr Node(const Node& other) {
    bitmap_ = other.bitmap_;
    size_ = other.size_;
    if constexpr (ThreadSafe) {
      branches_ = other.branches_.load();
    } else {
      branches_ = other.branches_;
    }
  }
  constexpr Node& operator=(const Node& other) {
    bitmap_ = other.bitmap_;
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

  constexpr optional<T> find(const T& value,
                             HashUtilityType&& hash_utility) const {
    using std::forward;
    const auto bit_index = hash_utility.next();
    assert(bit_index < bitmap_.size());

    // Hash collision, continue search.
    if constexpr (ThreadSafe) {
      if (bitmap_[bit_index]) {
        return visit(
            [&](auto&& branch) -> optional<T> {
              return branch.find(value, forward<HashUtilityType>(hash_utility));
            },
            branches_.load()[CountLowBits<TrieFanOut>(bitmap_, bit_index)]);
      }
    } else {
      if (bitmap_[bit_index]) {
        return visit(
            [&](auto&& branch) -> optional<T> {
              return branch.find(value, forward<HashUtilityType>(hash_utility));
            },
            branches_[CountLowBits<TrieFanOut>(bitmap_, bit_index)]);
      }
    }

    return optional<T>();
  }

  template <typename UnnecessaryExchangePredicate = EqualPredicate>
  constexpr optional<Branch> insert(const T& value,
                                    HashUtilityType&& hash_utility) const {
    using std::forward;
    using std::move;

    const auto bit_index = hash_utility.next();
    const auto branch_index = CountLowBits<TrieFanOut>(bitmap_, bit_index);
    assert(branch_index ==
           (bitmap_ & (bitset<TrieFanOut>().set() >> (TrieFanOut - bit_index)))
               .count());
    assert(bit_index < bitmap_.size());

    Branch new_branch;
    // Hash collision, insert in branch.
    if (bitmap_[bit_index]) {
      optional<Branch> new_subtree;
      if constexpr (ThreadSafe) {
        new_subtree = visit(
            [&](auto&& branch) -> optional<Branch> {
              return branch.template insert<UnnecessaryExchangePredicate>(
                  value, forward<HashUtilityType>(hash_utility));
            },
            branches_.load()[branch_index]);
      } else {
        new_subtree = visit(
            [&](auto&& branch) -> optional<Branch> {
              return branch.template insert<UnnecessaryExchangePredicate>(
                  value, forward<HashUtilityType>(hash_utility));
            },
            branches_[branch_index]);
      }
      if (not new_subtree.has_value())
        // value already in leaf, no change.
        return optional<Branch>();
      new_branch = move(*new_subtree);
    } else {
      new_branch.template emplace<LeafType>(value, hash_utility.hash);
    }
    const bitset<TrieFanOut> new_bitmap =
        (decltype(bitmap_)(bitmap_)).set(bit_index);
    assert(new_bitmap[bit_index] and
           new_bitmap.count() - bitmap_.count() <= 1 and
           new_bitmap.count() >= bitmap_.count());
    NodeType ret(new_bitmap, size_ + (bitmap_[bit_index] ? 0 : 1));

    using std::begin;
    using std::cbegin;
    using std::cend;
    using std::copy;
    using std::span;

    span<Branch> from_span;
    span<Branch> to_span;

    if constexpr (ThreadSafe) {
      auto ret_branches = ret.branches_.load();
      from_span = span<Branch>(branches_.load().get(), size_);
      to_span = span<Branch>(ret_branches.get(), ret.size_);
      ret_branches[branch_index] = move(new_branch);
    } else {
      from_span = span<Branch>(branches_.get(), size_);
      to_span = span<Branch>(ret.branches_.get(), ret.size_);
      ret.branches_[branch_index] = move(new_branch);
    }

    const auto lowerFromSpan = from_span.first(branch_index);
    const auto higherFromSpan =
        from_span.last(to_span.size() - branch_index - 1);

    const auto higherToSpan = to_span.last(to_span.size() - branch_index - 1);

    assert(lowerFromSpan.size() + 1 + higherFromSpan.size() == to_span.size());
    assert(higherFromSpan.size() == higherToSpan.size());
    assert(branch_index + 1 + higherToSpan.size() == to_span.size());

    copy(cbegin(lowerFromSpan), cend(lowerFromSpan), begin(to_span));
    // ret.branches[branchIndex] = move(newBranch); moved up to constexpr
    copy(cbegin(higherFromSpan), cend(higherFromSpan), begin(higherToSpan));

    return ret;
  }

  constexpr optional<Branch> erase(const T& value,
                                   HashUtilityType&& hash_utility) const {
    using std::forward;
    using std::holds_alternative;
    const auto bit_index = hash_utility.next();
    const auto branch_index = CountLowBits<TrieFanOut>(bitmap_, bit_index);
    assert(branch_index ==
           (bitmap_ & (bitset<TrieFanOut>().set() >> (TrieFanOut - bit_index)))
               .count());
    assert(bit_index < bitmap_.size());

    Branch new_branch;
    if (bitmap_[bit_index]) {
      // Found hash collision, erase in sub-tree.
      optional<Branch> new_subtree;
      if constexpr (ThreadSafe) {
        new_subtree = visit(
            [&](auto&& branch) -> optional<Branch> {
              return branch.erase(value,
                                  forward<HashUtilityType>(hash_utility));
            },
            branches_.load()[branch_index]);
      } else {
        new_subtree = visit(
            [&](auto&& branch) -> optional<Branch> {
              return branch.erase(value,
                                  forward<HashUtilityType>(hash_utility));
            },
            branches_[branch_index]);
      }
      if (not new_subtree.has_value())
        // No change in sub-tree
        return optional<Branch>();
      if (size_ == 1 and holds_alternative<LeafType>(*new_subtree)) {
        // If all we have left is one leaf we can collapse the leaf up one
        // level.
        return new_subtree;
      }
      new_branch = move(*new_subtree);
    } else {
      return optional<Branch>();
    }
    using std::get;
    if (holds_alternative<LeafType>(new_branch) and
        get<LeafType>(new_branch).size_ == 0) {
      // If the sub-tree is an empty leaf node then it means that we can
      // collapse this branch.
      if constexpr (ThreadSafe) {
        if (size_ == 2) {
          const auto& branch = branches_.load()[1 - branch_index];
          if (holds_alternative<LeafType>(branch))
            // Special case where there is only one leaf left. We collapse it up
            // one level.
            return branch;
        }
      } else {
        if (size_ == 2 and
            holds_alternative<LeafType>(branches_[1 - branch_index])) {
          // Special case where there is only one leaf left. We collapse it up
          // one level.
          return branches_[1 - branch_index];
        }
      }
      const bitset<TrieFanOut> new_bitmap =
          (decltype(bitmap_)(bitmap_)).reset(bit_index);
      assert(not new_bitmap[bit_index] and bitmap_[bit_index] and
             new_bitmap.count() == bitmap_.count() - 1);
      NodeType ret(new_bitmap, size_ - 1);

      using std::begin;
      using std::cbegin;
      using std::cend;
      using std::copy;
      using std::span;

      span<Branch> from_span;
      span<Branch> to_span;

      if constexpr (ThreadSafe) {
        from_span = span<Branch>(branches_.load().get(), size_);
        to_span = span<Branch>(ret.branches_.load().get(), ret.size_);
      } else {
        from_span = span<Branch>(branches_.get(), size_);
        to_span = span<Branch>(ret.branches_.get(), ret.size_);
      }

      const auto lower_from_span = from_span.first(branch_index);
      const auto higher_from_span =
          from_span.last(to_span.size() - branch_index);

      const auto higher_to_span = to_span.last(to_span.size() - branch_index);

      assert(lower_from_span.size() + higher_from_span.size() ==
             to_span.size());
      assert(higher_from_span.size() == higher_to_span.size());
      assert(branch_index + higher_to_span.size() == to_span.size());

      copy(cbegin(lower_from_span), cend(lower_from_span), begin(to_span));
      copy(cbegin(higher_from_span), cend(higher_from_span),
           begin(higher_to_span));

      return ret;
    }
    NodeType ret(bitmap_, size_);

    using std::begin;
    using std::cbegin;
    using std::cend;
    using std::copy;
    using std::span;

    span<Branch> from_span;
    span<Branch> to_span;

    if constexpr (ThreadSafe) {
      auto ret_branches = ret.branches_.load();
      from_span = span<Branch>(branches_.load().get(), size_);
      to_span = span<Branch>(ret_branches.get(), ret.size_);
      ret_branches[branch_index] = move(new_branch);
    } else {
      from_span = span<Branch>(branches_.get(), size_);
      to_span = span<Branch>(ret.branches_.get(), ret.size_);
      ret.branches_[branch_index] = move(new_branch);
    }

    const auto lower_from_span = from_span.first(branch_index);
    const auto higher_from_span =
        from_span.last(to_span.size() - branch_index - 1);

    const auto higher_to_span = to_span.last(to_span.size() - branch_index - 1);

    assert(lower_from_span.size() + 1 + higher_from_span.size() ==
           to_span.size());
    assert(higher_from_span.size() == higher_to_span.size());
    assert(branch_index + 1 + higher_to_span.size() == to_span.size());

    copy(cbegin(lower_from_span), cend(lower_from_span), begin(to_span));
    // ret.branches[branchIndex] = move(newBranch); move up to constexpr
    copy(cbegin(higher_from_span), cend(higher_from_span),
         begin(higher_to_span));

    return ret;
  }

  [[nodiscard]] constexpr Branch* GetPtr() const noexcept {
    if constexpr (ThreadSafe) {
      return branches_.load().get();
    } else {
      return branches_.get();
    }
  }

  [[nodiscard]] constexpr size_t size() const noexcept { return size_; }
};
template <HAMT_TEMPLATE_TYPES>
class TransientLeaf;
template <HAMT_TEMPLATE_TYPES>
class TransientNode;

using std::vector;
template <HAMT_TEMPLATE_TYPES>
class TransientLeaf {
  using HashType = invoke_result_t<Hash, T>;
  using Container = vector<T, Allocator>;

  using TransientNodeType = TransientNode<HAMT_TEMPLATE_NAMES>;
  using TransientLeafType = TransientLeaf<HAMT_TEMPLATE_NAMES>;
  using HashUtilityType = HashUtility<T, Hash, TrieFanOut>;
  using TransientBranch = variant<TransientNodeType, TransientLeafType>;

  friend TransientNodeType;

  template <typename Allocator>
  using NodeType =
      Node<T, Hash, EqualPredicate, TrieFanOut, Allocator, ThreadSafe>;
  template <typename Allocator>
  using LeafType =
      Leaf<T, Hash, EqualPredicate, TrieFanOut, Allocator, ThreadSafe>;
  template <typename Allocator>
  using Branch = variant<NodeType<Allocator>, LeafType<Allocator>>;

  HashType hash_ = 0;
  Container values_;

  using TAllocatorType =
      typename std::allocator_traits<Allocator>::template rebind_alloc<T>;
  using TAllocatorTraits = std::allocator_traits<TAllocatorType>;

  [[no_unique_address]] const Hash hasher_;
  [[no_unique_address]] const EqualPredicate equal_predicate_;

 public:
  constexpr TransientLeaf() = default;
  constexpr TransientLeaf(const T& value)
      : hash_(Hash{}(value)), values_({value}) {}
  constexpr TransientLeaf(const T& value, HashType known_hash)
      : hash_(known_hash), values_({value}) {
    assert(known_hash == Hash{}(value));
  }
  constexpr TransientLeaf(const TransientLeaf&) = default;
  constexpr TransientLeaf& operator=(const TransientLeaf& other) {
    hash_ = other.hash_;
    values_ = other.values_;
    return *this;
  }
  constexpr TransientLeaf(TransientLeaf&&) = default;
  constexpr TransientLeaf& operator=(TransientLeaf&&) = default;
  ~TransientLeaf() = default;

  constexpr bool insert(const T& value, const HashUtilityType&& hash_utility) {
    using std::begin;
    using std::bind_front;
    using std::cref;
    using std::end;
    using std::find_if;
    assert(hash_ == hash_utility.hash);
    auto result = find_if(begin(values_), end(values_),
                          bind_front(equal_predicate_, cref(value)));
    if (result == end(values_)) {
      values_.push_back(value);
      return true;
    } else {
      *result = value;
      return false;
    }
  }
  template <typename Allocator = TAllocatorType>
  constexpr Branch<Allocator> MakePersistent(
      const Allocator& allocator = Allocator()) {
    assert(values_.size() > 0);
    using std::begin;
    LeafType<Allocator> ret(hash_, values_.size());
    using std::end;
    using std::move;
    using std::span;
    span<T> to_span;
    if constexpr (ThreadSafe) {
      to_span = span<T>(ret.GetPtr(), values_.size());
    } else {
      to_span = span<T>(ret.GetPtr(), values_.size());
    }
    move(begin(values_), end(values_), begin(to_span));
    hash_ = 0;
    values_.clear();
    return ret;
  }
};

template <HAMT_TEMPLATE_TYPES>
class TransientNode {
  using TransientNodeType = TransientNode<HAMT_TEMPLATE_NAMES>;
  using TransientLeafType = TransientLeaf<HAMT_TEMPLATE_NAMES>;
  using HashUtilityType = HashUtility<T, Hash, TrieFanOut>;
  using TransientBranch = variant<TransientNodeType, TransientLeafType>;

  friend TransientLeafType;

  using TransientBranchAllocatorType = typename std::allocator_traits<
      Allocator>::template rebind_alloc<TransientBranch>;
  using TransientBranchAllocatorTraits =
      std::allocator_traits<TransientBranchAllocatorType>;

  using Branches = vector<TransientBranch, TransientBranchAllocatorType>;

  template <typename Allocator>
  using NodeType =
      Node<T, Hash, EqualPredicate, TrieFanOut, Allocator, ThreadSafe>;
  template <typename Allocator>
  using LeafType =
      Leaf<T, Hash, EqualPredicate, TrieFanOut, Allocator, ThreadSafe>;
  template <typename Allocator>
  using Branch = variant<NodeType<Allocator>, LeafType<Allocator>>;

  // TODO: Figure this out. Does not work, temp fix NodeType::GetPtr;
  // template <typename AnyT, typename AnyHash, typename AnyEqualPredicate,
  //          size_t AnyTrieFanOut, typename AnyAllocator, bool AnyThreadSafe>
  // friend class Node;

  using TAllocatorType =
      typename std::allocator_traits<Allocator>::template rebind_alloc<T>;
  using TAllocatorTraits = std::allocator_traits<TAllocatorType>;

  bitset<TrieFanOut> bitmap_;
  Branches branches_;

 public:
  constexpr TransientNode() noexcept = default;
  constexpr TransientNode(bitset<TrieFanOut> bitmap,
                          initializer_list<TransientBranch> branches)
      : bitmap_(bitmap), branches_(branches) {}
  constexpr TransientNode(const TransientNode&) = default;
  constexpr TransientNode& operator=(const TransientNode&) = default;
  constexpr TransientNode(TransientNode&&) = default;
  constexpr TransientNode& operator=(TransientNode&&) = default;
  ~TransientNode() = default;

  constexpr bool insert(const T& value, HashUtilityType&& hash_utility) {
    using std::forward;
    using std::visit;
    const auto bit_index = hash_utility.next();
    const auto branch_index = CountLowBits<TrieFanOut>(bitmap_, bit_index);
    assert(branch_index ==
           (bitmap_ & (bitset<TrieFanOut>().set() >> (TrieFanOut - bit_index)))
               .count());
    assert(bit_index < bitmap_.size());

    using std::get;
    using std::holds_alternative;
    if (bitmap_[bit_index]) {
      // Hash collision, insert in branch.
      if (holds_alternative<TransientLeafType>(branches_[branch_index]))
        if (const auto hash =
                get<TransientLeafType>(branches_[branch_index]).hash_;
            hash != hash_utility.hash) {
          // The leaf must be promoted to a node.
          branches_[branch_index] = TransientNodeType(
              decltype(bitmap_)().set(hash_utility.peakOther(hash)),
              {branches_[branch_index]});
        }
      return visit(
          [&](auto&& branch) -> bool {
            return branch.insert(value, forward<HashUtilityType>(hash_utility));
          },
          branches_[branch_index]);
    } else {
      bitmap_.set(bit_index);
      using std::cbegin;
      branches_.insert(cbegin(branches_) + branch_index,
                       TransientLeafType(value, hash_utility.hash));
      return true;
    }
  }

  template <typename Allocator = TAllocatorType>
  constexpr Branch<Allocator> MakePersistent(
      const Allocator& allocator = Allocator()) {
    if (branches_.size() == 0) return NodeType<Allocator>();
    using Branch = Branch<Allocator>;
    NodeType<Allocator> ret(bitmap_, branches_.size());
    using std::begin;
    using std::end;
    using std::span;
    using std::transform;
    span<Branch> to_span(ret.GetPtr(), branches_.size());
    transform(begin(branches_), end(branches_), begin(to_span),
              [&allocator](auto&& branch) {
                return visit(
                    [&allocator](auto&& branch) -> Branch {
                      return branch.MakePersistent(allocator);
                    },
                    branch);
              });
    bitmap_.reset();
    branches_.clear();
    return ret;
  }
};

using std::array;
template <HAMT_TEMPLATE_TYPES>
class HAMTIterator {
  using NodeType = Node<HAMT_TEMPLATE_NAMES>;
  using LeafType = Leaf<HAMT_TEMPLATE_NAMES>;
  using HashUtilityType = HashUtility<T, Hash, TrieFanOut>;
  using Branch = variant<NodeType, LeafType>;
  using IndexArray = array<size_t, HashUtilityType::max_depth + 1>;

  friend NodeType;
  friend LeafType;

  IndexArray indices_ = {};
  NodeType root_ = {};

  constexpr HAMTIterator() = default;

 public:
  using difference_type = std::ptrdiff_t;
  using value_type = std::remove_cv_t<T>;
  using const_pointer = const T*;
  using const_reference = const T&;
  using iterator_category = std::forward_iterator_tag;

  constexpr HAMTIterator(const HAMTIterator&) = default;
  ~HAMTIterator() = default;
  constexpr HAMTIterator& operator=(const HAMTIterator&) = default;
  constexpr HAMTIterator(HAMTIterator&&) noexcept = default;
  constexpr HAMTIterator& operator=(HAMTIterator&&) = default;
  constexpr HAMTIterator& operator++() {
    using std::get;
    using std::holds_alternative;
    if (indices_[0] < root_.size()) {
      IndexArray max_indices;
      max_indices.fill(0);
      size_t i = 0;
      max_indices[i] = root_.size();
      const Branch* current_branch = root_.GetPtr() + indices_[i++];
      while (holds_alternative<NodeType>(*current_branch)) {
        const auto& node = get<NodeType>(*current_branch);
        max_indices[i] = node.size();
        current_branch = node.GetPtr() + indices_[i++];
      }
      max_indices[i] = get<LeafType>(*current_branch).size();
      for (i = indices_.size() - 1; true; --i) {
        if (++indices_[i] >= max_indices[i] and i != 0) {
          // Level index exceeded size, reset to zero go up one level.
          indices_[i] = 0;
        } else {
          // Found the next index, break out.
          break;
        }
      }
    }
    return *this;
  }
  constexpr const_reference operator*() const {
    using std::get;
    using std::holds_alternative;
    assert(indices_[0] < root_.size());
    size_t i = 0;
    const Branch* current_branch = root_.GetPtr() + indices_[i++];
    while (holds_alternative<NodeType>(*current_branch)) {
      const auto& node = get<NodeType>(*current_branch);
      assert(indices_[i] < node.size());
      current_branch = node.GetPtr() + indices_[i++];
    }
    assert(indices_[i] < get<LeafType>(*current_branch).size());
    return get<LeafType>(*current_branch).GetPtr()[indices_[i]];
  }
  constexpr friend void swap(HAMTIterator& lhs, HAMTIterator& rhs) {
    using std::swap;
    swap(lhs.root_, rhs.root_);
    swap(lhs.indices_, rhs.indices_);
  }
  constexpr HAMTIterator operator++(int) {
    auto tmp(*this);
    operator++();
    return tmp;
  }
  /*
  value_type operator*() const {
    using std::get;
    using std::holds_alternative;
    assert(indices_[0] < root_.size_);
    size_t i = 0;
    Branch& current_branch = root_.GetPtr()[indices_[i++]];
    while (holds_alternative<NodeType>(current_branch)) {
      const auto& node = get<NodeType>(current_branch);
      assert(indices_[i] < node.size_);
      current_branch = node.GetPtr()[indices_[i++]];
    }
    assert(indices_[i] < get<LeafType>(current_branch).size_);
    return get<LeafType>(current_branch).GetPtr()[indices_[i]];
  }
  */
  constexpr const_pointer operator->() const {
    using std::get;
    using std::holds_alternative;
    assert(indices_[0] < root_.size());
    size_t i = 0;
    const Branch* current_branch = root_.GetPtr() + indices_[i++];
    while (holds_alternative<NodeType>(*current_branch)) {
      const auto& node = get<NodeType>(*current_branch);
      assert(indices_[i] < node.size());
      current_branch = node.GetPtr() + indices_[i++];
    }
    assert(indices_[i] < get<LeafType>(*current_branch).size());
    return get<LeafType>(*current_branch).GetPtr() + indices_[i];
  }
  constexpr friend bool operator==(const HAMTIterator& lhs,
                                   const HAMTIterator& rhs) {
    return lhs.root_.GetPtr() == rhs.root_.GetPtr() and
           lhs.indices_ == rhs.indices_;
  }
  constexpr friend bool operator!=(const HAMTIterator& lhs,
                                   const HAMTIterator& rhs) {
    return not(lhs == rhs);
  }
  /* Fix Later
  HAMTIterator& operator--();    // prefix decrement
  HAMTIterator operator--(int);  // postfix decrement
  */

  [[nodiscard]] static constexpr HAMTIterator GetBeginIterator(NodeType root) {
    HAMTIterator ret;
    ret.root_ = root;
    ret.indices_.fill(0);
    return ret;
  }
  [[nodiscard]] static constexpr HAMTIterator GetEndIterator(NodeType root) {
    HAMTIterator ret;
    ret.root_ = root;
    ret.indices_.fill(0);
    ret.indices_[0] = root.size();
    return ret;
  }
};

}  // namespace detail

template <HAMT_TEMPLATE_TYPES>
class TransientHAMT;

using std::invoke_result_t;
using std::predicate;
using std::unsigned_integral;
template <typename T, typename Hash, typename EqualPredicate, size_t TrieFanOut,
          typename Allocator, bool ThreadSafe>
requires(helper::IsPowerOf2(TrieFanOut) &&
         unsigned_integral<invoke_result_t<Hash, T>> &&
         predicate<EqualPredicate, T, T>) class HAMT {
  using NodeType = detail::Node<HAMT_TEMPLATE_NAMES>;
  using LeafType = detail::Leaf<HAMT_TEMPLATE_NAMES>;
  using HashUtilityType = detail::HashUtility<T, Hash, TrieFanOut>;

  NodeType root_;
  size_t size_ = 0;

  using TAllocatorType =
      typename std::allocator_traits<Allocator>::template rebind_alloc<T>;
  using TAllocatorTraits = std::allocator_traits<TAllocatorType>;

 public:
  template <typename Allocator = TAllocatorType>
  using TransientHAMT =
      TransientHAMT<T, Hash, EqualPredicate, TrieFanOut, Allocator, ThreadSafe>;

  using difference_type = std::ptrdiff_t;
  using value_type = std::remove_cv_t<T>;
  using size_type = size_t;
  using const_pointer = const T*;
  using const_reference = const T&;
  using const_iterator = detail::HAMTIterator<HAMT_TEMPLATE_NAMES>;

  constexpr HAMT() noexcept = default;
  template <std::input_iterator InputIter>
  constexpr HAMT(InputIter first, InputIter last) {
    *this = TransientHAMT<TAllocatorType>(first, last)
                .MakePersistent<TAllocatorType>();
  }
  constexpr HAMT(const HAMT&) = default;
  constexpr HAMT(NodeType&& node, size_t size) : root_(node), size_(size) {}
  constexpr HAMT(const NodeType& node, size_t size)
      : root_(node), size_(size) {}
  constexpr HAMT(initializer_list<T> il) {
    *this = TransientHAMT<>(il).template MakePersistent<>();
  }
  constexpr HAMT& operator=(const HAMT&) = default;
  constexpr HAMT(HAMT&&) = default;
  constexpr HAMT& operator=(HAMT&&) noexcept = default;
  ~HAMT() = default;

  [[nodiscard]] constexpr optional<T> find(const T& value) const {
    return root_.find(value, HashUtilityType(value));
  }

  [[nodiscard]] constexpr optional<T> find(T&& value) const {
    using std::forward;
    return root_.find(forward<T>(value), HashUtilityType(forward<T>(value)));
  }

  [[nodiscard]] constexpr bool contains(const T& value) const {
    return find(value).has_value();
  }

  [[nodiscard]] constexpr bool contains(T&& value) const {
    using std::forward;
    return find(forward<T>(value)).has_value();
  }

  template <typename InputIter,
            typename UnnecessaryExchangePredicate = EqualPredicate>
  [[nodiscard]] constexpr HAMT insert(InputIter first, InputIter last) {
    using std::move;
    NodeType new_root = root_;
    auto new_size = size_;
    for (; first != last; first++) {
      auto maybe_new_root =
          new_root.template insert<UnnecessaryExchangePredicate>(
              *first, HashUtilityType(*first));
      if (maybe_new_root) {
        ++new_size;
        new_root = move(get<NodeType>(*maybe_new_root));
      }
    }
    return HAMT(move(new_root), new_size);
  }

  template <typename UnnecessaryExchangePredicate = EqualPredicate>
  [[nodiscard]] constexpr HAMT insert(const T& value) const {
    using std::get;
    auto maybe_new_root = root_.template insert<UnnecessaryExchangePredicate>(
        value, HashUtilityType(value));
    if (maybe_new_root) {
      return HAMT(get<NodeType>(*maybe_new_root), size_ + 1);
    }
    return HAMT(root_, size_);
  }

  template <typename UnnecessaryExchangePredicate = EqualPredicate>
  [[nodiscard]] constexpr HAMT insert(T&& value) const {
    using std::forward;
    using std::get;
    auto maybe_new_root = root_.template insert<UnnecessaryExchangePredicate>(
        forward<T>(value), HashUtilityType(forward<T>(value)));
    if (maybe_new_root) {
      return HAMT(move(get<NodeType>(*maybe_new_root)), size_ + 1);
    }
    return HAMT(root_, size_);
  }

  [[nodiscard]] constexpr HAMT erase(const T& value) const {
    using std::get;
    using std::holds_alternative;
    auto new_root = root_.erase(value, HashUtilityType(value));
    if (new_root.has_value()) {
      if (holds_alternative<NodeType>(*new_root)) {
        return HAMT(get<NodeType>(*new_root), size_ - 1);
      }
      return HAMT(get<LeafType>(*new_root).Promote(), size_ - 1);
    }
    return *this;
  }

  [[nodiscard]] constexpr HAMT erase(T&& value) const {
    using std::forward;
    using std::get;
    using std::holds_alternative;
    using std::move;
    auto new_root =
        root_.erase(forward<T>(value), HashUtilityType(forward<T>(value)));
    if (new_root.has_value()) {
      if (holds_alternative<NodeType>(*new_root)) {
        return HAMT(move(get<NodeType>(*new_root)), size_ - 1);
      }
      return HAMT(move(get<LeafType>(*new_root).Promote()), size_ - 1);
    }
    return *this;
  }

  [[nodiscard]] constexpr size_t size() const noexcept { return size_; }

  [[nodiscard]] constexpr bool empty() const noexcept { return size_ == 0; }

  constexpr const_iterator begin() const noexcept {
    return const_iterator::GetBeginIterator(root_);
  }
  constexpr const_iterator end() const noexcept {
    return const_iterator::GetEndIterator(root_);
  }
  constexpr const_iterator cbegin() const noexcept {
    return const_iterator::GetBeginIterator(root_);
  }
  constexpr const_iterator cend() const noexcept {
    return const_iterator::GetEndIterator(root_);
  }
};

template <HAMT_TEMPLATE_TYPES>
class TransientHAMT {
  using TransientNodeType = detail::TransientNode<HAMT_TEMPLATE_NAMES>;
  using TransientLeafType = detail::TransientLeaf<HAMT_TEMPLATE_NAMES>;
  using HashUtilityType = detail::HashUtility<T, Hash, TrieFanOut>;

  TransientNodeType root_;
  size_t size_;

  template <typename Allocator>
  using NodeType =
      detail::Node<T, Hash, EqualPredicate, TrieFanOut, Allocator, ThreadSafe>;

  using TAllocatorType =
      typename std::allocator_traits<Allocator>::template rebind_alloc<T>;
  using TAllocatorTraits = std::allocator_traits<TAllocatorType>;

 public:
  template <typename Allocator = TAllocatorType>
  using HAMT = HAMT<T, Hash, EqualPredicate, TrieFanOut, Allocator, ThreadSafe>;
  constexpr TransientHAMT() = default;
  template <typename InputIter>
  constexpr TransientHAMT(InputIter first, InputIter last) {
    for (; first != last; first++) {
      insert(*first);
    }
  }
  constexpr TransientHAMT(initializer_list<T> il) {
    for (const auto& v : il) {
      insert(v);
    }
  }
  constexpr TransientHAMT(const TransientHAMT&) = default;
  constexpr TransientHAMT(initializer_list<TransientNodeType> il) {
    using std::cbegin;
    if (il.size() >= 1) {
      root_ = *cbegin(il);
    }
  }
  constexpr TransientHAMT& operator=(const TransientHAMT& other) = default;
  constexpr TransientHAMT(TransientHAMT&&) = default;
  constexpr TransientHAMT& operator=(TransientHAMT&&) = default;
  ~TransientHAMT() = default;

  template <typename InputIter>
  constexpr void insert(InputIter first, InputIter last) {
    for (; first != last; first++) {
      insert(*first);
    }
  }

  constexpr void insert(const T& value) {
    if (root_.insert(value, HashUtilityType(value))) ++size_;
  }

  constexpr void insert(T&& value) {
    if (root_.insert(forward<T>(value), HashUtilityType(forward<T>(value))))
      ++size_;
  }

  template <typename Allocator = TAllocatorType>
  [[nodiscard]] constexpr HAMT<Allocator> MakePersistent(
      const Allocator& allocator = Allocator()) {
    using std::get;
    return HAMT<Allocator>(
        get<NodeType<Allocator>>(root_.MakePersistent(allocator)), size_);
  }
};

}  // namespace hamt
}  // namespace persistent