
// Copyright (c) 2020 Axel Boldt-Christmas
//

/*
        TODO: Relax the memory order.
        TODO: Clean up interface, auxiliary functions
                        * Keep track of size?
                        * Fix the accessibility, currently everything is public.
                        * Currently just a list based stack
        TODO: Create iterators, (add STL required types)
*/

#pragma once
#include <atomic>
#include <memory>
#include <type_traits>

namespace persistent {
namespace list {
namespace details {
using std::allocate_shared;
using std::allocator;
using std::atomic;
using std::conditional_t;
using std::nullptr_t;
using std::shared_ptr;

template <typename T, typename Allocator = allocator<T>,
          bool UseAtomics = false>
struct ListNode {
  using ListNodeType = ListNode<T, Allocator, UseAtomics>;
  using ListNodePtr = shared_ptr<ListNodeType>;
  using ListNodeContainer =
      conditional_t<UseAtomics, atomic<ListNodePtr>, ListNodePtr>;

  T content;
  ListNodeContainer nextNode;

  constexpr ListNode() = default;
  constexpr ListNode(const T& value) : content(value) {}
  constexpr ListNode(const T& value, ListNodePtr rest)
      : content(value), nextNode(rest) {}
  constexpr ListNode(const ListNode&) = default;
  constexpr ListNode& operator=(const ListNode&) = default;
  constexpr ListNode(ListNode&&) = default;
  constexpr ListNode& operator=(ListNode&&) = default;
  ~ListNode() = default;

  [[nodiscard]] constexpr ListNodePtr next() { return nextNode; }
};
}  // namespace details
}  // namespace list
template <typename T, typename Allocator = std::allocator<T>,
          bool UseAtomics = true>
struct List {
  using ListNodeType = list::detail::ListNode<T, Allocator, UseAtomics>;
  using ListNodePtr = typename ListNodeType::ListNodePtr;
  using ListNodeContainer = typename ListNodeType::ListNodeContainer;
  ListNodeContainer head;

  using ListNodeAllocatorType = typename std::allocator_traits<
      Allocator>::template rebind_alloc<ListNodeType>;
  using ListNodeAllocatorTraits = std::allocator_traits<ListNodeAllocatorType>;
  [[no_unique_address]] const ListNodeAllocatorType list_node_allocator_;

  constexpr List() = default;
  constexpr List(const List&) = default;
  constexpr List& operator=(const List&) = default;
  constexpr List(List&&) = default;
  constexpr List& operator=(List&&) = default;
  ~List() = default;

  constexpr void push(const T& value) {
    head = allocate_shared<ListNodeType>(list_node_allocator_, value, head);
  }
  constexpr void pop() {
    if constexpr (UseAtomics) {
      if (auto ptr = head.load(); ptr != nullptr) {
        head = ptr->next();
      }
    } else {
      if (head != nullptr) {
        head = head->next();
      }
    }
  }
};
}  // namespace persistent
