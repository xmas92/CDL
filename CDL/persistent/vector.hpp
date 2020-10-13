
// Copyright (c) 2020 Axel Boldt-Christmas
//

#pragma once
#include <climits>
#include <concepts>
#include <functional>
#include <memory>

#include "chunked_array.hpp"

namespace persistent {

template <typename T, size_t ChunkSize, size_t TrieFanOut, typename Allocator,
          bool ThreadSafe>
requires(helper::IsPowerOf2(TrieFanOut) &&
         helper::IsPowerOf2(ChunkSize)) struct Vector {
 private:
  using ChunkedArray = chunked_array::ChunkedArray<T, ChunkSize, TrieFanOut,
                                                   Allocator, ThreadSafe>;

 public:
  using ContainerType = ChunkedArray;
  template <typename Allocator = Allocator>
  using TransientContainerType = void;

  ContainerType data;
}
}  // namespace persistent