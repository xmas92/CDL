
// Copyright (c) 2020 Axel Boldt-Christmas
//

#pragma once

#include <cassert>
#include <concepts>
#include <type_traits>

namespace parsing::helper {

// Declaration VariadicFilteredApply
template <template <typename> typename, template <typename...> typename,
          typename...>
struct VariadicFilteredApply;
// Helpers VariadicFilteredApply
template <template <typename> typename Predicate,
          template <typename...> typename Variadic, typename... Args>
using VariadicFilteredApply_t =
    typename VariadicFilteredApply<Predicate, Variadic, Args...>::type;
// Base VariadicFilteredApply
template <template <typename> typename Predicate,
          template <typename...> typename Variadic>
struct VariadicFilteredApply<Predicate, Variadic> {
  using type = Variadic<>;
};
// General VariadicFilteredApply
template <template <typename> typename Predicate,
          template <typename...> typename Variadic, typename Arg,
          typename... Args>
struct VariadicFilteredApply<Predicate, Variadic, Arg, Args...> {
  template <typename, typename>
  struct Construct;
  template <typename... Args>
  using Construct_t = typename Construct<Args...>::type;
  template <typename First, typename... Rest>
  struct Construct<First, Variadic<Rest...>> {
    using type = Variadic<First, Rest...>;
  };
  using type = typename std::conditional_t<
      Predicate<Arg>::value,
      Construct_t<Arg, VariadicFilteredApply_t<Predicate, Variadic, Args...>>,
      VariadicFilteredApply_t<Predicate, Variadic, Args...>>;
};
// Example VariadicFilteredApply
namespace example {
template <typename... Ts>
using VariadicFilteredApplyTest =
    VariadicFilteredApply_t<std::is_integral, std::tuple, Ts...>;
static_assert(
    std::is_same_v<std::tuple<long, long>,
                   VariadicFilteredApplyTest<long, long, double, float>>);
}  // namespace example

// Declaration AnyOf
template <template <typename> typename, typename...>
struct AnyOf;
// Helpers AnyOf
template <template <typename> typename Predicate, typename... Args>
constexpr bool AnyOf_v = AnyOf<Predicate, Args...>::value;
// Base AnyOf
template <template <typename> typename Predicate>
struct AnyOf<Predicate> : std::false_type {};
// General AnyOf
template <template <typename> typename Predicate, typename Arg,
          typename... Args>
struct AnyOf<Predicate, Arg, Args...>
    : std::conditional_t<Predicate<Arg>::value, std::true_type,
                         AnyOf<Predicate, Args...>> {};

// Declaration AllOf
template <template <typename> typename, typename...>
struct AllOf;
// Helpers AllOf
template <template <typename> typename Predicate, typename... Args>
constexpr bool AllOf_v = AllOf<Predicate, Args...>::value;
// Base AllOf
template <template <typename> typename Predicate>
struct AllOf<Predicate> : std::true_type {};
// General AllOf
template <template <typename> typename Predicate, typename Arg,
          typename... Args>
struct AllOf<Predicate, Arg, Args...>
    : std::conditional_t<Predicate<Arg>::value, AllOf<Predicate, Args...>,
                         std::false_type> {};

// use std::index_sequence and std::make_index_sequence instead
// template <size_t... Is>
// struct UnsginedSequence {
//  [[nodiscard]] static constexpr size_t size() noexcept {
//    return sizeof...(Is);
//  }
//  using type = UnsginedSequence<Is...>;
//};
// template <size_t N, size_t... Is>
// struct GenerateUnsginedSequence
//    : GenerateUnsginedSequence<N - 1, N - 1, Is...> {};
// template <size_t... Is>
// struct GenerateUnsginedSequence<0, Is...> : UnsginedSequence<Is...> {};

// Declaration VariadicTransform
template <template <typename> typename, typename>
struct VariadicTransform;
// Helpers VariadicTransform
template <template <typename> typename Transform, typename Variadic>
using VariadicTransform_t =
    typename VariadicTransform<Transform, Variadic>::type;
// Base VariadicTransform
template <template <typename> typename Transform,
          template <typename...> typename Variadic, typename... Args>
struct VariadicTransform<Transform, Variadic<Args...>> {
  using type = Variadic<typename Transform<Args>::type...>;
};
// Example VariadicTransform
static_assert(std::is_same_v<
              std::variant<long*, long*, double*, float*>,
              VariadicTransform_t<std::add_pointer,
                                  std::variant<long, long, double, float>>>);

// Declaration VariadicFilter
template <template <typename> typename, typename>
struct VariadicFilter;
// Helpers VariadicFilter
template <template <typename> typename Predicate, typename Variadic>
using VariadicFilter_t = typename VariadicFilter<Predicate, Variadic>::type;
// Base VariadicFilter
template <template <typename> typename Predicate,
          template <typename...> typename Variadic>
struct VariadicFilter<Predicate, Variadic<>> {
  using type = Variadic<>;
};
// General VariadicFilter
template <template <typename> typename Predicate,
          template <typename...> typename Variadic, typename Arg,
          typename... Args>
struct VariadicFilter<Predicate, Variadic<Arg, Args...>> {
  template <typename, typename>
  struct Construct;
  template <typename... Args>
  using Construct_t = typename Construct<Args...>::type;
  template <typename First, typename... Rest>
  struct Construct<First, Variadic<Rest...>> {
    using type = Variadic<First, Rest...>;
  };
  using type = typename std::conditional_t<
      Predicate<Arg>::value,
      Construct_t<Arg, VariadicFilter_t<Predicate, Variadic<Args...>>>,
      VariadicFilter_t<Predicate, Variadic<Args...>>>;
};
// Example VariadicFilter
static_assert(
    std::is_same_v<
        std::variant<long, int, long>,
        VariadicFilter_t<std::is_integral,
                         std::variant<long, int, float, long, double, float>>>);

// Declaration VariadicBinaryFilter
template <template <typename, typename> typename, typename>
struct VariadicBinaryFilter;
// Helpers VariadicBinaryFilter
template <template <typename, typename> typename Predicate, typename Variadic>
using VariadicBinaryFilter_t =
    typename VariadicBinaryFilter<Predicate, Variadic>::type;
// Base VariadicBinaryFilter
template <template <typename, typename> typename Predicate,
          template <typename...> typename Variadic>
struct VariadicBinaryFilter<Predicate, Variadic<>> {
  using type = Variadic<>;
};
// General VariadicBinaryFilter
template <template <typename, typename> typename Predicate,
          template <typename...> typename Variadic, typename Arg,
          typename... Args>
struct VariadicBinaryFilter<Predicate, Variadic<Arg, Args...>> {
  template <typename, typename>
  struct Construct;
  template <typename... Args>
  using Construct_t = typename Construct<Args...>::type;
  template <typename First, typename... Rest>
  struct Construct<First, Variadic<Rest...>> {
    using type = Variadic<First, Rest...>;
  };
  template <typename T>
  using BindArg = Predicate<Arg, T>;
  using type = typename std::conditional_t<
      AnyOf_v<BindArg, Args...>,
      VariadicBinaryFilter_t<Predicate, Variadic<Args...>>,
      Construct_t<Arg, VariadicBinaryFilter_t<Predicate, Variadic<Args...>>>>;
};
// Example VariadicBinaryFilter
static_assert(
    std::is_same_v<
        std::variant<int, long, float>,
        VariadicBinaryFilter_t<
            std::is_same, std::variant<long, int, float, long, float, float>>>);

// Detail VariadicIndexFilter
namespace detail {
// Declaration detail::VariadicIndexFilter
template <size_t, template <size_t> typename, typename>
struct VariadicIndexFilter;
// Helpers detail::VariadicIndexFilter
template <size_t N, template <size_t> typename Predicate, typename Variadic>
using VariadicIndexFilter_t =
    typename VariadicIndexFilter<N, Predicate, Variadic>::type;
// Base detail::VariadicIndexFilter
template <size_t N, template <size_t> typename Predicate,
          template <typename...> typename Variadic>
struct VariadicIndexFilter<N, Predicate, Variadic<>> {
  using type = Variadic<>;
};
// General detail::VariadicIndexFilter
template <size_t N, template <size_t> typename Predicate,
          template <typename...> typename Variadic, typename Arg,
          typename... Args>
struct VariadicIndexFilter<N, Predicate, Variadic<Arg, Args...>> {
  template <typename, typename>
  struct Construct;
  template <typename... Args>
  using Construct_t = typename Construct<Args...>::type;
  template <typename First, typename... Rest>
  struct Construct<First, Variadic<Rest...>> {
    using type = Variadic<First, Rest...>;
  };
  using type = typename std::conditional_t<
      Predicate<N>::value,
      VariadicIndexFilter_t<N + 1, Predicate, Variadic<Args...>>,
      Construct_t<Arg,
                  VariadicIndexFilter_t<N + 1, Predicate, Variadic<Args...>>>>;
};
}  // namespace detail
// Declaration VariadicIndexFilter
template <template <size_t> typename Predicate, typename Variadic>
struct VariadicIndexFilter
    : detail::VariadicIndexFilter<0, Predicate, Variadic> {};
// Helpers VariadicIndexFilter
template <template <size_t> typename Predicate, typename Variadic>
using VariadicIndexFilter_t =
    typename VariadicIndexFilter<Predicate, Variadic>::type;
// Example VariadicIndexFilter
namespace example {
template <size_t N>
using LessThan2Predicate = std::bool_constant<(N < 2)>;
static_assert(
    std::is_same_v<std::tuple<double, float, int, long>,
                   VariadicIndexFilter_t<
                       LessThan2Predicate,
                       std::tuple<long, long, double, float, int, long>>>);
}  // namespace example

//// Declaration VariadicAddFront
// template <typename, typename>
// struct VariadicAddFront;
//// Helpers VariadicAddFront
// template <typename Variadic, typename Arg>
// using VariadicAddFront_t = typename VariadicAddFront<Variadic, Arg>::type;
//// Base VariadicFilteredApply
// template <template <typename...> typename Variadic, typename Arg,
//          typename... Args>
// struct VariadicAddFront<Variadic<Args...>, Arg> {
//  using type = Variadic<Arg, Args...>;
//};

// Declaration VariadicFromVariadic
template <typename, typename>
struct VariadicFromVariadic;
// Helpers VariadicFromVariadic
template <typename ToVariadic, typename FromVariadic>
using VariadicFromVariadic_t =
    typename VariadicFromVariadic<ToVariadic, FromVariadic>::type;
// Base VariadicFromVariadic
template <template <typename...> typename ToVariadic,
          template <typename...> typename FromVariadic, typename... Args1,
          typename... Args2>
struct VariadicFromVariadic<ToVariadic<Args1...>, FromVariadic<Args2...>> {
  using type = ToVariadic<Args1..., Args2...>;
};
// Example VariadicFromVariadic
static_assert(
    std::is_same_v<
        std::variant<int, long, float>,
        VariadicFromVariadic_t<std::variant<>, std::tuple<int, long, float>>>);
static_assert(
    std::is_same_v<std::variant<double, int, long, float>,
                   VariadicFromVariadic_t<std::variant<double>,
                                          std::tuple<int, long, float>>>);

// Declaration VariadicFromVariadicLiteral
template <typename, typename, typename>
struct VariadicFromVariadicLiteral;
// Helpers VariadicFromVariadicLiteral
template <typename Literal, typename ToVariadic, typename FromVariadic>
using VariadicFromVariadicLiteral_t =
    typename VariadicFromVariadicLiteral<Literal, ToVariadic,
                                         FromVariadic>::type;
// Base VariadicFromVariadicLiteral
template <typename Literal, template <Literal...> typename ToVariadic,
          template <Literal...> typename FromVariadic, Literal... Args1,
          Literal... Args2>
struct VariadicFromVariadicLiteral<Literal, ToVariadic<Args1...>,
                                   FromVariadic<Args2...>> {
  using type = ToVariadic<Args1..., Args2...>;
};
// Is there a way to generalize literals and typenames
// Example VariadicFromVariadicLiteral
namespace example {
template <int...>
struct Ints {};
}  // namespace example
static_assert(
    std::is_same_v<example::Ints<1, 2, 3>,
                   VariadicFromVariadicLiteral_t<int, example::Ints<>,
                                                 example::Ints<1, 2, 3>>>);
static_assert(
    std::is_same_v<example::Ints<1, 2, 3, 4>,
                   VariadicFromVariadicLiteral_t<int, example::Ints<1>,
                                                 example::Ints<2, 3, 4>>>);

// Declaration VariadicConcat
template <typename, typename...>
struct VariadicConcat;
// Helpers VariadicConcat
template <typename Variadic, typename... Rest>
using VariadicConcat_t = typename VariadicConcat<Variadic, Rest...>::type;
// Base VariadicConcat
template <template <typename...> typename Variadic, typename... Args>
struct VariadicConcat<Variadic<Args...>> {
  using type = Variadic<Args...>;
};
// General VariadicConcat
template <template <typename...> typename Variadic, typename... Rest,
          typename... Args1, typename... Args2>
struct VariadicConcat<Variadic<Args1...>, Variadic<Args2...>, Rest...> {
  using type = VariadicConcat_t<Variadic<Args1..., Args2...>, Rest...>;
};
// Example VariadicConcat
static_assert(
    std::is_same_v<
        std::variant<int, long, float, int, long, float, long, long, float>,
        VariadicConcat_t<std::variant<int, long, float>,
                         std::variant<int, long, float>,
                         std::variant<long, long, float>>>);

// Declaration VariadicLiteralConcat
template <typename, typename, typename...>
struct VariadicLiteralConcat;
// Helpers VariadicLiteralConcat
template <typename Literal, typename Variadic, typename... Rest>
using VariadicLiteralConcat_t =
    typename VariadicLiteralConcat<Literal, Variadic, Rest...>::type;
// Base VariadicLiteralConcat
template <typename Literal, template <Literal...> typename Variadic,
          Literal... Args>
struct VariadicLiteralConcat<Literal, Variadic<Args...>> {
  using type = Variadic<Args...>;
};
// General VariadicLiteralConcat
template <typename Literal, template <Literal...> typename Variadic,
          typename... Rest, Literal... Args1, Literal... Args2>
struct VariadicLiteralConcat<Literal, Variadic<Args1...>, Variadic<Args2...>,
                             Rest...> {
  using type =
      VariadicLiteralConcat_t<Literal, Variadic<Args1..., Args2...>, Rest...>;
};
// Example VariadicLiteralConcat
static_assert(
    std::is_same_v<example::Ints<1, 2, 3, 4, 5, 6, 7, 8, 9>,
                   VariadicLiteralConcat_t<int, example::Ints<1, 2, 3>,
                                           example::Ints<4, 5, 6>,
                                           example::Ints<7, 8, 9>>>);

// Declaration VariadicInsert
template <typename, typename, size_t>
struct VariadicInsert;
// Helpers VariadicInsert
template <typename Variadic, typename Arg, size_t N>
using VariadicInsert_t = typename VariadicInsert<Variadic, Arg, N>::type;
// Base VariadicInsert
template <template <typename...> typename Variadic, typename Arg, size_t N,
          typename... Args>
struct VariadicInsert<Variadic<Args...>, Arg, N> {
  static_assert(N <= sizeof...(Args));

 private:
  template <size_t I>
  using LessThanN = std::bool_constant<(I < N)>;
  template <size_t I>
  using GreaterEqN = std::bool_constant<(I >= N)>;
  using Before = VariadicIndexFilter_t<GreaterEqN, Variadic<Args...>>;
  using After = VariadicIndexFilter_t<LessThanN, Variadic<Args...>>;

 public:
  using type = VariadicConcat_t<Before, Variadic<Arg>, After>;
};
// Example VariadicInsert
static_assert(
    std::is_same_v<
        std::variant<int, long, float, double, unsigned>,
        VariadicInsert_t<std::variant<int, long, double, unsigned>, float, 2>>);

// Declaration VariadicErase
template <typename, size_t>
struct VariadicErase;
// Helpers VariadicErase
template <typename Variadic, size_t N>
using VariadicErase_t = typename VariadicErase<Variadic, N>::type;
// Base VariadicErase
template <template <typename...> typename Variadic, size_t N, typename... Args>
struct VariadicErase<Variadic<Args...>, N> {
  static_assert(N < sizeof...(Args));

 private:
  template <size_t I>
  using LessEqN = std::bool_constant<(I <= N)>;
  template <size_t I>
  using GreaterEqN = std::bool_constant<(I >= N)>;
  using Before = VariadicIndexFilter_t<GreaterEqN, Variadic<Args...>>;
  using After = VariadicIndexFilter_t<LessEqN, Variadic<Args...>>;

 public:
  using type = VariadicConcat_t<Before, After>;
};
// Example VariadicErase
static_assert(
    std::is_same_v<
        std::variant<int, long, double, unsigned>,
        VariadicErase_t<std::variant<int, long, float, double, unsigned>, 2>>);
static_assert(
    std::is_same_v<
        std::variant<int, long, float, double>,
        VariadicErase_t<std::variant<int, long, float, double, unsigned>, 4>>);
static_assert(
    std::is_same_v<
        std::variant<long, float, double, unsigned>,
        VariadicErase_t<std::variant<int, long, float, double, unsigned>, 0>>);

}  // namespace parsing::helper