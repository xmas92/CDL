
// Copyright (c) 2020 Axel Boldt-Christmas
//

#pragma once
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "detail/helpers.hpp"

namespace parsing::rules {

namespace detail {}

enum Tokenize : bool { Yes = true, No = false };

template <typename T>
concept Tokenizable = T::Tokenize::value;
template <typename T>
struct IsTokenize : std::false_type {};
template <Tokenizable T>
struct IsTokenize<T> : std::true_type {};
struct NoToken {};

template <typename T>
concept TranslationInput = requires(T a) {
  { a.Next(0) }
  ->std::same_as<std::optional<std::u32string>>;
  { a.NextChar() }
  ->std::same_as<std::optional<char32_t>>;
};
template <typename T>
concept Matchable = true;
// Will try an fix this later. Is there even a way to constrain class templates
// with template methods.
// requires(T a, TranslationInput i) {
//  { a.template Match<TranslationInput>(i) }
//  ->std::convertible_to<std::optional<TranslationInput>>;
//};

namespace terminal {
template <char32_t value, Tokenize tokenize = Tokenize::No>
struct Literal {
  static const char32_t Value = value;
  using Tokenize =
      std::conditional_t<tokenize, std::true_type, std::false_type>;
  using Token = std::conditional_t<Tokenize::value, char32_t, NoToken>;
  Token token;
  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    Input input = ti;
    if (auto maybe_literal = input.NextChar();
        maybe_literal && *maybe_literal == value) {
      if constexpr (tokenize) token = value;
      return input;
    }
    return std::optional<Input>();
  }
};
struct Empty {
  using Tokenize = std::false_type;
  using Token = NoToken;
  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    return ti;
  }
};
}  // namespace terminal
namespace non_terminal {

template <Matchable... Rules>
struct Seq {
  using Tokenize = helper::AnyOf<IsTokenize, Rules...>;
  using Token = std::conditional_t<
      Tokenize::value,
      helper::VariadicFilteredApply_t<IsTokenize, std::tuple, Rules...>,
      std::tuple<>>;
  Token token;

  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    std::optional<Input> input = ti;
    bool success = false;
    auto position = 0u;
    if constexpr (Tokenize::value)
      success = ((input = MatchRule<Rules>(
                      *input, (Rules::Tokenize::value ? position++ : 0)))
                     .has_value() &&
                 ...);
    else
      success = ((input = Rules().Match(*input)) && ...);
    assert(success == input.has_value());
    return input;
  }

 private:
  using IndexSequence = std::make_index_sequence<std::tuple_size_v<Token>>;
  template <TranslationInput Input, size_t... Is>
  constexpr std::optional<Input> MatchRule(Input& input, size_t position,
                                           std::index_sequence<Is...>) {
    std::optional<Input> ret;
    ((Is == position ? ret = std::get<Is>(token).Match(input) : ret), ...);
    return ret;
  }

  template <Matchable Rule, TranslationInput Input>
  std::optional<Input> MatchRule(Input& input, size_t position) {
    if constexpr (Rule::Tokenize::value) {
      return MatchRule(input, position, IndexSequence());
    } else {
      return Rule().Match(input);
    }
  }
};

template <Matchable... Rules>
struct Or {
  using Tokenize = helper::AnyOf<IsTokenize, Rules...>;
  using Token = std::conditional_t<
      Tokenize::value,
      helper::VariadicInsert_t<
          helper::VariadicBinaryFilter_t<
              std::is_same, helper::VariadicFilteredApply_t<
                                IsTokenize, std::variant, Rules...>>,
          std::monostate, 0>,
      std::variant<std::monostate>>;

  Token token;

  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    std::optional<Input> ret;
    bool success = ((ret = MatchRule<Rules>(ti)) || ...);
    assert(success == ret.has_value());
    return ret;
  }

 private:
  template <Matchable Rule, TranslationInput Input>
  std::optional<Input> MatchRule(const Input& ti) {
    if constexpr (Rule::Tokenize::value) {
      Rule rule;
      auto ret = rule.Match(ti);
      if (ret) token = rule;
      return ret;
    } else
      return Rule().Match(ti);
  }
};
template <Matchable... Rules>
struct Star {
  using Tokenize = helper::AnyOf<IsTokenize, Rules...>;
  using Token = std::vector<typename Seq<Rules...>::Token>;

  Token token;

  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    std::optional<Input> ret = ti;
    Seq<Rules...> seq{};
    for (auto maybe_ret = seq.Match(*ret); maybe_ret;
         maybe_ret = seq.Match(*ret)) {
      ret = maybe_ret;
      if constexpr (Tokenize::value) token.push_back(seq.token);
      seq = Seq<Rules...>();
    }
    return ret;
  }
};
template <Matchable... Rules>
struct Plus {
  using Tokenize = helper::AnyOf<IsTokenize, Rules...>;
  using Token = std::vector<typename Seq<Rules...>::Token>;

  Token token;

  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    std::optional<Input> ret = ti;
    if (Seq<Rules...> seq; ret = seq.Match(*ret)) {
      if constexpr (Tokenize::value) token.push_back(seq.token);
      seq = Seq<Rules...>();
      for (auto maybe_ret = seq.Match(*ret); maybe_ret;
           maybe_ret = seq.Match(*ret)) {
        ret = maybe_ret;
        if constexpr (Tokenize::value) token.push_back(seq.token);
        seq = Seq<Rules...>();
      }
    }
    return ret;
  }
};
template <Matchable... Rules>
struct Opt {
  using Tokenize = helper::AnyOf<IsTokenize, Rules...>;
  using Token = std::optional<typename Seq<Rules...>::Token>;

  Token token;

  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    std::optional<Input> ret;
    if (Seq<Rules...> seq; ret = seq.Match(ti)) {
      if constexpr (Tokenize::value) token = seq.token;
      return ret;
    }
    return ti;
  }
};
template <Matchable... Rules>
struct And {
  using Tokenize = std::false_type;
  using Token = NoToken;

  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    std::optional<Input> ret = ti;
    if (ret = Seq<Rules...>{}.Match(*ret)) {
      return ti;
    }
    return ret;
  }
};
template <Matchable... Rules>
struct Not {
  using Tokenize = std::false_type;
  using Token = NoToken;

  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    std::optional<Input> ret = ti;
    if (Seq<Rules...>{}.Match(*ret)) {
      return std::optional<Input>();
    }
    return ti;
  }
};
}  // namespace non_terminal
}  // namespace parsing::rules