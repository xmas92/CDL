
// Copyright (c) 2020 Axel Boldt-Christmas
//

#pragma once
#include <locale>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "detail/helpers.hpp"

namespace parsing::rules {

enum Tokenize : bool { Yes = true, No = false };
// Forward declared tokenized values.
struct Form;
struct BindingForm;
template <typename T>
concept Tokenizable = T::Tokenize::value || T::base::Tokenize::value ||
                      std::is_same_v<Form, T> || std::is_same_v<BindingForm, T>;
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

// special terminal tokens
enum Classification {
  Whitespace,
  Blankspace,
  Control,
  Uppercase,
  Lowercase,
  Alphabetic,
  Digit,
  Punctuation,
  HexDigit,
  Alphanumeric,
  Printable,
  Graphical,
};

namespace detail {
template <Classification classification>
constexpr bool CheckClassification(char32_t c) {
  if constexpr (parsing::rules::terminal::Whitespace == classification)
    return std::isspace(c, std::locale());
  else if constexpr (parsing::rules::terminal::Blankspace == classification)
    return std::isblank(c, std::locale());
  else if constexpr (parsing::rules::terminal::Control == classification)
    return std::iscntrl(c, std::locale());
  else if constexpr (parsing::rules::terminal::Uppercase == classification)
    return std::isupper(c, std::locale());
  else if constexpr (parsing::rules::terminal::Lowercase == classification)
    return std::islower(c, std::locale());
  else if constexpr (parsing::rules::terminal::Alphabetic == classification)
    return std::isalpha(c, std::locale());
  else if constexpr (parsing::rules::terminal::Digit == classification)
    return std::isdigit(c, std::locale());
  else if constexpr (parsing::rules::terminal::Punctuation == classification)
    return std::ispunct(c, std::locale());
  else if constexpr (parsing::rules::terminal::HexDigit == classification)
    return std::isxdigit(c, std::locale());
  else if constexpr (parsing::rules::terminal::Alphanumeric == classification)
    return std::isalnum(c, std::locale());
  else if constexpr (parsing::rules::terminal::Printable == classification)
    return std::isprint(c, std::locale());
  else if constexpr (parsing::rules::terminal::Graphical == classification)
    return std::isgraph(c, std::locale());
  else
    return false;
}
}  // namespace detail
template <Classification classification, Tokenize tokenize = Tokenize::No>
struct ClassLiteral {
  using Tokenize =
      std::conditional_t<tokenize, std::true_type, std::false_type>;
  using Token = std::conditional_t<Tokenize::value, char32_t, NoToken>;
  Token token;
  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    Input input = ti;
    if (auto maybe_literal = input.NextChar();
        maybe_literal &&
        detail::CheckClassification<classification>(*maybe_literal)) {
      if constexpr (tokenize) token = *maybe_literal;
      return input;
    }
    return std::optional<Input>();
  }
};

template <Tokenize tokenize = Tokenize::No>
struct AnyLiteral {
  using Tokenize =
      std::conditional_t<tokenize, std::true_type, std::false_type>;
  using Token = std::conditional_t<Tokenize::value, char32_t, NoToken>;
  Token token;
  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    Input input = ti;
    if (auto maybe_literal = input.NextChar(); maybe_literal) {
      if constexpr (tokenize) token = *maybe_literal;
      return input;
    }
    return std::optional<Input>();
  }
};

template <char32_t low, char32_t high, Tokenize tokenize = Tokenize::No>
requires(low <= high) struct RangeLiteral {
  using Tokenize =
      std::conditional_t<tokenize, std::true_type, std::false_type>;
  using Token = std::conditional_t<Tokenize::value, char32_t, NoToken>;
  Token token;
  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    Input input = ti;
    if (auto maybe_literal = input.NextChar();
        maybe_literal && low <= *maybe_literal && *maybe_literal <= high) {
      if constexpr (tokenize) token = *maybe_literal;
      return input;
    }
    return std::optional<Input>();
  }
};

template <Tokenize tokenize, char32_t... chars>
struct String {
  using Tokenize =
      std::conditional_t<tokenize, std::true_type, std::false_type>;
  using Token = std::conditional_t<Tokenize::value, std::u32string, NoToken>;
  Token token;
  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    Input input = ti;
    if (auto maybe_string = input.Next(sizeof...(chars));
        maybe_string && *maybe_string == GetValue()) {
      if constexpr (tokenize) token = *maybe_string;  // move?
      return input;
    }
    return std::optional<Input>();
  }

 private:
  constexpr auto GetValue() { return std::u32string({chars...}); };
};

// https://stackoverflow.com/a/30009264 explains how defer and expand work
#define EMPTY()
#define DEFER(id) id EMPTY()
#define EXPAND(...) __VA_ARGS__
#define PARSING_RULE_TERMINAL_DETAIL_STRING_NODE_SIZE 8
#define PARSING_RULE_TERMINAL_DETAIL_STRING_MAX_LEVEL 3
#define PARSING_RULE_TERMINAL_DETAIL_STRING_MAX_SIZE \
  (PARSING_RULE_TERMINAL_DETAIL_STRING_NODE_SIZE     \
   << PARSING_RULE_TERMINAL_DETAIL_STRING_MAX_LEVEL)

namespace detail {
template <char32_t...>
struct String {};
// Declaration ToString
template <Tokenize, typename>
struct ToString;
// Helpers ToString
template <Tokenize tokenize, typename String>
using ToString_t = typename ToString<tokenize, String>::type;
// Base ToString
template <Tokenize tokenize, template <char32_t...> typename String,
          char32_t... Args>
struct ToString<tokenize, String<Args...>> {
  using type = parsing::rules::terminal::String<tokenize, Args...>;
};
}  // namespace detail

#define PARSING_RULE_TERMINAL_DETAIL_STRING_GETAT(String, str, i)              \
  std::conditional_t<                                                          \
      (0##i < (sizeof(str) / sizeof(char32_t)) - 1),                           \
      String<(0##i < (sizeof(str) / sizeof(char32_t)) - 1) ? (str)[0##i] : 0>, \
      String<>>

#define PARSING_RULE_TERMINAL_DETAIL_STRING_NODE(id, String, str, i)        \
  parsing::helper::VariadicLiteralConcat_t<                                 \
      char32_t, DEFER(id)(String, str, i##0), DEFER(id)(String, str, i##1), \
      DEFER(id)(String, str, i##2), DEFER(id)(String, str, i##3),           \
      DEFER(id)(String, str, i##4), DEFER(id)(String, str, i##5),           \
      DEFER(id)(String, str, i##6), DEFER(id)(String, str, i##7)>

#define PARSING_RULE_TERMINAL_DETAIL_STRING_LEVEL1(String, str, i) \
  PARSING_RULE_TERMINAL_DETAIL_STRING_NODE(                        \
      PARSING_RULE_TERMINAL_DETAIL_STRING_GETAT, String, str, i)
#define PARSING_RULE_TERMINAL_DETAIL_STRING_LEVEL2(String, str, i) \
  PARSING_RULE_TERMINAL_DETAIL_STRING_NODE(                        \
      PARSING_RULE_TERMINAL_DETAIL_STRING_LEVEL1, String, str, i)
#define PARSING_RULE_TERMINAL_DETAIL_STRING_LEVEL3(String, str, i) \
  PARSING_RULE_TERMINAL_DETAIL_STRING_NODE(                        \
      PARSING_RULE_TERMINAL_DETAIL_STRING_LEVEL2, String, str, i)

#define PARSING_RULE_TERMINAL_DETAIL_STRING(String, str)                      \
  EXPAND(EXPAND(                                                              \
      EXPAND(std::enable_if_t<((sizeof(str) / sizeof(char32_t) - 1) <         \
                               PARSING_RULE_TERMINAL_DETAIL_STRING_MAX_SIZE), \
                              PARSING_RULE_TERMINAL_DETAIL_STRING_LEVEL3(     \
                                  String, str, )>)))

#define PARSING_RULE_TERMINAL_STRING2(str, tokenize) \
  parsing::rules::terminal::detail::ToString_t<      \
      tokenize, PARSING_RULE_TERMINAL_DETAIL_STRING( \
                    parsing::rules::terminal::detail::String, U##str)>
#define PARSING_RULE_TERMINAL_STRING1(str) \
  PARSING_RULE_TERMINAL_STRING2(str, parsing::rules::Tokenize::No)

#define PARSING_RULE_TERMINAL_DETAIL_GET_MACRO(_1, _2, NAME, ...) NAME
#define PARSING_RULE_TERMINAL_STRING(...)         \
  EXPAND(PARSING_RULE_TERMINAL_DETAIL_GET_MACRO(  \
      __VA_ARGS__, PARSING_RULE_TERMINAL_STRING2, \
      PARSING_RULE_TERMINAL_STRING1)(__VA_ARGS__))

// base terminal tokens
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
  using Token = std::vector<std::conditional_t<
      Tokenize::value,
      helper::VariadicInsert_t<
          helper::VariadicBinaryFilter_t<
              std::is_same, helper::VariadicFilteredApply_t<
                                IsTokenize, std::variant, Rules...>>,
          std::monostate, 0>,
      std::variant<std::monostate>>>;
  Token token;

  template <TranslationInput Input>
  std::optional<Input> Match(const Input& ti) {
    std::optional<Input> input = ti;
    bool success = false;
    // auto position = 0u;
    if constexpr (Tokenize::value)
      success = ((input = MatchRule<Rules>(*input)).has_value() && ...);
    else
      success = ((input = Rules().Match(*input)).has_value() && ...);
    assert(success == input.has_value());
    return input;
  }

 private:
  // Could not use tuple because of circular references making the construction
  // of the tuple not possible as the forward declarations are not
  // constructible. Maybe there is some workaround, using vector now instead.
  // using IndexSequence = std::make_index_sequence<std::tuple_size_v<Token>>;
  // template <TranslationInput Input, size_t... Is>
  // constexpr std::optional<Input> MatchRule(Input& input, size_t position,
  //                                         std::index_sequence<Is...>) {
  //  std::optional<Input> ret;
  //  ((Is == position ? ret = std::get<Is>(token).Match(input) : ret), ...);
  //  return ret;
  //}

  // template <Matchable Rule, TranslationInput Input>
  // std::optional<Input> MatchRule(Input& input, size_t position) {
  //  if constexpr (Rule::Tokenize::value) {
  //    return MatchRule(input, position, IndexSequence());
  //  } else {
  //    return Rule().Match(input);
  //  }
  //}
  template <Matchable Rule, TranslationInput Input>
  std::optional<Input> MatchRule(Input& input) {
    if constexpr (Rule::Tokenize::value) {
      Rule rule;
      auto ret = rule.Match(input);
      token.emplace_back(rule);
      return ret;
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