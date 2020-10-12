
// Copyright (c) 2020 Axel Boldt-Christmas
//

/*
    TODO: Fix so locale can be specified. Currently only std::locale() works.
*/

#pragma once
#include <array>
#include <cassert>
#include <codecvt>
#include <concepts>
#include <functional>
#include <gsl/gsl>
#include <istream>
#include <limits>
#include <locale>
#include <memory>
#include <optional>
#include <string>

namespace parsing {

template <typename T>
concept IStream = std::derived_from<T, std::basic_istream<char>> ||
                  std::derived_from<T, std::basic_istream<wchar_t>> ||
                  std::derived_from<T, std::basic_istream<char8_t>> ||
                  std::derived_from<T, std::basic_istream<char16_t>> ||
                  std::derived_from<T, std::basic_istream<char32_t>>;

namespace translation_input::detail {

template <IStream InputStream>
std::u32string::difference_type Fetch(
    InputStream& input_stream, std::u32string& output_buffer,
    std::vector<typename std::u32string::difference_type>& newline_offsets,
    std::u32string::difference_type count) {
  using PromotedCharType =
      std::conditional_t<std::is_same_v<InputStream::char_type, char>, wchar_t,
                         InputStream::char_type>;
  constexpr auto eof = InputStream::traits_type::eof();
  constexpr size_t buffer_size = 64u;
  const auto buffer_initial_length = output_buffer.length();
  input_stream.clear();
  input_stream.exceptions(InputStream::badbit);
  if constexpr (std::is_same_v<InputStream::char_type, char32_t>) {
    do {
      output_buffer.resize(output_buffer.length() + buffer_size);
      const gsl::span<char32_t> buffer_span(output_buffer.data(),
                                            output_buffer.length());
      input_stream.read(buffer_span.last(buffer_size).data(), buffer_size);
      assert(input_stream.gcount() == buffer_size || input_stream.eof());
    } while (!input_stream.eof() and
             output_buffer.length() - buffer_initial_length < count);
    output_buffer.resize(output_buffer.length() -
                         (buffer_size - input_stream.gcount()));
  } else {
    auto& u32_converter = std::use_facet<
        std::codecvt<char32_t, PromotedCharType, std::mbstate_t>>(
        std::locale());
    using InputBuffer = std::array<PromotedCharType, buffer_size>;
    InputBuffer input_buffer;
    auto partial_left = 0;
    const auto do_read_convert = [&](const auto& to_read_span,
                                     const auto& input_buffer_span) {
      std::streamsize chars_read = 0;
      if constexpr (std::is_same_v<InputStream::char_type, char>) {
        // Need to widen char first or it is treated as Unicode.
        auto& char_widener = std::use_facet<std::ctype<wchar_t>>(std::locale());
        std::array<char, buffer_size> narrow_buffer;
        const gsl::span<char> narrow_buffer_span(narrow_buffer.data(),
                                                 to_read_span.size());
        input_stream.read(narrow_buffer_span.data(), narrow_buffer_span.size());
        chars_read = input_stream.gcount();
        char_widener.widen(
            narrow_buffer_span.data(),
            narrow_buffer_span.data() + narrow_buffer_span.size(),
            to_read_span.data());
        // TODO: maybe need to remove EOF chars
      } else {
        input_stream.read(to_read_span.data(), to_read_span.size());
        chars_read = input_stream.gcount();
      }
      assert(chars_read == to_read_span.size() || input_stream.eof());
      const auto new_chars = chars_read + partial_left;
      const PromotedCharType* from_ptr;
      char32_t* to_ptr;
      std::mbstate_t mb{};
      output_buffer.resize(output_buffer.length() + new_chars);
      const auto read_span = input_buffer_span.first(new_chars);
      const gsl::span<char32_t> output_buffer_span(output_buffer.data(),
                                                   output_buffer.length());
      u32_converter.in(
          mb, read_span.data(), read_span.data() + read_span.size(), from_ptr,
          output_buffer_span.last(new_chars).data(),
          output_buffer_span.data() + output_buffer_span.size(), to_ptr);
      output_buffer.resize(to_ptr - output_buffer_span.data());
      partial_left = read_span.size() - (from_ptr - read_span.data());
      const auto partial_span = read_span.last(partial_left);
      std::copy(std::cbegin(partial_span), std::cend(partial_span),
                std::begin(read_span));
    };
    do {
      const gsl::span<PromotedCharType> input_buffer_span(input_buffer);
      const gsl::span<PromotedCharType> to_read_span =
          input_buffer_span.last(buffer_size - partial_left);
      do_read_convert(to_read_span, input_buffer_span);
    } while (!input_stream.eof() and
             output_buffer.length() - buffer_initial_length < count);
    if (partial_left != 0) {
      if (input_stream.eof()) {
        // What to do here? stream ended in partial codeword.
      } else {
        const gsl::span<PromotedCharType> input_buffer_span(input_buffer);
        const gsl::span<PromotedCharType> to_read_span =
            input_buffer_span.subspan(partial_left, 1);
        do_read_convert(to_read_span, input_buffer_span);
      }
    }
  }
  const auto& char_eq = std::u32string::traits_type::eq;
  const auto nl_check = std::bind_front(char_eq, U'\n');
  const auto cr_check = std::bind_front(char_eq, U'\r');
  const auto eol_predicate = [&nl_check, &cr_check](const auto& c) {
    return nl_check(c) or cr_check(c);
  };
  const auto first = std::begin(output_buffer);
  const auto last = std::end(output_buffer);
  auto iter = first + buffer_initial_length;
  if (iter != first) --iter;  // Check if last fetch ended in newline
  iter = std::find_if(iter, last, eol_predicate);
  while (iter != last) {
    if (const auto next = iter + 1; next == last) {
      break;  // Last character read, store newline next fetch.
    } else if (eol_predicate(*next) and *next != *iter) {
      ++iter;  // A "\n\r" or "\r\n" EOL.
    }
    newline_offsets.push_back(std::distance(first, iter));
    iter = std::find_if(++iter, last, eol_predicate);
  }
  return output_buffer.length() - buffer_initial_length;
}

template <IStream InputStream, bool lazy_read>
struct TranslationInputData {};

struct TranslationInputCommonData {
  using InputBuffer = std::u32string;
  using InputBufferSpan = gsl::span<InputBuffer::value_type>;
  using InputBufferPtr = std::shared_ptr<InputBuffer>;
  using Offset = InputBuffer::difference_type;
  using Offsets = std::vector<Offset>;
  using OffsetsPtr = std::shared_ptr<Offsets>;

  InputBufferPtr input_buffer_;
  OffsetsPtr newline_offsets_;
  Offset position_;

  TranslationInputCommonData() {
    input_buffer_ = std::make_shared<InputBuffer>();
    newline_offsets_ = std::make_shared<Offsets>();
    position_ = 0;
  }
};
template <IStream InputStream>
struct TranslationInputData<InputStream, false> : TranslationInputCommonData {
  TranslationInputData([[maybe_unused]] InputStream&& /*unused*/) noexcept {}
};

template <IStream InputStream>
struct TranslationInputData<InputStream, true> : TranslationInputCommonData {
  using InputStreamPtr = std::shared_ptr<InputStream>;
  using StreamOffset = typename InputStream::off_type;

  InputStreamPtr input_stream_;

  TranslationInputData(InputStream&& input_stream)
      : TranslationInputCommonData() {
    input_stream_ =
        std::make_shared<InputStream>(std::forward<InputStream>(input_stream));
  }
};
}  // namespace translation_input::detail

template <IStream InputStream, bool lazy_read>
class TranslationInput {
  using TranslationInputData =
      translation_input::detail::TranslationInputData<InputStream, lazy_read>;
  using Offset = typename TranslationInputData::Offset;
  using InputBuffer = typename TranslationInputData::InputBuffer;
  using InputBufferSpan = typename TranslationInputData::InputBufferSpan;
  using Offsets = typename TranslationInputData::Offsets;

  TranslationInputData data_;

  constexpr InputBuffer& Buffer() noexcept { return *data_.input_buffer_; }
  constexpr Offsets& NewlineOffsets() noexcept {
    return *data_.newline_offsets_;
  }
  constexpr Offset BufferSize() noexcept { return data_.input_buffer_->size(); }
  constexpr Offset& Position() noexcept { return data_.position_; }
  constexpr bool HasMore(Offset count = 1) noexcept(not lazy_read) {
    if (Position() + count <= BufferSize()) {
      return true;
    }
    if constexpr (lazy_read) {
      const Offset missing_count = (BufferSize() - (Position() + count));
      translation_input::detail::Fetch(*data_.input_stream_, Buffer(),
                                       NewlineOffsets(), missing_count);
      if (Position() + count <= BufferSize()) {
        return true;
      }
    }
    return false;
  }

  constexpr InputBufferSpan BufferSpan() noexcept {
    return InputBufferSpan(Buffer());
  }
  constexpr InputBufferSpan PrePositionSpan() noexcept {
    return BufferSpan().first(Position());
  }
  constexpr InputBufferSpan PostBufferSpan() noexcept {
    return BufferSpan().last(BufferSize() - Position());
  }

 public:
  TranslationInput() = delete;
  TranslationInput(InputStream&& input_stream)
      : data_(std::forward<InputStream>(input_stream)) {
    if constexpr (!lazy_read) {
      translation_input::detail::Fetch(input_stream, *data_.input_buffer_,
                                       *data_.newline_offsets_,
                                       std::numeric_limits<Offset>::max());
    }
  }
  constexpr TranslationInput(const TranslationInput&) = default;
  constexpr TranslationInput(TranslationInput&&) = default;
  constexpr TranslationInput& operator=(const TranslationInput&) = default;
  constexpr TranslationInput& operator=(TranslationInput&&) = default;
  ~TranslationInput() = default;

  std::optional<std::u32string> Next(Offset count) {
    if (HasMore(count)) {
      auto next_span = PostBufferSpan().first(count);
      Position() += count;
      return std::u32string(std::begin(next_span), std::end(next_span));
    }
    return std::optional<std::u32string>();
  }

  std::optional<char32_t> NextChar() {
    if (HasMore()) {
      return Buffer()[Position()++];
    }
    return std::optional<char32_t>();
  }
  std::optional<char32_t> PeekChar() {
    if (HasMore()) {
      return Buffer()[Position()];
    }
    return std::optional<char32_t>();
  }
};

template <IStream InputStream>
using EagerTranslationInput = TranslationInput<InputStream, false>;

template <IStream InputStream>
using LazyTranslationInput = TranslationInput<InputStream, true>;

}  // namespace parsing