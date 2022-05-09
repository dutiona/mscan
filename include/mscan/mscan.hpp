#pragma once

#include <ctre-unicode.hpp>
#include <fmt/format.h>
#include <tl/expected.hpp>

#include "mscan/casters.hpp"
#include "mscan/mscan.hpp"

#include <algorithm>
#include <cassert>
#include <charconv>
#include <cmath>
#include <concepts>
#include <iostream>
#include <limits>
#include <ranges>
#include <string>
#include <string_view>
#include <system_error>
#include <tuple>
#include <type_traits>
#include <typeinfo>
#include <utility>
#include <variant>
#include <vector>

namespace mscan
{

struct alignas(8) parse_error
{
  enum class error_type
  {
    nested_brace,
    unexpected_closing_brace,
    opening_brace_juxtaposed_to_closing_brace,
    missmatch_pattern_and_input
  };

  const std::string& what() const
  {
    return msg_;
  }

  auto type() const
  {
    return t_;
  }

  constexpr parse_error(const error_type& type, const std::string& msg)
      : msg_(msg)
      , t_(type)
  {
  }

private:
  std::string msg_;
  error_type t_;
};

namespace detail
{
using parse_result_t = std::vector<expected_variant_t>;

using expected_parse_result_t = tl::expected<parse_result_t, parse_error>;

using namespace std::literals;

struct alignas(32) scan_options
{
  std::string_view fill_chr;
  std::string_view align;
  std::string_view grouping;
  std::string_view prec;
  std::string_view type;

  constexpr explicit scan_options(std::string_view current_pattern);

private:
  std::string_view whole_;
  static constexpr auto pattern_matcher_ = ctre::match<
      "(?:([^\\{}])?(<|>|\\^))?"  // optional fill-and-align
      "(?:\\.([1-9][0-9]*))?"  // optional precision
      "(_|,)?"  // optional grouping separator
      "(s|(?:l?u?(?:b|d|o|x))|(?:l?(?:f|(?:lf))))?"  // optional type
      >;

  static constexpr auto default_fill_chr_ = " "sv;
  static constexpr auto default_align_ = "<"sv;
  static constexpr auto default_prec_ = ""sv;
  static constexpr auto default_grouping_ = ""sv;
  static constexpr auto default_type_ = "s"sv;
};

constexpr scan_options::scan_options(std::string_view current_pattern)
    : whole_(""sv)
    , fill_chr(default_fill_chr_)
    , align(default_align_)
    , prec(default_prec_)
    , grouping(default_grouping_)
    , type(default_type_)
{
  auto [whole, fill_chr_, align_, prec_, grouping_, type_] =
      pattern_matcher_(current_pattern);
  if (whole)
    whole = whole;
  if (fill_chr_ != ""sv)
    fill_chr = fill_chr_;
  if (align_ != ""sv)
    align = align_;
  if (prec_ != ""sv)
    prec = prec_;
  if (grouping_ != ""sv)
    grouping = grouping_;
  if (type_ != ""sv)
    type = type_;
}

#define CONSTEXPR_FAIL(...) __builtin_unreachable()

constexpr expected_variant_t perform_scanning_pipeline(std::string_view input,
                                                       const scan_options& opts)
{
  using namespace std::literals;

  std::string tmp;
  if (opts.grouping != ""sv)
    std::ranges::remove_copy(
        begin(input), end(input), back_inserter(tmp), opts.grouping[0]);
  else
    std::ranges::copy(begin(input), end(input), back_inserter(tmp));

  //  default, plain string, ignore precision
  if (opts.type == "s"sv)
    return tmp;

  // unsigned, int, float, double, ...
  // different cases:
  // b,d,o,x,lb,ld,lo,lx,ub,ud,uo,ux,lub,lud,luo,lux,f,dlb,ldbl
  // compute precision
  auto prec = opts.prec != ""sv ? cast_to_int<unsigned>(opts.prec)
                                : tl::unexpected {invalid_int::not_an_int};
  auto tmp_sv = std::string_view {begin(tmp), end(tmp)};

  // match type detail
  auto type_matcher =
      ctre::match<"(?:s|(?:(l)?(u)?(b|d|o|x))|(?:(l)?(f|(?:lf))))?">;
  if (auto [whole, lng1, unsgn, intt, lng2, fltt] = type_matcher(opts.type);
      whole)
  {
    auto [lng1_, unsgn_, intt_, lng2_, fltt_] = std::make_tuple(lng1.to_view(),
                                                                unsgn.to_view(),
                                                                intt.to_view(),
                                                                lng2.to_view(),
                                                                fltt.to_view());
    // long type
    if (not lng1_.empty() && lng1_[0] == 'l') {
      // long unsigned
      if (not unsgn_.empty() && unsgn_[0] == 'u') {
        switch (intt_[0]) {
          case 'b':
            return cast_to_int<long long unsigned>(tmp_sv, 2);
          case 'o':
            return cast_to_int<long long unsigned>(tmp_sv, 8);
          case 'd':
            return cast_to_int<long long unsigned>(tmp_sv, 10);
          case 'x':
            return cast_to_int<long long unsigned>(tmp_sv, 16);
          default:
            CONSTEXPR_FAIL("Type must be b, o, d or x");
        }
        // long signed
      } else {
        switch (intt_[0]) {
          case 'b':
            return cast_to_int<long long>(tmp_sv, 2);
          case 'o':
            return cast_to_int<long long>(tmp_sv, 8);
          case 'd':
            return cast_to_int<long long>(tmp_sv, 10);
          case 'x':
            return cast_to_int<long long>(tmp_sv, 16);
          default:
            CONSTEXPR_FAIL("Type must be b, o, d or x");
        }
      }
      // normal length type
    } else if (not intt_.empty()) {
      // unsigned
      if (not unsgn_.empty() && unsgn_[0] == 'u') {
        switch (intt_[0]) {
          case 'b':
            return cast_to_int<unsigned>(tmp_sv, 2);
          case 'o':
            return cast_to_int<unsigned>(tmp_sv, 8);
          case 'd':
            return cast_to_int<unsigned>(tmp_sv, 10);
          case 'x':
            return cast_to_int<unsigned>(tmp_sv, 16);
          default:
            CONSTEXPR_FAIL("Type must be b, o, d or x");
        }
        // signed
      } else {
        switch (intt_[0]) {
          case 'b':
            return cast_to_int<int>(tmp_sv, 2);
          case 'o':
            return cast_to_int<int>(tmp_sv, 8);
          case 'd':
            return cast_to_int<int>(tmp_sv, 10);
          case 'x':
            return cast_to_int<int>(tmp_sv, 16);
          default:
            CONSTEXPR_FAIL("Type must be b, o, d or x");
        }
      }
      // Floating point numbers
    } else if (not lng2_.empty() && lng2_[0] == 'l') {
      // long float -> double
      if (not fltt_.empty() && fltt_[0] == 'f') {
        return prec ? cast_to_double(tmp_sv, *prec) : cast_to_double(tmp_sv);
      }
      // long long float -> long double
      else if (fltt_.size() >= 2 && fltt_[0] == 'l' && fltt_[1] == 'f')
      {
        return prec ? cast_to_long_double(tmp_sv, *prec)
                    : cast_to_long_double(tmp_sv);
      } else {
        CONSTEXPR_FAIL("Type must be f, lf, llf");
      }
    }
    // float
    else if (not fltt_.empty() && fltt_[0] == 'f')
    {
      return prec ? cast_to_float(tmp_sv, *prec) : cast_to_float(tmp_sv);
    } else {
      CONSTEXPR_FAIL("Type must be f, lf, llf");
    }
  } else {
    CONSTEXPR_FAIL("Type must be (l?u?(b|o|d|x)) | (f|lf|llf)");
  }
}

#undef CONSTEXPR_FAIL

template<class B, class E>
constexpr auto scan_from_pattern(std::string_view pattern,
                                 char capture_until,
                                 B capturing_starting_point,
                                 E end_of_input)
{
  auto options = scan_options {pattern};
  auto current = capturing_starting_point;

  // left padding
  if (options.align == ">"sv || options.align == "^"sv)  // align right/center
  {
    while (current != end_of_input && *current == options.fill_chr[0]) {
      current++;
    }
  }

  // actual match
  auto actual_ret_beg = current;
  do {
    current++;
  } while (current != end_of_input && *current != capture_until);
  auto actual_ret_end = current;

  // right padding
  if (options.align == "<"sv || options.align == "^"sv)  // align left/center
  {
    while (current + 1 != end_of_input && *(current + 1) == options.fill_chr[0])
    {
      current++;
    }
  }

  // return detailed result
  // return std::make_tuple(
  //    actual_ret_beg, actual_ret_end, capturing_starting_point, current);
  return std::make_tuple(
      perform_scanning_pipeline(
          std::string_view {actual_ret_beg, actual_ret_end}, options),
      current);
}

template<class... ExpectedTypes>
using expected_output_t = std::tuple<select_expected_t<ExpectedTypes>...>;

template<class... ExpectedTypes, typename F, size_t... Is>
constexpr auto generate_resulting_tuple_impl(F func, std::index_sequence<Is...>)
{
  using tuple_t = std::tuple<ExpectedTypes...>;
  return std::make_tuple(
      convert_to<decltype(std::get<Is>(std::declval<tuple_t>()))>(func(Is))...);
}

template<size_t N, class... ExpectedTypes, typename F>
constexpr auto generate_resulting_tuple(F func)
{
  return generate_resulting_tuple_impl<ExpectedTypes...>(
      func, std::make_index_sequence<N> {});
}

template<class... ExpectedTypes>
constexpr expected_output_t<ExpectedTypes...> assemble_resulting_tuple(
    const parse_result_t& parse_result)
{
  constexpr std::size_t nb_matches = sizeof...(ExpectedTypes);

  assert(nb_matches == parse_result.size()
         && "Error! Size of result tuple and parsed result does not match!");

  auto expected_output = generate_resulting_tuple<nb_matches, ExpectedTypes...>(
      [&parse_result](std::size_t idx)
      {
        return parse_result[idx];  // get the resulting value from vector
      });

  return expected_output;
}

}  // namespace detail

template<class... ExpectedTypes>
using expected_result_type_t =
    tl::expected<std::tuple<detail::select_expected_t<ExpectedTypes>...>,
                 parse_error>;

template<class... ExpectedTypes>
constexpr expected_result_type_t<ExpectedTypes...> scanner(
    std::string_view pattern, std::string_view input)
{
  using namespace std::literals;

  auto ret = [&]() -> detail::expected_parse_result_t
  {
    auto parse_result = detail::parse_result_t {};
    {
      auto current_pat = begin(pattern);
      auto end_pat = end(pattern);

      auto current_in = begin(input);
      auto end_in = end(input);

      bool capture = false;

      auto get_pos = [&pattern](auto it) -> std::size_t
      { return static_cast<std::size_t>(it - begin(pattern)); };

      auto capturing_pattern_beg = current_pat;
      auto capturing_pattern_end = current_pat;
      auto capturing_input_beg = current_in;
      auto capturing_input_end = current_in;

      while (current_pat != end_pat) {
        if (*current_pat == '{') {  // start capturing
          if (capture) {
            return tl::unexpected {parse_error {
                parse_error::error_type::nested_brace,
                fmt::format("Unexpected nested '{{' in pattern at pos <{}>\n"sv,
                            get_pos(current_pat))}};

          } else {
            capture = true;
            capturing_pattern_beg = current_pat + 1;  // exclude opening {}
            capturing_input_beg = current_in;
          }
        }

        else if (*current_pat == '}')  // end capturing
        {
          if (!capture) {
            return tl::unexpected {parse_error {
                parse_error::error_type::unexpected_closing_brace,
                fmt::format("Unexpected closing '}}' in pattern at pos {}\n"sv,
                            get_pos(current_pat))}};
          } else {
            capture = false;
            capturing_pattern_end = current_pat;  // exclude closing }
          }

          auto current_pattern =
              std::string_view {capturing_pattern_beg, capturing_pattern_end};

          // get next  caracter
          auto capture_input_until = capturing_pattern_end + 1;

          if (capture_input_until >= end_pat) {  // guard against end of stream
            capturing_input_end = end_in;
          } else {
            // error case, there are two juxtaposed {}{}
            if (*capture_input_until == '{') {
              return tl::unexpected {parse_error {
                  parse_error::error_type::
                      opening_brace_juxtaposed_to_closing_brace,
                  fmt::format(
                      "Unexpected opening '{{' in pattern right next to a closing '}}' at pos {}\n "sv,
                      get_pos(capture_input_until))}};
            } else {
              // we match formatting pattern against the regex
              auto [matched_result, resume_pos] =
                  detail::scan_from_pattern(current_pattern,
                                            *capture_input_until,
                                            current_in,
                                            capturing_input_end);

              parse_result.push_back(matched_result);
              current_in = resume_pos;
            }
          }
        } else if (!capture) {
          if (*current_pat != *current_in) {
            return tl::unexpected {parse_error {
                parse_error::error_type::missmatch_pattern_and_input,
                fmt::format("Missmatch input '{}' and pattern '{}'\n"sv,
                            *current_in,
                            *current_pat)}};
          } else {
            ++current_in;
          }
        }

        ++current_pat;
      }
    }

    return parse_result;
  }();

  if (ret) {
    return assemble_resulting_tuple<ExpectedTypes...>(*ret);
  } else {
    return tl::unexpected {ret.error()};
  }
}

}  // namespace mscan
