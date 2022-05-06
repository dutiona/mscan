#include <ctre-unicode.hpp>
#include <fmt/format.h>
#include <tl/expected.hpp>

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

template<class Ret = long long>
constexpr Ret pow10(int n)
{
  long long result = 1;
  for (int i = 1; i <= n; ++i)
    result *= 10;
  return static_cast<Ret>(result);
}

using namespace std::literals;

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

enum class invalid_int
{
  not_an_int,
  out_of_range,
  unknown_error
};

template<class Int>
using expected_int_t = tl::expected<Int, invalid_int>;

enum class invalid_float
{
  not_a_float,
  out_of_range,
  unknown_error
};

using expected_float_t = tl::expected<float, invalid_float>;

enum class invalid_double
{
  not_a_double,
  out_of_range,
  unknown_error
};

using expected_double_t = tl::expected<double, invalid_double>;

enum class invalid_long_double
{
  not_a_long_double,
  out_of_range,
  unknown_error
};

using expected_long_double_t = tl::expected<long double, invalid_long_double>;

enum class invalid_string
{
  not_a_string,
  out_of_range,
  value_too_large,
  unknown_error
};

using expected_string_t = tl::expected<std::string, invalid_string>;

using expected_variant_t = std::variant<expected_string_t,
                                        expected_long_double_t,
                                        expected_double_t,
                                        expected_float_t,
                                        expected_int_t<int>,
                                        expected_int_t<long long>,
                                        expected_int_t<unsigned>,
                                        expected_int_t<long long unsigned>>;

using parse_result_t = std::vector<expected_variant_t>;

using expected_parse_result_t = tl::expected<parse_result_t, parse_error>;

// format mini-language
/*
  fill-and-align(optional) .precision(optional) grouping-separator(optional)
  type(optional)

  fill-and-align is an optional fill character (which can be any character other
  than { or }), followed by one of the align options <,
  >, ^. The meaning of align options is as follows:

  <: Forces the field to be aligned to the start of the available space. This is
     the default when a non-integer non-floating-point presentation type is
     used.
  >: Forces the field to be aligned to the end of the available space. This is
     the default when an integer or floating-point presentation type is used.
  ^: Forces the field to be centered within the available space by inserting
     ⌊n/2] characters before and ⌈n/2] characters after the value, where n is
     the total number of fill characters to insert.

  Negative zero is treated as a negative number.
  The sign option applies to floating-point infinity and NaN.

  precision is a dot (.) followed by either a non-negative decimal number or a
  nested replacement field. This field indicates the precision or maximum field
  size. It can only be used with floating-point and string types. For
  floating-point types, this field specifies the formatting precision. For
  string types, it provides an upper bound for the estimated width (see below)
  of the prefix of the string to be copied to the output. For a string in a
  Unicode encoding, the text to be copied to the output is the longest prefix of
  whole extended grapheme clusters whose estimated width is no greater than the
  precision.

  The type option determines how the data should be presented.

  The available string presentation types are:
    none, s: Copies the string to the output.

  The available integer presentation types for integral types other
  than char, wchar_t, and bool are:

 b, B: Binary format. Pass base=2 to underlying from_char.
 d, D: Decimal format. Pass base=10 to underlying from_char.
 f, F: Floating point format. Call from_char into a float.
 o, O: Octal format. Produces the output as if by calling std::to_chars(first,
       last, value, 8). The base prefix is 0 if the corresponding argument value
       is nonzero and is empty otherwise.
 x, X: Hex format. Produces the output as if by calling std::to_chars(first,
       last, value, 16). The base prefix is 0x. X: same as x, except that it
       uses uppercase letters for digits above 9 and the base prefix is 0X.
 none: same as s.
  The available bool presentation types are:



  For lower-case presentation types, infinity and NaN are formatted as
  inf and nan, respectively. For upper-case presentation types,
  infinity and NaN are formatted as INF and NAN, respectively.
*/
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
      "(s|(?:l?u?(?:b|d|o|x))|(?:f|(?:l?dbl)))?"  // optional type
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

template<class Int, class From>
requires std::is_same_v<From, std::string_view>  //
constexpr expected_int_t<Int> cast_to_int(const From& input, int base = 10)
{
  Int result;
  auto [ptr, err] =
      std::from_chars(input.data(), input.data() + input.size(), result, base);
  if (err == std::errc())  // OK
    return result;
  else {  // KO
    if (err == std::errc::invalid_argument)  // not a number
      return tl::unexpected {invalid_int::not_an_int};
    else if (err == std::errc::result_out_of_range)
      return tl::unexpected {invalid_int::out_of_range};
    else
      return tl::unexpected {invalid_int::unknown_error};
  }
}

template<class Int, class From>
requires std::is_floating_point_v<std::remove_cvref_t<From>>  //
constexpr expected_int_t<Int> cast_to_int(From&& input)
{
  return static_cast<Int>(std::forward<From>(input));
}

template<class Int, class From>
requires std::is_integral_v<std::remove_cvref_t<From>>  //
constexpr expected_int_t<Int> cast_to_int(From&& input)
{
  return static_cast<Int>(std::forward<From>(input));
}

template<class From>
requires std::is_same_v<std::remove_cvref_t<From>, std::string_view>  //
constexpr expected_float_t cast_to_float(
    const From& input, unsigned prec = std::numeric_limits<float>::digits10)
{
  float result;
  auto mult = pow10<float>(prec);
  auto [ptr, err] =
      std::from_chars(input.data(), input.data() + input.size(), result);
  if (err == std::errc())  // OK
    return std::round(result * mult) / mult;
  else {  // KO
    if (err == std::errc::invalid_argument)  // not a number
      return tl::unexpected {invalid_float::not_a_float};
    else if (err == std::errc::result_out_of_range)
      return tl::unexpected {invalid_float::out_of_range};
    else
      return tl::unexpected {invalid_float::unknown_error};
  }
}

template<class From>
requires std::is_integral_v<std::remove_cvref_t<From>>  //
constexpr expected_float_t cast_to_float(
    From&& input, unsigned = std::numeric_limits<float>::digits10)
{
  return static_cast<float>(std::forward<From>(input));
}

template<class From>
requires std::is_floating_point_v<std::remove_cvref_t<From>>  //
constexpr expected_float_t cast_to_float(
    From&& input, unsigned prec = std::numeric_limits<float>::digits10)
{
  auto mult = pow10<float>(prec);
  return static_cast<float>(std::round(std::forward<From>(input) * mult)
                            / mult);
}

template<class From>
requires std::is_same_v<std::remove_cvref_t<From>, std::string_view>  //
constexpr expected_double_t cast_to_double(
    const From& input, unsigned prec = std::numeric_limits<double>::digits10)
{
  double result;
  auto mult = pow10<double>(prec);
  auto [ptr, err] =
      std::from_chars(input.data(), input.data() + input.size(), result);
  if (err == std::errc())  // OK
    return std::round(result * mult) / mult;
  else {  // KO
    if (err == std::errc::invalid_argument)  // not a number
      return tl::unexpected {invalid_double::not_a_double};
    else if (err == std::errc::result_out_of_range)
      return tl::unexpected {invalid_double::out_of_range};
    else
      return tl::unexpected {invalid_double::unknown_error};
  }
}

template<class From>
requires std::is_integral_v<std::remove_cvref_t<From>>  //
constexpr expected_double_t cast_to_double(
    From&& input, unsigned = std::numeric_limits<double>::digits10)
{
  return static_cast<double>(std::forward<From>(input));
}

template<class From>
requires std::is_floating_point_v<std::remove_cvref_t<From>>  //
constexpr expected_float_t cast_to_double(
    From&& input, unsigned prec = std::numeric_limits<float>::digits10)
{
  auto mult = pow10<double>(prec);
  return static_cast<double>(std::round(std::forward<From>(input) * mult)
                             / mult);
}

template<class From>
requires std::is_same_v<std::remove_cvref_t<From>, std::string_view>  //
constexpr expected_long_double_t cast_to_long_double(
    const From& input,
    unsigned prec = std::numeric_limits<long double>::digits10)
{
  long double result;
  auto mult = pow10<long double>(prec);
  auto [ptr, err] =
      std::from_chars(input.data(), input.data() + input.size(), result);
  if (err == std::errc())  // OK
    return std::round(result * mult) / mult;
  else {  // KO
    if (err == std::errc::invalid_argument)  // not a number
      return tl::unexpected {invalid_long_double::not_a_long_double};
    else if (err == std::errc::result_out_of_range)
      return tl::unexpected {invalid_long_double::out_of_range};
    else
      return tl::unexpected {invalid_long_double::unknown_error};
  }
}

template<class From>
requires std::is_integral_v<std::remove_cvref_t<From>>  //
constexpr expected_long_double_t cast_to_long_double(
    From&& input, unsigned = std::numeric_limits<long double>::digits10)
{
  return static_cast<long double>(std::forward<From>(input));
}

template<class From>
requires std::is_floating_point_v<std::remove_cvref_t<From>>  //
constexpr expected_long_double_t cast_to_long_double(
    From&& input, unsigned prec = std::numeric_limits<long double>::digits10)
{
  auto mult = pow10<long double>(prec);
  return static_cast<long double>(std::round(std::forward<From>(input) * mult)
                                  / mult);
}

constexpr expected_variant_t perform_scanning_pipeline(std::string_view input,
                                                       const scan_options& opts)
{
  std::string tmp;
  if (opts.grouping != ""sv)
    std::ranges::remove_copy(
        begin(input), end(input), back_inserter(tmp), opts.grouping[0]);
  else
    std::ranges::copy(begin(input), end(input), back_inserter(tmp));

  //  default, plain string, ignore precision
  if (opts.type == "s"sv) {
    return tmp;
  } else {  // unsigned, int, float, double, ...
    // different cases:
    // b,d,o,x,lb,ld,lo,lx,ub,ud,uo,ux,lub,lud,luo,lux,f,dlb,ldbl
    // compute precision
    auto prec = opts.prec != ""sv ? cast_to_int<unsigned>(opts.prec)
                                  : tl::unexpected {invalid_int::not_an_int};
    auto tmp_sv = std::string_view {begin(tmp), end(tmp)};
    // integers, ignore precision
    if (opts.type == "b") {
      return cast_to_int<int>(tmp_sv, 2);
    } else if (opts.type == "d"sv) {
      return cast_to_int<int>(tmp_sv, 10);
    } else if (opts.type == "o"sv) {
      return cast_to_int<int>(tmp_sv, 8);
    } else if (opts.type == "x"sv) {
      return cast_to_int<int>(tmp_sv, 16);
    } else if (opts.type == "lb"sv) {
      return cast_to_int<long long>(tmp_sv, 2);
    } else if (opts.type == "ld"sv) {
      return cast_to_int<long long>(tmp_sv, 10);
    } else if (opts.type == "lo"sv) {
      return cast_to_int<long long>(tmp_sv, 8);
    } else if (opts.type == "lx"sv) {
      return cast_to_int<long long>(tmp_sv, 16);
    } else if (opts.type == "ub"sv) {
      return cast_to_int<unsigned>(tmp_sv, 2);
    } else if (opts.type == "ud"sv) {
      return cast_to_int<unsigned>(tmp_sv, 10);
    } else if (opts.type == "uo"sv) {
      return cast_to_int<unsigned>(tmp_sv, 8);
    } else if (opts.type == "ux"sv) {
      return cast_to_int<unsigned>(tmp_sv, 16);
    } else if (opts.type == "lub"sv) {
      return cast_to_int<long long unsigned>(tmp_sv, 2);
    } else if (opts.type == "lud"sv) {
      return cast_to_int<long long unsigned>(tmp_sv, 10);
    } else if (opts.type == "luo"sv) {
      return cast_to_int<long long unsigned>(tmp_sv, 8);
    } else if (opts.type == "lux"sv) {
      return cast_to_int<long long unsigned>(tmp_sv, 16);
    } else {
      if (prec) {
        if (opts.type == "f"sv) {
          return cast_to_float(tmp_sv, *prec);
        } else if (opts.type == "dbl"sv) {
          return cast_to_double(tmp_sv, *prec);
        } else if (opts.type == "ldbl"sv) {
          return cast_to_long_double(tmp_sv, *prec);
        } else {
          return tmp;
        }
      } else {
        if (opts.type == "f"sv) {
          return cast_to_float(tmp_sv);
        } else if (opts.type == "dbl"sv) {
          return cast_to_double(tmp_sv);
        } else if (opts.type == "ldbl"sv) {
          return cast_to_long_double(tmp_sv);
        } else {
          return tmp;
        }
      }
    }
  }
}

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

template<typename T>
inline constexpr bool always_false_v = false;

template<class T>
struct select_expected
{
  static_assert(always_false_v<T>,
                "Supported scanned types are only <int>, <float>, <double> and "
                "<std::string>.");
  using type = void;
};

template<>
struct select_expected<int>
{
  using type = expected_int_t<int>;
};

template<>
struct select_expected<float>
{
  using type = expected_float_t;
};

template<>
struct select_expected<double>
{
  using type = expected_double_t;
};

template<>
struct select_expected<std::string>
{
  using type = expected_string_t;
};

template<class T>
using select_expected_t = typename select_expected<T>::type;

template<class To>
requires std::is_integral_v<std::remove_cvref_t<To>>  //
constexpr expected_variant_t cast_to(auto val)
{
  using expected_t = std::remove_cvref_t<decltype(val)>;
  if constexpr (std::is_same_v<std::string, typename expected_t::value_type>) {
    auto sv = std::string_view {val->begin(), val->end()};
    return cast_to_int<std::remove_cvref_t<To>>(sv);
  } else {
    return cast_to_int<std::remove_cvref_t<To>>(*val);
  }
}

template<class To>
requires std::is_floating_point_v<std::remove_cvref_t<To>>  //
    && std::is_same_v<float, std::remove_cvref_t<To>>  //
constexpr expected_variant_t cast_to(auto val)
{
  using expected_t = std::remove_cvref_t<decltype(val)>;
  if constexpr (std::is_same_v<std::string, typename expected_t::value_type>) {
    auto sv = std::string_view {val->begin(), val->end()};
    return cast_to_float(sv);
  } else {
    return cast_to_float(*val);
  }
}

template<class To>
requires std::is_floating_point_v<std::remove_cvref_t<To>>  //
    && std::is_same_v<double, std::remove_cvref_t<To>>  //
constexpr expected_variant_t cast_to(auto val)
{
  using expected_t = std::remove_cvref_t<decltype(val)>;
  if constexpr (std::is_same_v<std::string, typename expected_t::value_type>) {
    auto sv = std::string_view {val->begin(), val->end()};
    return cast_to_double(sv);
  } else {
    return cast_to_double(*val);
  }
}

template<class To>
requires std::is_floating_point_v<std::remove_cvref_t<To>>  //
    && std::is_same_v<long double, std::remove_cvref_t<To>>  //
constexpr expected_variant_t cast_to(auto val)
{
  using expected_t = std::remove_cvref_t<decltype(val)>;
  if constexpr (std::is_same_v<std::string, typename expected_t::value_type>) {
    auto sv = std::string_view {val->begin(), val->end()};
    return cast_to_long_double(sv);
  } else {
    return cast_to_long_double(*val);
  }
}

template<class To>
requires std::is_same_v<std::string, std::remove_cvref_t<To>>  //
constexpr expected_variant_t cast_to(auto val)
{
  using expected_t = std::remove_cvref_t<decltype(val)>;
  if constexpr (std::is_floating_point_v<typename expected_t::value_type>  //
                || std::is_integral_v<typename expected_t::value_type>)
  {
    auto digits =
        std::string(std::numeric_limits<long double>::digits10 + 2, '\0');
    auto [ptr, err] =
        std::to_chars(digits.data(), digits.data() + digits.size(), *val);
    if (err == std::errc()) {
      return expected_string_t {digits};
    } else {
      if (err == std::errc::invalid_argument)  // not a number
        return tl::unexpected {invalid_string::not_a_string};
      else if (err == std::errc::result_out_of_range)
        return tl::unexpected {invalid_string::out_of_range};
      else if (err == std::errc::value_too_large)
        return tl::unexpected {invalid_string::value_too_large};
      else
        return tl::unexpected {invalid_string::unknown_error};
    }
  } else {
    return expected_string_t {val};
  }
}

template<class To>
constexpr auto cast_var_to(auto var)
{
  return std::visit([](auto v) { return cast_to<To>(v); }, var);
}

template<class To>
requires std::is_integral_v<std::remove_cvref_t<To>>  //
    || std::is_floating_point_v<std::remove_cvref_t<To>>  //
    || std::is_same_v<std::remove_cvref_t<To>, std::string>
constexpr auto convert_to(auto var)
{
  if (!std::holds_alternative<select_expected_t<std::remove_cvref_t<To>>>(var))
  {
    auto ret = cast_var_to<std::remove_cvref_t<To>>(var);
    return std::get<select_expected_t<std::remove_cvref_t<To>>>(ret);
  } else {
    return std::get<select_expected_t<std::remove_cvref_t<To>>>(var);
  }
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

template<class... ExpectedTypes>
using expected_result_type_t =
    tl::expected<std::tuple<select_expected_t<ExpectedTypes>...>, parse_error>;

template<class... ExpectedTypes>
constexpr expected_result_type_t<ExpectedTypes...> parse_input_from_pattern(
    std::string_view pattern, std::string_view input)
{
  auto ret = [&]() -> expected_parse_result_t
  {
    auto parse_result = parse_result_t {};
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
                  scan_from_pattern(current_pattern,
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

template<class... ExpectedTypes>
auto match_pattern(std::string_view pattern, std::string_view input)
{
  using std::ranges::begin;
  using std::ranges::end;

  // Debug
  std::cout << fmt::format("PATTERN: {} \nINPUT: {}\n", pattern, input);

  auto ret = parse_input_from_pattern<ExpectedTypes...>(pattern, input);
  if (ret) {
    std::cout << fmt::format("Expected idx=0 {}\n", *std::get<0>(*ret));
    std::cout << fmt::format("Expected idx=1 {}\n", *std::get<1>(*ret));
    std::cout << fmt::format("Expected idx=2 {}\n", *std::get<2>(*ret));
  }

  return true;
}

int main()
{
  constexpr std::string_view input =
      "14 thus  10.54321666    gives      1001."sv;
  constexpr std::string_view pattern = "{} thus {^.5f} gives {>}."sv;

  [[maybe_unused]] auto ret =
      match_pattern<std::string, float, std::string>(pattern, input);

  return 0;
}
