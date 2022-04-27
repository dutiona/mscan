#include <ctre-unicode.hpp>
#include <fmt/format.h>
#include <tl/expected.hpp>

#include "mscan/mscan.hpp"

#include <algorithm>
#include <cassert>
#include <charconv>
#include <concepts>
#include <iostream>
#include <ranges>
#include <string>
#include <string_view>
#include <system_error>
#include <tuple>
#include <utility>
#include <variant>

using namespace std::literals;

struct parse_error
{
  enum class error_type
  {
    nestedBrace,
    unexpectedClosingBrace,
    openingBraceJuxtaposedToClosingBrace,
    missmatchPatternAndInput
  };

  const std::string& what() const
  {
    return msg_;
  }

  auto type() const
  {
    return t_;
  }

  parse_error(const error_type& type, const std::string& msg)
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
  NotAnInt,
  OutOfRange,
  UnknownError
};

template<class Int>
using expected_int_t = tl::expected<Int, invalid_int>;

enum class invalid_float
{
  NotAFloat,
  OutOfRange,
  UnknownError
};

using expected_float_t = tl::expected<float, invalid_float>;

enum class invalid_double
{
  NotADouble,
  OutOfRange,
  UnknownError
};

using expected_double_t = tl::expected<double, invalid_double>;

enum class invalid_long_double
{
  NotALongDouble,
  OutOfRange,
  UnknownError
};

using expected_long_double_t = tl::expected<long double, invalid_long_double>;

enum class invalid_string
{
  NotAString,
  OutOfRange,
  UnknownError
};

using expected_string_t = tl::expected<std::string, invalid_string>;

using parse_result_t = std::vector<std::string_view
/*std::variant<expected_int_t,
                                                expected_float_t,
                                                expected_double_t,
                                                expected_string_t>*/>;

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
struct scan_options
{
  std::string_view fill_chr;
  std::string_view align;
  std::string_view grouping;
  std::string_view prec;
  std::string_view type;

  constexpr scan_options(std::string_view current_pattern);

private:
  std::string_view whole_;
  static constexpr auto pattern_matcher_ = ctre::match<
      "(?:([^\\{}])?(<|>|^))?"  // optional fill-and-align
      "(?:\\.([1-9][0-9]*))?"  // optional precision
      "(_|,)?"  // optional grouping separator
      "((?:l?u?(?:b|d|o|x))|(?:f|dbl|ldbl)|(?:s))?"  // optional type
      >;

  static constexpr auto default_fill_chr_ = " "sv;
  static constexpr auto default_align_ = "<"sv;
  static constexpr auto default_prec_ = ""sv;
  static constexpr auto default_grouping_ = ""sv;
  static constexpr auto default_type_ = "s"sv;
};

constexpr scan_options::scan_options(std::string_view current_pattern)
{
  auto [whole, fill_chr_, align_, grouping_, prec_, type_] =
      pattern_matcher_(current_pattern);
  std::tie(whole_, fill_chr, align, grouping, prec, type) =
      std::tie(whole,
               fill_chr_ != ""sv ? fill_chr_ : default_fill_chr_,
               align_ != ""sv ? align_ : default_align_,
               grouping_ != ""sv ? grouping_ : default_grouping_,
               prec_ != ""sv ? prec_ : default_prec_,
               type_ != ""sv ? type_ : default_type_);
}

template<class Int>
expected_int_t<Int> cast_to_int(std::string_view input, int base = 10)
{
  Int result;
  auto [ptr, err] =
      std::from_chars(input.data(), input.data() + input.size(), result, base);
  if (err == std::errc())  // OK
    return result;
  else {  // KO
    if (err == std::errc::invalid_argument)  // not a number
      return tl::unexpected {invalid_int::NotAnInt};
    else if (err == std::errc::result_out_of_range)
      return tl::unexpected {invalid_int::OutOfRange};
    else
      return tl::unexpected {invalid_int::UnknownError};
  }
}

expected_float_t cast_to_float(std::string_view input)
{
  float result;
  auto [ptr, err] =
      std::from_chars(input.data(), input.data() + input.size(), result);
  if (err == std::errc())  // OK
    return result;
  else {  // KO
    if (err == std::errc::invalid_argument)  // not a number
      return tl::unexpected {invalid_float::NotAFloat};
    else if (err == std::errc::result_out_of_range)
      return tl::unexpected {invalid_float::OutOfRange};
    else
      return tl::unexpected {invalid_float::UnknownError};
  }
}

expected_double_t cast_to_double(std::string_view input)
{
  double result;
  auto [ptr, err] =
      std::from_chars(input.data(), input.data() + input.size(), result);
  if (err == std::errc())  // OK
    return result;
  else {  // KO
    if (err == std::errc::invalid_argument)  // not a number
      return tl::unexpected {invalid_double::NotADouble};
    else if (err == std::errc::result_out_of_range)
      return tl::unexpected {invalid_double::OutOfRange};
    else
      return tl::unexpected {invalid_double::UnknownError};
  }
}

expected_long_double_t cast_to_long_double(std::string_view input)
{
  long double result;
  auto [ptr, err] =
      std::from_chars(input.data(), input.data() + input.size(), result);
  if (err == std::errc())  // OK
    return result;
  else {  // KO
    if (err == std::errc::invalid_argument)  // not a number
      return tl::unexpected {invalid_long_double::NotALongDouble};
    else if (err == std::errc::result_out_of_range)
      return tl::unexpected {invalid_long_double::OutOfRange};
    else
      return tl::unexpected {invalid_long_double::UnknownError};
  }
}

constexpr std::variant<expected_string_t,
                       expected_long_double_t,
                       expected_double_t,
                       expected_float_t,
                       expected_int_t<int>,
                       expected_int_t<long long>,
                       expected_int_t<unsigned>,
                       expected_int_t<long long unsigned>>
construct_scanning_pipeline(std::string_view input, const scan_options& opts)
{
  auto vw = input
      | std::views::filter(
                [group_sep = opts.grouping](char c)
                {
                  if (group_sep != ""sv) {
                    return c != group_sep[0];
                  }
                });

  std::string tmp;
  std::ranges::copy(vw, begin(tmp));
  // default, plain string, ignore precision
  if (opts.type == "s"sv) {
    return tmp;
  } else {  // unsigned, int, float, double, ...
    // different cases:
    // b,d,o,x,lb,ld,lo,lx,ub,ud,uo,ux,lub,lud,luo,lux,f,dlb,ldbl
    if (opts.type == "b") {
      return cast_to_int<int>(tmp, 2);
    } else if (opts.type == "d"sv) {
      return cast_to_int<int>(tmp, 10);
    } else if (opts.type == "o"sv) {
      return cast_to_int<int>(tmp, 8);
    } else if (opts.type == "x"sv) {
      return cast_to_int<int>(tmp, 16);
    } else if (opts.type == "lb"sv) {
      return cast_to_int<long long>(tmp, 2);
    } else if (opts.type == "ld"sv) {
      return cast_to_int<long long>(tmp, 10);
    } else if (opts.type == "lo"sv) {
      return cast_to_int<long long>(tmp, 8);
    } else if (opts.type == "lx"sv) {
      return cast_to_int<long long>(tmp, 16);
    } else if (opts.type == "ub"sv) {
      return cast_to_int<unsigned>(tmp, 2);
    } else if (opts.type == "ud"sv) {
      return cast_to_int<unsigned>(tmp, 10);
    } else if (opts.type == "uo"sv) {
      return cast_to_int<unsigned>(tmp, 8);
    } else if (opts.type == "ux"sv) {
      return cast_to_int<unsigned>(tmp, 16);
    } else if (opts.type == "lub"sv) {
      return cast_to_int<long long unsigned>(tmp, 2);
    } else if (opts.type == "lud"sv) {
      return cast_to_int<long long unsigned>(tmp, 10);
    } else if (opts.type == "luo"sv) {
      return cast_to_int<long long unsigned>(tmp, 8);
    } else if (opts.type == "lux"sv) {
      return cast_to_int<long long unsigned>(tmp, 16);
    } else if (opts.type == "f"sv) {
      return cast_to_float(tmp);
    } else if (opts.type == "dbl"sv) {
      return cast_to_double(tmp);
    } else if (opts.type == "ldbl"sv) {
      return cast_to_long_double(tmp);
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
  return std::make_tuple(
      actual_ret_beg, actual_ret_end, capturing_starting_point, current);
}

expected_parse_result_t parse_input_from_pattern(std::string_view pattern,
                                                 std::string_view input)
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
              parse_error::error_type::nestedBrace,
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
              parse_error::error_type::unexpectedClosingBrace,
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
                parse_error::error_type::openingBraceJuxtaposedToClosingBrace,
                fmt::format(
                    "Unexpected opening '{{' in pattern right next to a closing '}}' at pos {}\n "sv,
                    get_pos(capture_input_until))}};
          } else {
            // we match formatting pattern against the regex
            auto [matched_captured_input_beg,
                  matched_captured_input_end,
                  beg,
                  end] = scan_from_pattern(current_pattern,
                                           *capture_input_until,
                                           current_in,
                                           capturing_input_end);

            std::cout << fmt::format(
                "Pattern: <{}>, Captured <{}>, Parsed <{}>",
                current_pattern,
                std::string_view {matched_captured_input_beg,
                                  matched_captured_input_end},
                std::string_view {beg, end})
                      << std::endl;

            parse_result.push_back(std::string_view {
                matched_captured_input_beg, matched_captured_input_end});
            current_in = end;
          }
        }

      } else if (!capture) {
        if (*current_pat != *current_in) {
          return tl::unexpected {parse_error {
              parse_error::error_type::missmatchPatternAndInput,
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
}

template<typename T>
inline constexpr bool always_false_v = false;

template<class To>
requires std::is_same_v < std::remove_cvref_t<To>,
int > auto cast_to(std::string_view sv)
{
  return cast_to_int<int>(sv);
}

template<class To>
requires std::is_same_v < std::remove_cvref_t<To>,
float > auto cast_to(std::string_view sv)
{
  return cast_to_float(sv);
}

template<class To>
requires std::is_same_v < std::remove_cvref_t<To>,
double > auto cast_to(std::string_view sv)
{
  return cast_to_double(sv);
}

template<class To>
requires std::is_same_v<std::remove_cvref_t<To>, std::string>
auto cast_to(std::string_view sv)
{
  return std::string {begin(sv), end(sv)};
}

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

template<class... ExpectedTypes>
using expected_output_t = std::tuple<select_expected_t<ExpectedTypes>...>;

template<class... ExpectedTypes, typename F, size_t... Is>
auto generate_resulting_tuple_impl(F func, std::index_sequence<Is...>)
{
  using tuple_t = std::tuple<ExpectedTypes...>;
  // FIXME cast_to should receive a pair of arg, one is info about prec, 2nd
  // is actual parsed value
  return std::make_tuple(
      cast_to<decltype(std::get<Is>(std::declval<tuple_t>()))>(func(Is))...);
}

template<size_t N, class... ExpectedTypes, typename F>
auto generate_resulting_tuple(F func)
{
  return generate_resulting_tuple_impl<ExpectedTypes...>(
      func, std::make_index_sequence<N> {});
}

template<class... ExpectedTypes>
expected_output_t<ExpectedTypes...> assemble_resulting_tuple(
    const parse_result_t& parse_result)
{
  constexpr std::size_t nb_matches = sizeof...(ExpectedTypes);

  assert(nb_matches == parse_result.size()
         && "Error! Size of result tuple and parsed result does not match!");

  auto expected_output = generate_resulting_tuple<nb_matches, ExpectedTypes...>(
      [&parse_result](std::size_t idx)
      {
        // FIXME fwd also first arg containing precision info
        return parse_result[idx];  // keep only value of pattern
      });

  return expected_output;
}

template<class... ExpectedTypes>
auto match_pattern(std::string_view pattern, std::string_view input)
{
  using std::ranges::begin;
  using std::ranges::end;

  // Debug
  std::cout << fmt::format("PATTERN: {} \nINPUT: {}\n", pattern, input);

  auto parse_result = parse_input_from_pattern(pattern, input);
  if (parse_result) {
    auto res = *parse_result;

    std::cout << fmt::format("Result size <{}>\n"sv, res.size());

    // Print result
    for (auto&& m : res) {
      std::cout << fmt::format("<{}>\n"sv, m);
    }

    auto ret = assemble_resulting_tuple<ExpectedTypes...>(res);

    std::cout << fmt::format("Expected idx=0 {}\n", *std::get<0>(ret));
    std::cout << fmt::format("Expected idx=1 {}\n", *std::get<1>(ret));
    std::cout << fmt::format("Expected idx=2 {}\n", *std::get<2>(ret));
  } else {
    auto err = parse_result.error();
    std::cerr << fmt::format("Unexpected error: {}", err.what()) << std::endl;
  }

  return true;
}

int main()
{
  constexpr std::string_view input = "14 thus 10.5    gives      1001."sv;
  constexpr std::string_view pattern = "{} thus {:10.5f} gives {>}."sv;

  [[maybe_unused]] auto ret =
      match_pattern<int, float, std::string>(pattern, input);

  return 0;
}
