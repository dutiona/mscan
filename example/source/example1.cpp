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

using parse_result_t =
    std::vector<std::pair<std::string_view, std::string_view>>;

using expected_parse_result_t = tl::expected<parse_result_t, parse_error>;

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

          // get next  caracter
          auto capture_input_untill = capturing_pattern_end + 1;

          if (capture_input_untill >= end_pat) {  // guard against end of stream
            capturing_input_end = end_in;
          } else {
            // error case, there are two juxtaposed {}{}
            if (*capture_input_untill == '{') {
              return tl::unexpected {parse_error {
                  parse_error::error_type::openingBraceJuxtaposedToClosingBrace,
                  fmt::format(
                      "Unexpected opening '{{' in pattern right next to a closing '}}' at pos {}\n "sv,
                      get_pos(capture_input_untill))}};
            } else {
              do {
                current_in++;
              } while (*current_in != *capture_input_untill);

              capturing_input_end = current_in;
            }
          }

          parse_result.push_back(std::make_pair(
              std::string_view {capturing_pattern_beg, capturing_pattern_end},
              std::string_view {capturing_input_beg, capturing_input_end}));
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

enum class invalid_int
{
  NotAnInt,
  OutOfRange,
  UnknownError
};

using expected_int_t = tl::expected<int, invalid_int>;

expected_int_t cast_to_int(std::string_view input)
{
  int result;
  auto [ptr, err] =
      std::from_chars(input.data(), input.data() + input.size(), result);
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

enum class invalid_float
{
  NotAFloat,
  OutOfRange,
  UnknownError
};

using expected_float_t = tl::expected<float, invalid_float>;

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

enum class invalid_double
{
  NotADouble,
  OutOfRange,
  UnknownError
};

using expected_double_t = tl::expected<double, invalid_double>;

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

enum class invalid_string
{
  NotAString,
  OutOfRange,
  UnknownError
};

using expected_string_t = tl::expected<std::string, invalid_string>;

template<typename T>
inline constexpr bool always_false_v = false;

template<class To>
requires std::is_same_v < std::remove_cvref_t<To>,
int > auto cast_to(std::string_view sv)
{
  return cast_to_int(sv);
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
  using type = expected_int_t;
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
auto gen_tuple_impl(F func, std::index_sequence<Is...>)
{
  using tuple_t = std::tuple<ExpectedTypes...>;
  return std::make_tuple(
      cast_to<decltype(std::get<Is>(std::declval<tuple_t>()))>(func(Is))...);
}

template<size_t N, class... ExpectedTypes, typename F>
auto gen_tuple(F func)
{
  return gen_tuple_impl<ExpectedTypes...>(func, std::make_index_sequence<N> {});
}

template<class... ExpectedTypes>
expected_output_t<ExpectedTypes...> assemble_resulting_tuple(
    const parse_result_t& parse_result)
{
  constexpr std::size_t nb_matches = sizeof...(ExpectedTypes);

  assert(nb_matches == parse_result.size()
         && "Error! Size of result tuple and parsed result does not match!");

  auto expected_output = gen_tuple<nb_matches, ExpectedTypes...>(
      [&parse_result](std::size_t idx) { return parse_result[idx].second; });

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
    for (auto&& [p, m] : res) {
      std::cout << fmt::format("<{}> : <{}>\n"sv, p, m);
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
  constexpr std::string_view input = "14 thus 10.5 gives 1001."sv;
  constexpr std::string_view pattern = "{} thus {:10.5f} gives {}."sv;

  [[maybe_unused]] auto ret =
      match_pattern<int, float, std::string>(pattern, input);

  return 0;
}
