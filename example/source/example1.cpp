#include <ctre-unicode.hpp>
#include <fmt/format.h>
#include <tl/expected.hpp>

#include "mscan/mscan.hpp"

#include <algorithm>
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
auto match_pattern(std::string_view pattern, std::string_view input)
{
  constexpr std::size_t nb_matches = sizeof...(ExpectedTypes);
  using output_t = std::tuple<select_expected_t<ExpectedTypes>...>;

  using std::ranges::begin;
  using std::ranges::end;

  // Debug
  std::cout << fmt::format("pattern: {} ; input: {}\n", pattern, input);

  using parse_result_t =
      std::vector<std::pair<std::string_view, std::string_view>>;

  auto parse_result = parse_result_t {};

  {
    auto beg_pat = begin(pattern);
    auto end_pat = end(pattern);

    auto beg_in = begin(input);
    auto end_in = end(input);

    bool capture = false;

    auto capturing_pattern_beg = beg_pat;
    auto capturing_pattern_end = beg_pat;
    auto capturing_input_beg = beg_in;
    auto capturing_input_end = beg_in;

    while (beg_pat != end_pat) {
      if (*beg_pat == '{') {  // start capturing
        if (capture) {
          std::cerr << fmt::format(
              "Unexpected nested '{{' in pattern at pos {}\n"sv,
              static_cast<std::size_t>(beg_pat - begin(pattern)));
        } else {
          capture = true;
          capturing_pattern_beg = beg_pat + 1;
          capturing_input_beg = beg_in;
        }
      }

      else if (*beg_pat == '}')  // end capturing
      {
        if (!capture) {
          std::cerr << fmt::format(
              "Unexpected closing '}}' in pattern at pos {}\n"sv,
              static_cast<std::size_t>(beg_pat - begin(pattern)));
        } else {
          capture = false;
          capturing_pattern_end = beg_pat;

          auto capture_input_untill = beg_pat + 1;
          if (capture_input_untill >= end_pat)  // Capture till the end
          {
            capturing_input_end = end_in;
            // the closing} and next { are adjacent : error
          } else if (*capture_input_untill == '{') {
            std::cerr << fmt::format(
                "Unexpected opening '{{' in pattern right next to a closing '}}' at pos {}\n"sv,
                static_cast<std::size_t>(capture_input_untill
                                         - begin(pattern)));
          } else {
            std::cout << fmt::format(
                "capturing untill <{}>\n",
                static_cast<std::size_t>(capture_input_untill
                                         - begin(pattern)));
            do {
              ++beg_in;
            } while (*beg_in != *capture_input_untill);

            capturing_input_end = beg_in;
          }

          std::cout << fmt::format(
              "Pushing back <{}> <{}>\n",
              std::string_view {capturing_pattern_beg, capturing_pattern_end},
              std::string_view {capturing_input_beg, capturing_input_end});

          parse_result.push_back(std::make_pair(
              std::string_view {capturing_pattern_beg, capturing_pattern_end},
              std::string_view {capturing_input_beg, capturing_input_end}));
        }
      }

      if (!capture && *(beg_pat + 1) != *beg_in) {
        std::cerr << fmt::format(
            "Missmatch input '{}' and pattern '{}' at pos {}\n"sv,
            *(beg_in + 1),
            *beg_pat,
            static_cast<std::size_t>((beg_pat + 1) - begin(pattern)));
        return false;
      }

      ++beg_pat;  // continue pattern parsing

      if (!capture) {
        ++beg_in;
      }
    }
  }

  // Print result
  for (auto&& [p, m] : parse_result) {
    std::cout << fmt::format("<{}> : <{}>"sv, p, m);
  }

  return true;

  // group result is:
  // 0: value before first } until first match
  // 1: value inside {...}
  // constexpr auto splitter = ctre::split<"\\{(.*?)\\}">;
  // auto matches = splitter(pattern);
  //
  //// testing purpose, only handle empty {}
  // for (auto [fixed_str, inside_braces] : matches) {
  //   std::cout << fmt::format("Matches <{}> <{}>\n", fixed_str,
  //   inside_braces);
  //
  //  if (fixed_str.size() == 0) {
  //    auto found = std::ranges::search(
  //        begin(input), end(input), begin(fixed_str), end(fixed_str));
  //    if (found.empty())  // not found, input does not match
  //    // fixed string of pattern
  //    {
  //      std::cout << fmt::format("Not found!\n");
  //    } else {
  //      std::cout << fmt::format("Found! <{}>\n", found);
  //
  //      // subrange
  //      const auto subr_first = std::distance(begin(input), begin(found));
  //      const auto subr_last = std::distance(begin(input), end(found));
  //      // part to match against pattern inside {...}
  //      const auto first = begin(input);
  //      const auto last = begin(input) + subr_first;
  //
  //      std::cout << fmt::format("Splitter: <{}> | <{}> | <{}>\n",
  //                               std::string_view(begin(input), end(input)),
  //                               std::string_view(begin(input) + subr_first,
  //                                                begin(input) + subr_last),
  //                               std::string_view(first, last));
  //    }
  //  }
  //}
  // return false;
}

int main()
{
  constexpr std::string_view input = "14 thus 10.5 gives 1001."sv;
  constexpr std::string_view pattern = "{} thus {:10.5f} gives {}"sv;

  [[maybe_unused]] auto ret =
      match_pattern<int, float, std::string>(pattern, input);

  return 0;
}
