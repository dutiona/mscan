#include "mscan/mscan.hpp"

#include <catch2/catch_all.hpp>
#include <ctre-unicode.hpp>
#include <tl/expected.hpp>

#include <charconv>
#include <string>
#include <string_view>
#include <system_error>
#include <variant>

unsigned int Factorial(unsigned int number)
{
  return number <= 1 ? number : Factorial(number - 1) * number;
}

TEST_CASE("Factorials are computed", "[factorial]")
{
  REQUIRE(Factorial(1) == 1);
  REQUIRE(Factorial(2) == 2);
  REQUIRE(Factorial(3) == 6);
  REQUIRE(Factorial(10) == 3628800);
}

using namespace std::literals;

enum class invalid_int
{
  NotAnInt,
  OutOfRange,
  UnknownError
};

tl::expected<int, invalid_int> cast_to_int(std::string_view input)
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

TEST_CASE("Match all {} in a string_view", "[braces]")
{
  constexpr std::string_view strv = "14 then AHHH and finally so true."sv;

  constexpr auto matches = ctre::match<
      "([a-zA-Z0-9 _]+) then ([a-zA-Z0-9 _]+) and finally ([a-zA-Z0-9 "
      "_]+)\\.">(strv);
  REQUIRE(matches.count() == 4);
  REQUIRE(matches.get<0>() == strv);
  REQUIRE(matches.get<1>() == "14");
  REQUIRE(matches.get<2>() == "AHHH");
  REQUIRE(matches.get<3>() == "so true");
  auto my_int = cast_to_int(matches.get<1>());
  if (my_int) {
    REQUIRE(*my_int == 14);
  }
}