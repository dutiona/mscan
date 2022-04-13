#include "mscan/mscan.hpp"

#include <catch2/catch_all.hpp>
#include <ctre-unicode.hpp>
#include <tl/expected.hpp>

#include <string>
#include <string_view>

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

TEST_CASE("Match all {} in a string_view", "[braces]")
{
  constexpr std::string_view strv = "14 then AHHH and finally so true."sv;

  constexpr auto matches = ctre::match<
      "([a-zA-Z0-9 _]+) then ([a-zA-Z0-9 _]+) and finally ([a-zA-Z0-9 _]+)\\.">(
      strv);
  REQUIRE(matches.count() == 4);
  REQUIRE(matches.get<0>() == strv);
  REQUIRE(matches.get<1>() == "14");
  REQUIRE(matches.get<2>() == "AHHH");
  REQUIRE(matches.get<3>() == "so true");
}