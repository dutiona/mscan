#include <fmt/format.h>

#include "mscan/mscan.hpp"

#include <iostream>
#include <string_view>
#include <variant>

using namespace std::literals;

template<class... ExpectedTypes>
auto match_pattern(std::string_view pattern, std::string_view input)
{
  // Debug
  std::cout << fmt::format("PATTERN: {} \nINPUT: {}\n", pattern, input);

  auto ret = mscan::scanner<ExpectedTypes...>(pattern, input);
  if (ret) {
    auto [r0, r1, r2] = *ret;
    std::cout << (r0 ? fmt::format("Expected idx=0 {}\n", *r0)
                     : fmt::format("idx=0 error\n"));
    std::cout << (r1 ? fmt::format("Expected idx=1 {}\n", *r1)
                     : fmt::format("idx=1 error\n"));
    std::cout << (r2 ? fmt::format("Expected idx=2 {}\n", *r2)
                     : fmt::format("idx=2 error\n"));
  }

  return true;
}

int main()
{
  constexpr std::string_view input =
      "14 thus  10.54321666    gives      1001."sv;
  constexpr std::string_view pattern = "{} thus {^.5f} gives {>}."sv;

  [[maybe_unused]] auto ret =
      match_pattern<std::string, long double, std::string>(pattern, input);

  return 0;
}
