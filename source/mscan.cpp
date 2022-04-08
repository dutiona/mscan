#include "mscan/mscan.hpp"

#include <ctre-unicode.hpp>
#include <tl/expected.hpp>

#include <optional>
#include <string>
#include <string_view>

struct date
{
  std::string_view year;
  std::string_view month;
  std::string_view day;
};

constexpr std::optional<date> extract_date(std::string_view s) noexcept
{
  using namespace ctre::literals;
  if (auto [whole, year, month, day] =
          ctre::match<"(\\d{4})/(\\d{1,2})/(\\d{1,2})">(s);
      whole)
  {
    return date {year, month, day};
  } else {
    return std::nullopt;
  }
}

using namespace std::literals;

static_assert(extract_date("2018/08/27"sv).has_value());
static_assert((*extract_date("2018/08/27"sv)).year == "2018"sv);
static_assert((*extract_date("2018/08/27"sv)).month == "08"sv);
static_assert((*extract_date("2018/08/27"sv)).day == "27"sv);

exported_class::exported_class()
    : m_name("mscan")
{
}

auto exported_class::name() const -> const char*
{
  return m_name.c_str();
}
