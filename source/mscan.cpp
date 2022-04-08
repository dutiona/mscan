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

struct image
{
};

struct fail_reason
{
  std::string_view reason;
};

tl::expected<image, fail_reason> crop_to_cat(const image& img)
{
  return img;
}

tl::expected<image, fail_reason> add_bow_tie(const image& img)
{
  return img;
}

tl::expected<image, fail_reason> make_eyes_sparkle(const image& img)
{
  return img;
}

image make_smaller(const image& img)
{
  return img;
}

image add_rainbow(const image& img)
{
  return img;
}

tl::expected<image, fail_reason> get_cute_cat(const image& img)
{
  return crop_to_cat(img)
      .and_then(add_bow_tie)
      .and_then(make_eyes_sparkle)
      .map(make_smaller)
      .map(add_rainbow);
}

exported_class::exported_class()
    : m_name("mscan")
{
}

auto exported_class::name() const -> const char*
{
  return m_name.c_str();
}
