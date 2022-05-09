#pragma once

#include <tl/expected.hpp>

#include <charconv>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <variant>

namespace mscan::detail
{

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

template<class Ret = long long>
constexpr Ret pow10(int n)
{
  long long result = 1;
  for (int i = 1; i <= n; ++i)
    result *= 10;
  return static_cast<Ret>(result);
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

template<typename T>
inline constexpr bool always_false_v = false;

template<class T>
struct select_expected
{
  static_assert(always_false_v<T>,
                "Supported scanned types are only <int>, <float>, <double>, "
                "<long double> and <std::string>.");
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
struct select_expected<long double>
{
  using type = expected_long_double_t;
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

}  // namespace mscan::detail
