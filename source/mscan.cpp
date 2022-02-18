#include <string>

#include "mscan/mscan.hpp"

exported_class::exported_class()
    : m_name("mscan")
{
}

auto exported_class::name() const -> const char*
{
  return m_name.c_str();
}
