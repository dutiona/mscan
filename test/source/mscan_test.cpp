#include <string>

#include "mscan/mscan.hpp"

auto main() -> int
{
  exported_class e;

  return std::string("mscan") == e.name() ? 0 : 1;
}
