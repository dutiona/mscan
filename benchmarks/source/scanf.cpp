#include <benchmark/benchmark.h>
#include <fmt/format.h>

#include "mscan/mscan.hpp"

#include <cstdio>
#include <iostream>
#include <string_view>
#include <variant>

using namespace std::literals;

constexpr static std::string_view input =
    "14 thus  10.54321666    gives      1001."sv;
constexpr static std::string_view scanner_pattern =
    "{} thus {^.5f} gives {>}."sv;
constexpr static std::string_view scanf_pattern = "%s thus %f gives %s."sv;

static void BM_scanf(benchmark::State& state)
{
  for (auto _ : state) {
    char str1[10], str2[10];
    float flt;
    sscanf(input.data(), str1, &flt, str2);
    std::string str1_(str1), str2_(str2);

    benchmark::DoNotOptimize(str1_);
    benchmark::DoNotOptimize(flt);
    benchmark::DoNotOptimize(str2_);
  }
}

static void BM_scanner(benchmark::State& state)
{
  for (auto _ : state) {
    auto ret =
        mscan::scanner<std::string, float, std::string>(scanner_pattern, input);
    if (ret) {
      auto [str1, flt, str2] = *ret;
      benchmark::DoNotOptimize(str1);
      benchmark::DoNotOptimize(flt);
      benchmark::DoNotOptimize(str2);
    }
  }
}

// Register the function as a benchmark
BENCHMARK(BM_scanf);
BENCHMARK(BM_scanner);
// Run the benchmark
BENCHMARK_MAIN();