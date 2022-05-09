cmake_minimum_required(VERSION 3.14)

project(mscanBenchmarks LANGUAGES CXX)

include(../cmake/project-is-top-level.cmake)
include(../cmake/windows-set-path.cmake)

if(PROJECT_IS_TOP_LEVEL)
  find_package(mscan REQUIRED)
  enable_testing()
endif()

add_executable(mscan_benchmarks source/scanf.cpp)

FetchContent_Declare(
    fmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt
    GIT_TAG "8.1.1"
)

FetchContent_Declare(googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG release-1.11.0)

FetchContent_Declare(googlebenchmark
        GIT_REPOSITORY https://github.com/google/benchmark.git
        GIT_TAG master) # need master for benchmark::benchmark

FetchContent_MakeAvailable(fmt googletest googlebenchmark)

target_link_libraries(
  mscan_benchmarks
  PRIVATE
  mscan::mscan
  fmt
  benchmark::benchmark
)

target_compile_features(mscan_benchmarks PRIVATE cxx_std_20)
target_compile_options(mscan_benchmarks PRIVATE "-g")
# target_link_options(mscan_benchmarks PRIVATE "-lprofiler")

windows_set_path(mscan_benchmarks mscan::mscan)
