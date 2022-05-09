cmake_minimum_required(VERSION 3.14)

project(mscanExamples LANGUAGES CXX)

include(../cmake/project-is-top-level.cmake)
include(../cmake/windows-set-path.cmake)

if(PROJECT_IS_TOP_LEVEL)
  find_package(mscan REQUIRED)
  enable_testing()
endif()

add_executable(mscan_example source/example1.cpp)

FetchContent_Declare(
    fmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt
    GIT_TAG "8.1.1"
)

FetchContent_MakeAvailable(fmt)

target_link_libraries(
  mscan_example
  PRIVATE
  mscan::mscan
  fmt
)

target_compile_features(mscan_example PRIVATE cxx_std_20)

windows_set_path(mscan_example mscan::mscan)
