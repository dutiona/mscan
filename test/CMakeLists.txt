cmake_minimum_required(VERSION 3.14)

project(mscanTests LANGUAGES CXX)

include(../cmake/project-is-top-level.cmake)
include(../cmake/windows-set-path.cmake)

if(PROJECT_IS_TOP_LEVEL)
  find_package(mscan REQUIRED)
  enable_testing()
endif()

add_executable(mscan_test source/mscan_test.cpp)
target_link_libraries(mscan_test PRIVATE mscan::mscan)
target_compile_features(mscan_test PRIVATE cxx_std_20)

add_test(NAME mscan_test COMMAND mscan_test)
windows_set_path(mscan_test mscan::mscan)
