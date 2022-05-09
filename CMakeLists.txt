cmake_minimum_required(VERSION 3.14)

include(cmake/prelude.cmake)

project(
    mscan
    VERSION 0.1.0
    DESCRIPTION "Modern scan relying of fmt design and std::expected"
    HOMEPAGE_URL "https://github.com/dutiona/mscan"
    LANGUAGES CXX
)

docs_early_return()

include(cmake/project-is-top-level.cmake)
include(cmake/variables.cmake)

# ---- Declare library ----

add_library(
    mscan_mscan
    INTERFACE
)
add_library(mscan::mscan ALIAS mscan_mscan)


if(NOT BUILD_SHARED_LIBS)
  target_compile_definitions(mscan_mscan INTERFACE MSCAN_STATIC_DEFINE)
endif()

set_target_properties(
    mscan_mscan PROPERTIES
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN YES
    VERSION "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}"
    EXPORT_NAME mscan
    OUTPUT_NAME mscan
)

target_include_directories(
    mscan_mscan ${mscan_warning_guard}
    INTERFACE
    "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>"
)

target_include_directories(
    mscan_mscan SYSTEM
    INTERFACE
    "$<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/export>"
)

include(FetchContent)

FetchContent_Declare(
    expected
    GIT_REPOSITORY https://github.com/TartanLlama/expected
    GIT_TAG "v1.0.0"
)

FetchContent_Declare(
    ctre
    GIT_REPOSITORY https://github.com/hanickadot/compile-time-regular-expressions
    GIT_TAG "v3.6"
)

FetchContent_Declare(
    fmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt
    GIT_TAG "8.1.1"
)

FetchContent_MakeAvailable(expected ctre fmt)

target_link_libraries(
  mscan_mscan
  INTERFACE
  expected
  ctre
  fmt
)

target_compile_features(mscan_mscan INTERFACE cxx_std_23)

# ---- Install rules ----

if(NOT CMAKE_SKIP_INSTALL_RULES)
  include(cmake/install-rules.cmake)
endif()

# ---- Developer mode ----

if(NOT mscan_DEVELOPER_MODE)
  return()
elseif(NOT PROJECT_IS_TOP_LEVEL)
  message(
      AUTHOR_WARNING
      "Developer mode is intended for developers of mscan"
  )
endif()

include(cmake/dev-mode.cmake)
