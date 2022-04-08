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
    source/mscan.cpp
)
add_library(mscan::mscan ALIAS mscan_mscan)

include(GenerateExportHeader)
generate_export_header(
    mscan_mscan
    BASE_NAME mscan
    EXPORT_FILE_NAME export/mscan/mscan_export.hpp
    CUSTOM_CONTENT_FROM_VARIABLE pragma_suppress_c4251
)

if(NOT BUILD_SHARED_LIBS)
  target_compile_definitions(mscan_mscan PUBLIC MSCAN_STATIC_DEFINE)
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
    PUBLIC
    "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>"
)

target_include_directories(
    mscan_mscan SYSTEM
    PUBLIC
    "$<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/export>"
)

target_compile_features(mscan_mscan PUBLIC cxx_std_23)

# -- External dependencies

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

FetchContent_MakeAvailable(expected ctre)

target_link_libraries(
  mscan_mscan
  PRIVATE
  expected ctre
)

#target_link_libraries(
#  mscan_mscan
#  INTERFACE
#  expected ctre
#)

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
