if(PROJECT_IS_TOP_LEVEL)
  set(CMAKE_INSTALL_INCLUDEDIR include/mscan CACHE PATH "")
endif()

include(CMakePackageConfigHelpers)
include(GNUInstallDirs)

# find_package(<package>) call for consumers to find this project
set(package mscan)

install(
    DIRECTORY
    include/
    "${PROJECT_BINARY_DIR}/export/"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
    COMPONENT mscan_Development
)

install(
    TARGETS mscan_mscan
    EXPORT mscanTargets
    RUNTIME #
    COMPONENT mscan_Runtime
    LIBRARY #
    COMPONENT mscan_Runtime
    NAMELINK_COMPONENT mscan_Development
    ARCHIVE #
    COMPONENT mscan_Development
    INCLUDES #
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)

write_basic_package_version_file(
    "${package}ConfigVersion.cmake"
    COMPATIBILITY SameMajorVersion
)

# Allow package maintainers to freely override the path for the configs
set(
    mscan_INSTALL_CMAKEDIR "${CMAKE_INSTALL_DATADIR}/${package}"
    CACHE PATH "CMake package config location relative to the install prefix"
)
mark_as_advanced(mscan_INSTALL_CMAKEDIR)

install(
    FILES cmake/install-config.cmake
    DESTINATION "${mscan_INSTALL_CMAKEDIR}"
    RENAME "${package}Config.cmake"
    COMPONENT mscan_Development
)

install(
    FILES "${PROJECT_BINARY_DIR}/${package}ConfigVersion.cmake"
    DESTINATION "${mscan_INSTALL_CMAKEDIR}"
    COMPONENT mscan_Development
)

if(PROJECT_IS_TOP_LEVEL)
  include(CPack)
endif()
