# Adapted from the Arch Linux PKGBUILD for alglib
cmake_minimum_required(VERSION 3.0)
project(alglib VERSION @version@)

file(GLOB SRCS src/*.cpp)
file(GLOB HEADERS src/*.h)

add_library(alglib SHARED ${SRCS})
set_target_properties(alglib PROPERTIES
    VERSION "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}"
    PUBLIC_HEADER "${HEADERS}"
)

install(TARGETS alglib
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
install(FILES manual.cpp.html DESTINATION "${CMAKE_INSTALL_DOCDIR}")
