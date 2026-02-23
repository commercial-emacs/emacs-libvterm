cmake_minimum_required(VERSION 3.11)
include(ExternalProject)

project(emacs-libvterm C)

if(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD" OR CMAKE_SYSTEM_NAME STREQUAL "OpenBSD" OR CMAKE_SYSTEM_NAME STREQUAL "NetBSD")
   set(LIBVTERM_BUILD_COMMAND "gmake")
else()
   set(LIBVTERM_BUILD_COMMAND "make")
endif()

add_library(vterm-module MODULE vterm-module.c utf8.c elisp.c)
set_target_properties(vterm-module PROPERTIES
  C_STANDARD 99
  COMPILE_WARNING_AS_ERROR ON
  C_VISIBILITY_PRESET "hidden"
  POSITION_INDEPENDENT_CODE ON
  PREFIX ""
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}
  )

# Set RelWithDebInfo as default build type
if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "No build type selected, defaulting to RelWithDebInfo")
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Build type (default RelWithDebInfo)" FORCE)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  find_program(LIBTOOL NAMES glibtool)
  if(NOT LIBTOOL)
    message(FATAL_ERROR "glibtool not found. Please install glibtool")
  endif()
else()
  find_program(LIBTOOL NAMES libtool)
  if(NOT LIBTOOL)
    message(FATAL_ERROR "libtool not found. Please install libtool")
  endif()
endif()

ExternalProject_add(libvterm
  GIT_REPOSITORY https://github.com/dickmao/libvterm-mirror.git
  GIT_TAG 1364726
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${LIBVTERM_BUILD_COMMAND} "CFLAGS=-fPIC" "LDFLAGS=-static" "LIBTOOL=${LIBTOOL}"
  BUILD_IN_SOURCE ON
  INSTALL_COMMAND "")

ExternalProject_Get_property(libvterm SOURCE_DIR)

set(LIBVTERM_INCLUDE_DIR ${SOURCE_DIR}/include)
set(LIBVTERM_LIBRARY ${SOURCE_DIR}/.libs/libvterm.a)

add_dependencies(vterm-module libvterm)

# Workaround for https://gitlab.kitware.com/cmake/cmake/issues/15052
file(MAKE_DIRECTORY ${LIBVTERM_INCLUDE_DIR})

add_library(vterm STATIC IMPORTED)
set_target_properties(vterm PROPERTIES IMPORTED_LOCATION ${LIBVTERM_LIBRARY})
target_include_directories(vterm INTERFACE ${LIBVTERM_INCLUDE_DIR})

# Link with libvterm
target_link_libraries(vterm-module PUBLIC vterm)

# Add gprof profiling support
# target_compile_options(vterm-module PRIVATE -pg)
# target_link_options(vterm-module PRIVATE -pg)

# Custom run command for testing
add_custom_target(run
  COMMAND emacs -Q -L ${CMAKE_SOURCE_DIR} -L ${CMAKE_BINARY_DIR} --eval "\\(require \\'vterm\\)" --eval "\\(vterm\\)"
  DEPENDS vterm-module
  )
