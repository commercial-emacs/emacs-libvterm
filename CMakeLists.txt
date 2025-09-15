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

# try to find the vterm.h header file.
find_path(LIBVTERM_INCLUDE_DIR NAMES
  vterm.h
  )

if(NOT LIBVTERM_INCLUDE_DIR)
  message(FATAL_ERROR "libvterm header not found")
endif()

find_library(LIBVTERM_LIBRARY NAMES
  vterm
  libvterm
  )

if(NOT LIBVTERM_LIBRARY)
  message(FATAL_ERROR "libvterm not found")
endif()

message(STATUS "System libvterm detected")
execute_process(COMMAND grep -c "VTermStringFragment" "${LIBVTERM_INCLUDE_DIR}/vterm.h" OUTPUT_VARIABLE VTermStringFragmentExists)
if (${VTermStringFragmentExists} EQUAL "0")
  add_compile_definitions(VTermStringFragmentNotExists)
  add_definitions(-DVTermStringFragmentNotExists)
endif()
execute_process(COMMAND grep -c "VTermSelectionMask" "${LIBVTERM_INCLUDE_DIR}/vterm.h" OUTPUT_VARIABLE VTermSelectionMaskExists)
if (${VTermSelectionMaskExists} EQUAL "0")
  add_compile_definitions(VTermStringFragmentNotExists)
  add_definitions(-DVTermSelectionMaskNotExists)
endif()
execute_process(COMMAND grep -c "sb_clear" "${LIBVTERM_INCLUDE_DIR}/vterm.h" OUTPUT_VARIABLE VTermSBClearExists)
if (${VTermSBClearExists} EQUAL "0")
  add_definitions(-DVTermSBClearNotExists)
endif()

add_library(vterm STATIC IMPORTED)
set_target_properties(vterm PROPERTIES IMPORTED_LOCATION ${LIBVTERM_LIBRARY})
target_include_directories(vterm INTERFACE ${LIBVTERM_INCLUDE_DIR})

# Link with libvterm
target_link_libraries(vterm-module PUBLIC vterm)

# Custom run command for testing
add_custom_target(run
  COMMAND emacs -Q -L ${CMAKE_SOURCE_DIR} -L ${CMAKE_BINARY_DIR} --eval "\\(require \\'vterm\\)" --eval "\\(vterm\\)"
  DEPENDS vterm-module
  )
