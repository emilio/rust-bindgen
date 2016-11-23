cmake_minimum_required(VERSION 2.8.5)
project(Bindgen CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS "ON")

if(NOT CMAKE_CONFIGURATION_TYPES)
  if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Whether to build in `Debug` or `Release` mode." FORCE)
  endif()
endif()

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)
set(WILL_RUN_TESTS ON CACHE BOOL "Run tests")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -pthread -g3 -Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "-O0 -Werror -DDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -DNDEBUG")

add_library(bindgen OBJECT
  bindgen/BindgenAnnotations.cc
  bindgen/BindgenContext.cc
  bindgen/BindgenItem.cc
  bindgen/BindgenType.cc
  bindgen/RustBindgen.cc
)

include(ProcessorCount)
ProcessorCount(N)
if(NOT N EQUAL 0)
  set(JFLAG -j${N})
endif()

add_custom_target(format COMMAND
  find "${CMAKE_SOURCE_DIR}/bindgen"
    -regex "'.*\\.\\(cc\\|h\\)'"
    -exec clang-format -i {} "\;"
)
