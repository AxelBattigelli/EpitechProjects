# Define the project name and version
cmake_minimum_required(VERSION 3.13)
project(Raytracer)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_FLAGS "-g3")

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
else()
  set(CLANG_TIDY_EXE
      "CLANG_TIDY_EXE-NOTFOUND"
      CACHE INTERNAL "")
endif()

# Set the output directories to the source directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

add_executable(raytracer)

target_include_directories(raytracer PRIVATE ${CMAKE_SOURCE_DIR}/include)

install(TARGETS raytracer DESTINATION bin)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(raytracer PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()

add_subdirectory(src)
