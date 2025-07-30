# Define the project name and version
cmake_minimum_required(VERSION 3.10)
project(Arcade)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_FLAGS "-fPIC -fno-common -g3 -fno-omit-frame-pointer")

if(CMAKE_BUILD_TYPE STREQUAL "Debuga")
  find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
else()
  set(CLANG_TIDY_EXE
      "CLANG_TIDY_EXE-NOTFOUND"
      CACHE INTERNAL "")
endif()

# Set the output directories to the source directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

# Add subdirectories for each part of the project add_subdirectory(src/common)
add_subdirectory(src/arcade)
add_subdirectory(src/graphical_libs)
add_subdirectory(src/games)
add_subdirectory(src/common)
