add_library(common STATIC common.cpp)

target_include_directories(common PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/common/include)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(common PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()

