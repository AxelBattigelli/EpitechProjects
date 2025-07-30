add_executable(arcade main.cpp basicMenu.cpp frontend.cpp game.cpp utils.cpp)

target_include_directories(arcade PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/arcade/include)

target_link_libraries(arcade PRIVATE common)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(arcade PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()

