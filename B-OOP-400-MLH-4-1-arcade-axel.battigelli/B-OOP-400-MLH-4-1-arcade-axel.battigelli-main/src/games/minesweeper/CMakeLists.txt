add_library(arcade_minesweeper SHARED main.cpp $<TARGET_OBJECTS:game_layer>)

set_target_properties(arcade_minesweeper PROPERTIES PREFIX "")

target_include_directories(arcade_minesweeper PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/games/minesweeper/include)

target_link_libraries(arcade_minesweeper PRIVATE common)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(arcade_minesweeper PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()
