add_library(game_layer OBJECT main.cpp)

target_include_directories(game_layer PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/games/game_layer/include)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(game_layer PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()
