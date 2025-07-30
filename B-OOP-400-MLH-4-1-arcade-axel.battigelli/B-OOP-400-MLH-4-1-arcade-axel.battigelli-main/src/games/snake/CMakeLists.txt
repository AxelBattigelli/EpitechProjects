add_library(arcade_snake SHARED main.cpp $<TARGET_OBJECTS:game_layer>)

set_target_properties(arcade_snake PROPERTIES PREFIX "")

target_include_directories(arcade_snake PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/games/snake/include)

target_link_libraries(arcade_snake PRIVATE common)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(arcade_snake PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()
