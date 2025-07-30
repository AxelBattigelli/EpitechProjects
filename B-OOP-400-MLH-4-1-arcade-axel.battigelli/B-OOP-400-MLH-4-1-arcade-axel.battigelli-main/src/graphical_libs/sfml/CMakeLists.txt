add_library(arcade_sfml SHARED main.cpp $<TARGET_OBJECTS:graphic_layer>)

set_target_properties(arcade_sfml PROPERTIES PREFIX "")
set_target_properties(arcade_sfml PROPERTIES CXX_FLAGS "")

target_include_directories(arcade_sfml PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/graphical_libs/sfml/include)

target_link_libraries(arcade_sfml PRIVATE sfml-graphics common)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(arcade_sfml PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()
