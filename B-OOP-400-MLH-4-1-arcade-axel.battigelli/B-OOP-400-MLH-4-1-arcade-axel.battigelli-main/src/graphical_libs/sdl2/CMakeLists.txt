add_library(arcade_sdl2 SHARED main.cpp $<TARGET_OBJECTS:graphic_layer>)

set_target_properties(arcade_sdl2 PROPERTIES PREFIX "")
set_target_properties(arcade_sdl2 PROPERTIES CXX_FLAGS "")

target_include_directories(arcade_sdl2 PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/graphical_libs/sdl2/include)

find_package(SDL2 REQUIRED)
find_package(SDL2_image REQUIRED)
find_package(SDL2_ttf REQUIRED)
target_link_libraries(arcade_sdl2 PRIVATE SDL2::SDL2 SDL2_image::SDL2_image SDL2_ttf::SDL2_ttf common)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(arcade_sdl2 PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()
