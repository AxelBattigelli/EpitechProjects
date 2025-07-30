add_library(arcade_ncurses SHARED main.cpp $<TARGET_OBJECTS:graphic_layer>)

set(CURSES_NEED_NCURSES TRUE)
set(CURSES_NEED_WIDE TRUE)

find_package(Curses REQUIRED)
include_directories(${CURSES_INCLUDE_DIR})

target_link_libraries(arcade_ncurses PRIVATE ${CURSES_LIBRARIES} common)

set_target_properties(arcade_ncurses PROPERTIES PREFIX "")
set_target_properties(arcade_ncurses PROPERTIES CXX_FLAGS CURSES_CFLAGS)

target_include_directories(arcade_ncurses PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/graphical_libs/ncurses/include)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(arcade_ncurses PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()
