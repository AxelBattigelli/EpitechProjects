add_library(arcade_logger SHARED main.cpp $<TARGET_OBJECTS:graphic_layer>)

set_target_properties(arcade_logger PROPERTIES PREFIX "")

target_include_directories(arcade_logger PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/graphical_libs/logger/include)

                         target_link_libraries(arcade_logger PRIVATE common)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(arcade_logger PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()
