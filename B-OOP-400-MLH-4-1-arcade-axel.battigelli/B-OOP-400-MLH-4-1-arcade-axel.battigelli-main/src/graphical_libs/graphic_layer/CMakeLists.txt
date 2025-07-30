add_library(graphic_layer OBJECT main.cpp)

target_include_directories(graphic_layer PRIVATE ${CMAKE_SOURCE_DIR}/include
                           ${CMAKE_SOURCE_DIR}/src/graphical_libs/graphic_layer/include)

target_link_libraries(graphic_layer PRIVATE common)

if(NOT ${CLANG_TIDY_EXE} STREQUAL "CLANG_TIDY_EXE-NOTFOUND")
  set_target_properties(graphic_layer PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()
