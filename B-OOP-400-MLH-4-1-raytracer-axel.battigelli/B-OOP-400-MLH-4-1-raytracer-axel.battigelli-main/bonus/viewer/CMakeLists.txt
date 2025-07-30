cmake_minimum_required(VERSION 3.13)
project(PPMViewer)

set(CMAKE_CXX_STANDARD 23)

find_package(SFML 2.5 COMPONENTS graphics window system REQUIRED)

add_executable(PPMViewer main.cpp)
target_link_libraries(PPMViewer sfml-graphics sfml-window sfml-system)
