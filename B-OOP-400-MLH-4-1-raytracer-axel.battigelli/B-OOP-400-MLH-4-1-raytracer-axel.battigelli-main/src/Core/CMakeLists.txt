target_sources(raytracer PRIVATE Camera.cpp Ppm.cpp Parser.cpp)

find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBCONFIG REQUIRED libconfig++)

target_include_directories(raytracer PRIVATE ${LIBCONFIG_INCLUDE_DIRS})
target_link_libraries(raytracer PRIVATE ${LIBCONFIG_LIBRARIES})
