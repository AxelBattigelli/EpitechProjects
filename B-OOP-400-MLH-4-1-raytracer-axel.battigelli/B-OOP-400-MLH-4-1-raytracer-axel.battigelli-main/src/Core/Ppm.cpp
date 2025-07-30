#include "Core/Ppm.hpp"

#include "Exception.hpp"
#include "Math/Vector3.hpp"

#include <fstream>
#include <iostream>
#include <string>
#include <vector>
inline double l2g(double linear_component)
{
    if (linear_component > 0)
        return std::sqrt(linear_component);

    return 0;
}

void Raytracer::Ppm::writePPM(const std::string &filename, int width, int height,
    const std::vector<Raytracer::Color> &pixels)
{
    std::ofstream out(filename);
    if (!out)
        throw Exception("Could not open file for writing: " + filename);

    out << "P3\n" << width << " " << height << "\n255\n";
    for (int j = 0; j < height; ++j) {
        for (int i = 0; i < width; ++i) {
            const Raytracer::Color &c = pixels[(j * width) + i];

            out << ceil(l2g(c.x) * 255.999) << ' ' << ceil(l2g(c.y) * 255.999) << ' '
                << ceil(l2g(c.z) * 255.999) << ' ';
        }
        out << '\n';
    }
    out.close();
}

void Raytracer::Ppm::createEmptyPPM(const std::string &filename, int width, int height)
{
    std::ofstream file(filename, std::ios::binary);
    if (!file.is_open())
        throw Exception("Error creating the file");

    file << "P6\n" << width << " " << height << "\n255\n";

    const int totalPixels = width * height;
    for (int i = 0; i < totalPixels; ++i) {
        file.put(255); // Red
        file.put(255); // Green
        file.put(255); // Blue
    }

    file.close();
    std::cout << "Empty PPM file created: " << filename << "\n";
}
