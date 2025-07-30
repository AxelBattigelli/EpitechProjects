#include "Core/Parser.hpp"
#include "Core/Ppm.hpp"
#include "Exception.hpp"
#include "Primitives/List.hpp"
#include "Primitives/OBJFile.hpp"

#include <Materials/FlatColor.hpp>
#include <Materials/Transparency.hpp>
#include <Materials/Mirror.hpp>
#include <Primitives/Boxe.hpp>
#include <Primitives/Torus.hpp>
#include <Primitives/Cone.hpp>
#include <Primitives/Cylinder.hpp>
#include <Primitives/Pyramide.hpp>

#include <atomic>
#include <cstring>
#include <fcntl.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <unistd.h>

auto toByte(double value) -> unsigned char
{
    return static_cast<unsigned char>(std::max(0.0, std::min(1.0, value)) * 255.0);
}
inline double l2g(double linear_component)
{
    if (linear_component > 0)
        return std::sqrt(linear_component);

    return 0;
}
void echoPixel(std::vector<Raytracer::Color> image, int samples, int part, int stride)
{
    static thread_local bool initialized = false;
    static thread_local int fd = -1;
    static thread_local size_t pixelDataOffset = 0;
    static thread_local int imgWidth = 0;
    static thread_local int imgHeight = 0;
    const std::string filename = "preview.ppm";
    static std::atomic<int> progress = 0;

    if (!initialized) {
        fd = open(filename.c_str(), O_RDWR);
        if (fd == -1) {
            perror("Failed to open file");
            return;
        }

        std::ifstream file(filename, std::ios::binary);
        if (!file) {
            std::cerr << "Failed to open file for header parsing\n";
            close(fd);
            fd = -1;
            return;
        }

        std::string line;
        std::string magic;
        int maxVal = 0;

        std::getline(file, magic);
        if (magic != "P6") {
            std::cerr << "Not a valid P6 PPM file\n";
            close(fd);
            fd = -1;
            return;
        }
        while (std::getline(file, line)) {
            if (line[0] == '#')
                continue;
            std::istringstream dims(line);
            dims >> imgWidth >> imgHeight;
            break;
        }
        while (std::getline(file, line)) {
            if (line[0] == '#')
                continue;
            std::istringstream maxval(line);
            maxval >> maxVal;
            break;
        }

        pixelDataOffset = static_cast<size_t>(file.tellg());
        initialized = true;
    }

    if (fd == -1)
        return;

    for (int y = part; y < imgHeight; y += stride) {
        size_t pixelIndex = (y * imgWidth) * 3;
        const size_t offset = pixelDataOffset + pixelIndex;
        std::vector<uint8_t> buffer(imgWidth * 3);
        for (int x = 0; x < imgWidth; ++x) {
            auto color = (image[y * imgWidth + x] / samples);
            if (color.sqr_length() > 1)
                color = color.normalize();
            buffer[x * 3 + 0] = toByte(l2g(color.x));
            buffer[x * 3 + 1] = toByte(l2g(color.y));
            buffer[x * 3 + 2] = toByte(l2g(color.z));
        }
        if (pwrite(fd, buffer.data(), buffer.size(), offset) != buffer.size()) {
            perror("pwrite failed");
        }
    }

    int current = progress.load();
    while (true) {
        if (samples <= current)
            break;
        if (progress.compare_exchange_weak(current, samples))
            break;
        // current is automatically updated with the latest value on failure
    }

    if (part == 0) {
        thread_local std::ostringstream oss;
        oss.str("");
        oss.clear();
        oss << "Progress: " << progress << "\r";
        std::cout << oss.str() << std::flush;
    }

    /*fsync(fd); // Ensure changes are flushed and visible*/
}

auto main(int argc, char *argv[]) -> int
{
    for (int i = 0; i < argc; ++i) {
        if (std::strcmp(argv[i], "-h") == 0 || std::strcmp(argv[i], "--help") == 0) {
            std::cout << "USAGE:\n"
                      << "    ./raytracer <SCENE_FILE>\n"
                      << "SCENE_FILE:\n"
                      << "    scene configuration\n";
            return 0;
        }
    }

    try {
        if (argc != 2)
            throw Raytracer::Exception("Bad usage");

        std::default_random_engine rng{};

        Raytracer::Parser parser;
        parser.readFile(argv[1]);

        Raytracer::Camera cam = parser.getCam();
        auto world = parser.getWorld();
        std::cout << "World contains " << world.log() << " object(s):\n";
        world.buildBVH();

        Raytracer::Ppm::createEmptyPPM("preview.ppm", cam.width, cam.height);
        const std::vector<Raytracer::Color> pixels = cam.render(rng, world, echoPixel);
        Raytracer::Ppm::writePPM("output.ppm", cam.width, cam.height, pixels);
    } catch (const Raytracer::Exception &e) {
        std::cerr << "Error: " << e << '\n';
        return 84;
    } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what()
                  << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
        return 84;
    } catch (...) {
        std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                  << std::current_exception().__cxa_exception_type()->name() << '\n';
        return 84;
    }
}
