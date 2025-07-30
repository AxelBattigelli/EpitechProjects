#pragma once

#include "Core/Camera.hpp"
#include "Core/IPrimitive.hpp"
#include "Math/Vector3.hpp"

#include <memory>
#include <vector>

namespace Raytracer
{
    class Ppm {
      public:
        static void writePPM(const std::string &filename, int width, int height,
            const std::vector<Raytracer::Color> &pixels);
        static void createEmptyPPM(const std::string &filename, int width, int height);
    };
} // namespace Raytracer
