#pragma once

#include "Math/Vector3.hpp"
#include "Math/Ray.hpp"

namespace Raytracer {

    class ILight {
      public:
        virtual ~ILight() = default;

        virtual Color illuminate(const Ray& ray) const = 0;
    };

} // namespace Raytracer
