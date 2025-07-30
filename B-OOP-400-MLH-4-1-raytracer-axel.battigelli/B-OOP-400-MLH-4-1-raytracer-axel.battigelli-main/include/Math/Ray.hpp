#pragma once
#include "Vector3.hpp"

namespace Raytracer
{
    class Ray {
      public:
        Point3 origin;
        Vector3 direction;

        constexpr Ray(const Point3 &origin, const Vector3 &direction)
            : origin(origin), direction(direction) {};

        [[nodiscard]] constexpr auto at(double t) const -> Point3
        {
            return origin + t * direction;
        }
    };
} // namespace Raytracer
