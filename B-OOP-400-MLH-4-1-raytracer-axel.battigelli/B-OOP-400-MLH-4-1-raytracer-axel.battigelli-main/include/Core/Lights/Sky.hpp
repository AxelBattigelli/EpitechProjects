#pragma once

#include "Core/ILight.hpp"

namespace Raytracer {

    class SkyLight : public ILight {
      private:
        double _multiplier;

      public:
        SkyLight(double multiplier = 0.5) : _multiplier(multiplier) {}

        Color illuminate(const Ray& ray) const override {
            Vector3 unitDirection = ray.direction.normalize();
            double t = 0.1 * (unitDirection.y + 1.0);
            Color darkBlue(0.0, 0.0, 0.2);
            Color lightBlue(0.5, 0.7, 1.0);
            return _multiplier * ((1.0 - t) * darkBlue + t * lightBlue);
        }
    };

} // namespace Raytracer
